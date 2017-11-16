require 'open3'
require 'pp'
require 'json'

OPTION_VALUE_KEYS = %w[
 declarations
 default
 definitions
 description
 example
 files
 internal
 isDefined
 loc
 options
 type
 value
]

NIXOS = "(import <nixpkgs/nixos> {})"

def nix(cmd)
  out =
    Open3.popen2e('nix-instantiate', '--json', '--strict', '--eval', '-E', <<~NIX) do |_, se, _|
      with builtins;
      #{cmd}
    NIX
      se.read
    end
  JSON.parse(out)
rescue JSON::ParserError => ex
  p ex
  []
end

def attr_names(path)
  nix("attrNames #{NIXOS}.#{path}").reject do |name|
    name =~ /^_/
  end
end

def attr_option(path)
  nix(<<~NIX)
    let
      o = #{NIXOS}.#{path};
    in
      {
        inherit (o)
          default
          definitions
          description
          files
          example
          value;
      }
  NIX
end

def options(key = 'options')
  p key
  names = attr_names(key)
  p names
  # this is most likely a setting
  if (names & OPTION_VALUE_KEYS).size > (OPTION_VALUE_KEYS.size - 4)
    [:option, key, attr_option(key)]
  elsif key =~ /\.type$/
    [:type, key, nix("#{NIXOS}.#{key}.name")]
  else # just a path
    [:path, key, names.map{|n| options("#{key}.#{n}") }]
  end
end

pp options
