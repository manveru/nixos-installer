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
LIB = "(import <nixpkgs> {}).lib"

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

# adapted from nixos-manual generation
def deep_attr_names(path = 'options')
  nix(<<~NIX)
    let
      lib = #{LIB};
      options = #{NIXOS}.options;

      # Remove invisible and internal options.
      optionsList = lib.filter (opt: opt.visible && !opt.internal) (lib.optionAttrSetToDocList options);

      # Replace functions by the string <function>
      substFunction = x:
        if builtins.isAttrs x then lib.mapAttrs (name: substFunction) x
        else if builtins.isList x then map substFunction x
        else if builtins.isFunction x then "<function>"
        else x;

      # Clean up declaration sites to not refer to the NixOS source tree.
      optionsList' = lib.flip map optionsList (opt: opt // {
        declarations = map stripAnyPrefixes opt.declarations;
      }
      // lib.optionalAttrs (opt ? example) { example = substFunction opt.example; }
      // lib.optionalAttrs (opt ? default) { default = substFunction opt.default; }
      // lib.optionalAttrs (opt ? type) { type = substFunction opt.type; });

      # We need to strip references to /nix/store/* from options,
      # including any `extraSources` if some modules came from elsewhere,
      # or else the build will fail.
      #
      # E.g. if some `options` came from modules in ${pkgs.customModules}/nix,
      # you'd need to include `extraSources = [ pkgs.customModules ]`
      prefixesToStrip = map (p: "${toString p}/") ([ ../../.. ] ++ extraSources);
      stripAnyPrefixes = lib.flip (lib.fold lib.removePrefix) prefixesToStrip;
    in
      listToAttrs (map (o: {
        name = o.name;
        value = removeAttrs o ["name" "visible" "internal" "declarations"];
      }) optionsList')
  NIX
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
  # p names
  # this is most likely a setting
  if (names & OPTION_VALUE_KEYS).size > (OPTION_VALUE_KEYS.size - 4)
    # [:option, key, attr_option(key)]
  elsif key =~ /\.type$/
    # [:type, key, nix("#{NIXOS}.#{key}.name")]
  else # just a path
    [:path, key, names.map{|n| options("#{key}.#{n}") }]
  end
end

pp deep_attr_names["services.xserver.windowManager.i3.configFile"]
# pp options
