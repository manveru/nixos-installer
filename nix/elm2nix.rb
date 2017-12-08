#!/usr/bin/env ruby

require 'json'

system("elm-install")
depsSrc = JSON.parse(File.read("elm-stuff/exact-dependencies.json"))
deps = Hash[ depsSrc.map { |pkg, ver|
  url = "https://github.com/#{pkg}/archive/#{ver}.tar.gz"
  sha256 = `nix-prefetch-url #{url}`

  [ pkg, { version: ver,
           sha256: sha256.strip
         }
  ]
} ]

File.open("../nix/ui-package.nix", 'w') do |file|
  file.puts "{"
    for pkg, info in deps
      file.puts "  \"#{pkg}\" = {"
      file.puts "    version = \"#{info[:version]}\";"
      file.puts "    sha256 = \"#{info[:sha256]}\";"
      file.puts "  };"
    end
  file.puts "}"
end
