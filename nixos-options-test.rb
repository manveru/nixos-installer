require 'json'
require 'pp'

def nixos_options(query)
  `nix-instantiate --show-trace --json --eval ./nixos-options.nix --strict --argstr query #{query}`
end

$invalid = []

def test(query)
  value = JSON.parse(nixos_options(query))
  case value
  when Hash
  when Array
    value.each{|v| test("#{query}.#{v}") }
  end
rescue JSON::ParserError
  p query
  $invalid << query
end

test 'options'

pp $invalid

__END__
["options.assertions",
 "options.boot.crashDump.kernelPackages",
 "options.boot.kernelPackages",
 "options.environment.etc",
 "options.networking.nftables.rulesetFile",
 "options.services.gammu-smsd.backend.sql.database",
 "options.services.hoogle.haskellPackages",
 "options.services.httpd.configFile",
 "options.services.icecast.hostname",
 "options.services.nagios.cgiConfigFile",
 "options.services.nagios.mainConfigFile",
 "options.services.networking.websockify.sslKey",
 "options.services.nginx.virtualHosts",
 "options.services.pumpio.sslCert",
 "options.services.pumpio.sslKey",
 "options.services.rippled.package",
 "options.services.xserver.windowManager.xmonad.haskellPackages",
 "options.systemd.services",
 "options.virtualisation.openstack.glance.package"]

