{ system ? builtins.currentSystem }:

with import <nixpkgs/nixos/lib/testing.nix> { inherit system; };
with import <nixpkgs/nixos/lib/qemu-flags.nix>;
let
  pkgs = import <nixpkgs> { overlays = [ (import ../overlay.nix) ]; };
in
with pkgs.lib;
let

  # The configuration to install.
  makeConfig = {}:
    pkgs.writeText "configuration.nix" (builtins.readFile ../template/qemu.nix);

  channelContents = [ pkgs.rlwrap ];

  # The test script boots a NixOS VM, installs NixOS on an empty hard
  # disk, and then reboot from the hard disk.  It's parameterized with
  # a test script fragment `createPartitions', which must create
  # partitions and filesystems.
  testScriptFun = { bootLoader, createPartitions, grubVersion, grubDevice
                  , grubIdentifier, preBootCommands, extraConfig
                  }:
    let
      iface = if grubVersion == 1 then "ide" else "virtio";
      qemuFlags =
        (if system == "x86_64-linux" then "-m 768 " else "-m 512 ") +
        (optionalString (system == "x86_64-linux") "-cpu kvm64 ");
      hdFlags = ''hda => "vm-state-machine/machine.qcow2", hdaInterface => "${iface}", ''
        + optionalString (bootLoader == "systemd-boot") ''bios => "${pkgs.OVMF.fd}/FV/OVMF.fd", '';
    in
    ''
      $machine->start;

      # Make sure that we get a login prompt etc.
      $machine->succeed("echo hello");

      # Wait for hard disks to appear in /dev
      $machine->succeed("udevadm settle");

      $machine->waitForUnit("nixos-installer");
      $machine->waitForOpenPort(8081);
      $machine->sleep(10);
      $machine->succeed("curl -f http://localhost:8081");

      subtest "Configures the system", sub {
        # TODO: This should be done by the installer
        ${createPartitions}
        $machine->succeed("nixos-generate-config --root /mnt");

        $machine->succeed('mkdir -p /mnt/etc/nixos');
        $machine->succeed('curl -X POST -d \'{"time":{"timeZone":"Africa/Bamako"},"users":{"extraUsers":{"user":{"name":"user","initialPassword":"pass"}}},"networking":{"hostName":"host"}}\' http://localhost:8081/save');
        $machine->sleep(1);
        $machine->succeed("cat /mnt/etc/nixos/installer.json >&2");
        $machine->succeed("cat /mnt/etc/nixos/configuration.nix >&2");
        $machine->succeed("cat /mnt/etc/nixos/hardware-configuration.nix >&2");
      };

      # Perform the installation.
      $machine->succeed("nixos-install < /dev/null >&2");

      # Do it again to make sure it's idempotent.
      $machine->succeed("nixos-install < /dev/null >&2");

      $machine->succeed("umount /mnt/boot || true");
      $machine->succeed("umount /mnt");
      $machine->succeed("sync");

      $machine->shutdown;

      # Now see if we can boot the installation.
      $machine = createMachine({ ${hdFlags} qemuFlags => "${qemuFlags}", name => "boot-after-install" });

      # For example to enter LUKS passphrase.
      ${preBootCommands}

      # Did /boot get mounted?
      $machine->waitForUnit("local-fs.target");

      ${if bootLoader == "grub" then
          ''$machine->succeed("test -e /boot/grub");''
        else
          ''$machine->succeed("test -e /boot/loader/loader.conf");''
      }

      # Check whether /root has correct permissions.
      $machine->succeed("stat -c '%a' /root") =~ /700/ or die;

      # Did the swap device get activated?
      # uncomment once https://bugs.freedesktop.org/show_bug.cgi?id=86930 is resolved
      $machine->waitForUnit("swap.target");
      $machine->succeed("cat /proc/swaps | grep -q /dev");

      # Check that the store is in good shape
      $machine->succeed("nix-store --verify --check-contents >&2");

      # Check whether the channel works.
      $machine->succeed("nix-env -iA nixos.procps >&2");
      $machine->succeed("type -tP ps | tee /dev/stderr") =~ /.nix-profile/
          or die "nix-env failed";

      # Check that the daemon works, and that non-root users can run builds (this will build a new profile generation through the daemon)
      $machine->succeed("su alice -l -c 'nix-env -iA nixos.procps' >&2");

      # Check whether nixos-rebuild works.
      $machine->succeed("nixos-rebuild switch >&2");

      # Test nixos-option.
      $machine->succeed("nixos-option boot.initrd.kernelModules | grep virtio_console");
      $machine->succeed("nixos-option boot.initrd.kernelModules | grep 'List of modules'");
      $machine->succeed("nixos-option boot.initrd.kernelModules | grep qemu-guest.nix");

      $machine->shutdown;

      # Check whether a writable store build works
      $machine = createMachine({ ${hdFlags} qemuFlags => "${qemuFlags}", name => "rebuild-switch" });
      ${preBootCommands}
      $machine->waitForUnit("multi-user.target");
      $machine->succeed("nixos-rebuild boot >&2");
      $machine->shutdown;

      # And just to be sure, check that the machine still boots after
      # "nixos-rebuild switch".
      $machine = createMachine({ ${hdFlags} qemuFlags => "${qemuFlags}", "boot-after-rebuild-switch" });
      ${preBootCommands}
      $machine->waitForUnit("network.target");
      $machine->shutdown;
    '';

  makeInstallerTest = name:
    { createPartitions, preBootCommands ? "", extraConfig ? ""
    , extraInstallerConfig ? {}
    , bootLoader ? "grub" # either "grub" or "systemd-boot"
    , grubVersion ? 2, grubDevice ? "/dev/vda", grubIdentifier ? "uuid"
    , enableOCR ? false, meta ? {}
    }:
    makeTest {
      inherit enableOCR;
      name = "installer-" + name;
      meta = with pkgs.stdenv.lib.maintainers; {
        # put global maintainers here, individuals go into makeInstallerTest fkt call
        maintainers = [ manveru ] ++ (meta.maintainers or []);
      };
      nodes = {

        # The configuration of the machine used to run "nixos-install". It
        # also has a web server that simulates cache.nixos.org.
        machine =
          { config, lib, pkgs, ... }:

          { imports =
              [ <nixpkgs/nixos/modules/profiles/installation-device.nix>
                <nixpkgs/nixos/modules/profiles/base.nix>
                ../template/nixos-installer-service.nix
                extraInstallerConfig
              ];

            virtualisation.diskSize = 8 * 1024;
            virtualisation.memorySize = 1024;
            virtualisation.writableStore = true;

            # Use a small /dev/vdb as the root disk for the
            # installer. This ensures the target disk (/dev/vda) is
            # the same during and after installation.
            virtualisation.emptyDiskImages = [ 512 ];
            virtualisation.bootDevice =
              if grubVersion == 1 then "/dev/sdb" else "/dev/vdb";
            virtualisation.qemu.diskInterface =
              if grubVersion == 1 then "scsi" else "virtio";

            boot.loader.systemd-boot.enable = mkIf (bootLoader == "systemd-boot") true;

            hardware.enableAllFirmware = mkForce false;

            # The test cannot access the network, so any packages we
            # need must be included in the VM.
            system.extraDependencies = with pkgs;
              [ sudo
                libxml2.bin
                libxslt.bin
                docbook5
                docbook5_xsl
                unionfs-fuse
                ntp
                nixos-artwork.wallpapers.gnome-dark
                perlPackages.XMLLibXML
                perlPackages.ListCompare

                # add curl so that rather than seeing the test attempt to download
                # curl's tarball, we see what it's trying to download
                curl
              ]
              ++ optional (bootLoader == "grub" && grubVersion == 1) pkgs.grub
              ++ optionals (bootLoader == "grub" && grubVersion == 2) [ pkgs.grub2 pkgs.grub2_efi ];

            nix.binaryCaches = mkForce [ ];
          };

      };

      testScript = testScriptFun {
        inherit bootLoader createPartitions preBootCommands
                grubVersion grubDevice grubIdentifier extraConfig;
      };
    };
in {
  simple = makeInstallerTest "simple"
    { createPartitions =
        ''
          $machine->succeed(
              "parted --script /dev/vda mklabel msdos",
              "parted --script /dev/vda -- mkpart primary linux-swap 1M 1024M",
              "parted --script /dev/vda -- mkpart primary ext2 1024M -1s",
              "udevadm settle",
              "mkswap /dev/vda1 -L swap",
              "swapon -L swap",
              "mkfs.ext3 -L nixos /dev/vda2",
              "mount LABEL=nixos /mnt",
          );
        '';
    };
}
