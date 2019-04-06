# TODO: port tests
# TODO: fix sources
# TODO: drop nix/packages
with { pkgs = import ./nix {}; };
pkgs.callPackage ./nix/packages.nix {}
