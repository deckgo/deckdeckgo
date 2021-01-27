{}:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  staticPkgs = pkgs.pkgsMusl;
  compiler = "ghc865";

  haskellPackages = with pkgs.haskell.lib; staticPkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      # Dependencies we need to patch
      google-key-updater = self.callPackage ../google-key-updater {};
      deckdeckgo-handler = self.callPackage ../handler {};
      unsplash-proxy = self.callPackage ../unsplash-proxy {};
      #hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
      telegram-api = self.callPackage telegram-api-pkg {};
    };
  };

in

pkgs //
{ inherit haskellPackages sources; } //
{ terraform = pkgs.terraform_0_12 ; }
