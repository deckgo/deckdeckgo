{}:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  staticPkgs = pkgs.pkgsMusl;
  compiler = "ghc865";

  haskellPackages = with pkgs.haskell.lib; staticPkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      google-key-updater = self.callPackage ../google-key-updater {};
      deckdeckgo-handler = self.callPackage ../handler {};
      unsplash-proxy = self.callPackage ../unsplash-proxy {};
      firebase-login = self.callPackage ../firebase-login {};
      telegram-api = self.callPackage telegram-api-pkg {};
    };
  };

in

pkgs //
{ inherit haskellPackages sources; } //
{ terraform = pkgs.terraform_0_12 ; }
