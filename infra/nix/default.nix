{}:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { overlays  = [
    (self: super: {
      gnutls = super.gnutls.overrideAttrs (_:
        { doCheck = false; }
      );
      p11-kit = super.p11-kit.overrideAttrs (_:
        { doCheck = false; }
      );


    }
    )


  ];};
  staticPkgs = pkgs.pkgsMusl;
  compiler = "ghc865";

  staticHaskellPackages = with pkgs.haskell.lib; staticPkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      google-key-updater = self.callPackage ../google-key-updater {};
      deckdeckgo-handler = self.callPackage ../handler {};
      unsplash-proxy = self.callPackage ../unsplash-proxy {};
      firebase-login = self.callPackage ../firebase-login {};
      #telegram-api = self.callPackage telegram-api-pkg {};
    };
  };

in

pkgs //
{ inherit staticHaskellPackages sources; } //
{ terraform = pkgs.terraform_0_12 ; }
