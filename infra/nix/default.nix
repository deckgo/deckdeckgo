{}:
with rec
{ sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  wai-lambda = pkgs.callPackage "${sources.wai-lambda}/nix/packages.nix" {};
  surveyor = pkgs.callPackage ../surveyor {};

  pkgsStatic =
    (import "${sources.static-haskell-nix}/survey"
      { overlays = [ ] ; normalPkgs = import sources.nixpkgs {}; }).pkgs;

  haskellOverride = isStaticBuild:
    { overrides = self: super:
      with rec
        { pkgs' = if isStaticBuild then pkgsStatic.pkgsMusl else pkgs;
          mkPackage = name: path:
          with
            { addStaticLinkerFlagsWithPkgconfig =
                haskellDrv:
                pkgConfigNixPackages:
                pkgconfigFlagsString:
                  pkgs'.haskell.lib.overrideCabal (pkgs'.haskell.lib.appendConfigureFlag haskellDrv [
                    "--ld-option=--start-group"
                  ]) (old: {
                    preConfigure = builtins.concatStringsSep "\n" [
                      (old.preConfigure or "")
                      ''
                        set -e
                        configureFlags+=$(for flag in $(pkg-config --static ${pkgconfigFlagsString}); do echo -n " --ld-option=$flag"; done)
                      ''
                    ];
                    libraryPkgconfigDepends = (old.libraryPkgconfigDepends or []) ++ pkgConfigNixPackages;
                  });
            };
            { "${name}" =
                with
                  { drv =
                    super.callCabal2nix name (pkgs'.lib.cleanSource path) {};
                  };
                with pkgs'.haskell.lib;
                disableLibraryProfiling (
                disableExecutableProfiling (
                failOnAllWarnings (
                  # TODO: _AND_ is executable that depends on openssl
                  if isStaticBuild
                  then
                    let
                      openssl_static = pkgs'.openssl.override { static = true; };
                      in addStaticLinkerFlagsWithPkgconfig drv [openssl_static ] "--libs openssl"
                  else drv
              )));
            };
        };

      super //
        mkPackage "deckdeckgo-handler" ../handler //
        ( mkPackage "wai-lambda" wai-lambda.wai-lambda-source ) //
        ( mkPackage "firebase-login" ../firebase-login ) //
        { jose = super.callCabal2nix "jose" sources.hs-jose {}; } ;
    };
  normalHaskellPackages = pkgsStatic.pkgsMusl.haskellPackages.override
    (haskellOverride true);

  haskellPackages = pkgs.haskellPackages.override
    (haskellOverride false);

  haskellPackagesStatic =
    (import "${sources.static-haskell-nix}/survey"
      { overlays = [ ] ; normalPkgs = pkgs; inherit normalHaskellPackages; }
    ).haskellPackages;
};

pkgs //
{ inherit haskellPackagesStatic haskellPackages sources wai-lambda;
  inherit (surveyor) surveyor;
  inherit (import sources.niv {}) niv;
}
