{}:
with rec
{ sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  wai-lambda = pkgs.callPackage "${sources.wai-lambda}/nix/packages.nix" {};
  surveyor = pkgs.callPackage ../surveyor {};

  pkgsStatic =
    (import "${sources.static-haskell-nix}/survey"
      { overlays = [ ] ; normalPkgs = import sources.nixpkgs {}; }).pkgs;

  haskellOverride = pkgs':
    { overrides = self: super:
      with
        { mkPackage = name: path:
            { "${name}" =
                with pkgs'.haskell.lib;
                disableLibraryProfiling (
                disableExecutableProfiling (
                failOnAllWarnings (
                super.callCabal2nix name (pkgs'.lib.cleanSource path) {}
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
    (haskellOverride pkgsStatic.pkgsMusl);

  haskellPackages = pkgs.haskellPackages.override
    (haskellOverride pkgs);

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
