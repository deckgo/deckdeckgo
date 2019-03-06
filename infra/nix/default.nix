{}:
with rec
{ sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};

  pkgsStatic =
    (import "${sources.static-haskell-nix}/survey"
      { overlays = [ ] ; normalPkgs = import sources.nixpkgs {}; }).pkgs;

  haskellOverride = pkgs':
    { overrides = self: super:
      with
        { mkPackage = name: path:
            { "${name}" =
              pkgs'.haskell.lib.disableLibraryProfiling (
              pkgs'.haskell.lib.disableExecutableProfiling (
              pkgs'.haskell.lib.failOnAllWarnings (
                super.callCabal2nix name (pkgs'.lib.cleanSource path) {}
              )));
            };
        };

      super //
        mkPackage "deckdeckgo-handler" ../handler //
        mkPackage "deckdeckgo-handler-api" ../handler-api //
        mkPackage "deckdeckgo-handler-client" ../handler-client ;

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
{ inherit haskellPackagesStatic haskellPackages sources;
  inherit (import sources.niv {}) niv;
}
