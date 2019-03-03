{}:
with rec
{ sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};

  pkgsStatic =
    (import "${sources.static-haskell-nix}/survey"
      { overlays = [ ] ; normalPkgs = import sources.nixpkgs {}; }).pkgs;

  normalHaskellPackages = pkgsStatic.pkgsMusl.haskellPackages.override
    { overrides = self: super: super //
      { deckdeckgo-handler = super.callCabal2nix "handler"
          (pkgs.lib.cleanSource ../handler) {};
      };
    };

  haskellPackagesStatic =
    (import "${sources.static-haskell-nix}/survey"
      { overlays = [ ] ; normalPkgs = pkgs; inherit normalHaskellPackages; }
    ).haskellPackages;
};

pkgs //
{ inherit haskellPackagesStatic sources;
  inherit (import sources.niv {}) niv;
}
