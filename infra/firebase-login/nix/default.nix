{ sources ? import ./sources.nix }:
with
  { overlay = _: pkgs: rec
      { inherit (import sources.niv {}) niv;
        haskellPackages = pkgs.haskellPackages.override
          { overrides = _: super:
              { jose = super.callCabal2nix "jose" sources.hs-jose {}; };
          };

        packages = import ./packages.nix
          { inherit (pkgs) haskell lib ;
            inherit haskellPackages;
          };
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
