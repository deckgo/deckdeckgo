{ sources ? import ./sources.nix }:
with
  { overlay = _: pkgs: rec
      { inherit (import sources.niv {}) niv;

        packages = import ./packages.nix
          { inherit (pkgs) haskell lib ;
            inherit haskellPackages;
          };
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
