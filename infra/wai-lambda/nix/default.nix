{ sources ? import ./sources.nix }:
with
  { overlay = _: pkgs:
      { inherit (import sources.niv {}) niv;
        packages = import ./packages.nix { inherit (pkgs) haskell lib haskellPackages; };
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
