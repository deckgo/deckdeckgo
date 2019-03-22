{ sources ? import ./sources.nix }:
with
  { overlay = _: pkgs:
      { inherit (import sources.niv {}) niv;
        surveyor = pkgs.stdenv.mkDerivation
        { name = "surveyor";
          src = pkgs.lib.sourceByRegex ../.
            [ "^Makefile$"
              "^surveyor.c"
            ];
          installPhase = "mkdir -p $out/lib && cp surveyor.so $out/lib";
          } ;
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
