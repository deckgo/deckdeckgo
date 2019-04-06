with { pkgs = import ./nix {}; };
pkgs.haskellPackages.developPackage
  { root = ./.;
    modifier = drv: drv // { buildInputs = drv.buildInputs ++ [ pkgs.cabal-install ]; } ;
  }
