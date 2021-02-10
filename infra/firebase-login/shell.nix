with { pkgs = import ../nix {}; };

pkgs.haskellPackages.firebase-login.env.overrideAttrs(oldAttrs: {
  nativeBuildInputs = oldAttrs.nativeBuildInputs or [] ++ [ pkgs.cabal-install ];
})
