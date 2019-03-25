with
{ pkgs = import ./nix {};
};
let
  pkg = pkgs.haskellPackages.developPackage
    { root = ./handler; };
in
   pkg.overrideAttrs(attr: {
     buildInputs = with pkgs; [ niv terraform awscli ];
   })
