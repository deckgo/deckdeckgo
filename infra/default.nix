with { pkgs = import ./nix {}; };

rec
{ function = pkgs.runCommand "build-function" {}
  ''
    cp ${./main.js} main.js
    # Can't be called 'main' otherwise lambda tries to load it
    cp ${main}/bin/deckdeckgo-handler main_hs
    mkdir $out
    ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
  '';

  main = pkgs.haskellPackagesStatic.deckdeckgo-handler;
}
