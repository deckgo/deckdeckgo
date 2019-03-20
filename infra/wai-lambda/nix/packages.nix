{ haskell
, haskellPackages
, lib
, runCommand
, writeText
, zip
}:
rec
{ wai-lambda-sdist = haskell.lib.sdistTarball wai-lambda;
  wai-lambda = haskellPackages.callCabal2nix "wai-lambda" wai-lambda-source {};
  wai-lambda-source = lib.sourceByRegex ../.
    [ "^package.yaml$"
      "^src.*"
      "^examples.*"
      "^README.md$"
      "^LICENSE$"
    ];
  wai-lambda-version-file = writeText "version" wai-lambda.version;
  wai-lambda-js-wrapper = ../main.js;
  wai-lambda-js-build-lambda = exe: runCommand "build-lambda" {}
  ''
    cp ${wai-lambda-js-wrapper} main.js
    # Can't be called 'main' otherwise lambda tries to load it
    cp ${exe} main_hs
    mkdir $out
    ${zip}/bin/zip -r $out/function.zip main.js main_hs
  '';
}
