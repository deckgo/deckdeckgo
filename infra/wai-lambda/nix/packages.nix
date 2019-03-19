{ haskell
, haskellPackages
, lib
, writeText
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
}
