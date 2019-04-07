{ haskell
, haskellPackages
, lib
, runCommand
, writeText
, zip
}:
rec
{ firebase-login-sdist = haskell.lib.sdistTarball firebase-login;
  firebase-login = haskellPackages.callCabal2nix "firebase-login" firebase-login-source {};
  firebase-login-source = lib.sourceByRegex ../.
    [ "^package.yaml$"
      "^src.*"
      "^examples.*"
      "^README.md$"
      "^LICENSE$"
    ];
  firebase-login-version-file = writeText "version" firebase-login.version;
}
