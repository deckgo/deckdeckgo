{ haskell
, haskellPackages
, lib
, runCommand
, writeText
, zip
}:
rec
{ firebase-login-sdist = haskell.lib.sdistTarball firebase-login;
  firebase-login = haskellPackages.callPackage ../../firebase-login {};
  firebase-login-source = lib.sourceByRegex ../.
    [ "^package.yaml$"
      "^src.*"
      "^examples.*"
      "^README.md$"
      "^LICENSE$"
    ];
  firebase-login-version-file = writeText "version" firebase-login.version;
}
