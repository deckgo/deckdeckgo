{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-s3, base
, bytestring, hpack, http-client, http-client-tls, http-conduit
, http-types, servant, servant-client, servant-server, stdenv, text
, unliftio, wai, wai-cors, wai-lambda, warp, pkgsMusl
}:
mkDerivation {
  pname = "google-key-updater";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-s3 base bytestring
    http-client http-client-tls http-conduit http-types servant
    servant-client servant-server text unliftio wai wai-cors wai-lambda
    warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.agpl3;
  configureFlags = [

    "-flambda"
    "--ghc-option=-optl=-static"

          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgsMusl.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgsMusl.zlib.static}/lib"
          "--extra-lib-dirs=${pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ];
}
