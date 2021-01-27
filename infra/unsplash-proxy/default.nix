{ mkDerivation, aeson, base, bytestring, hpack, http-client
, http-client-tls, http-conduit, http-types, servant
, servant-client, servant-server, stdenv, text, wai, wai-cors
, wai-lambda, warp
}:
mkDerivation {
  pname = "unsplash-proxy";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring http-client http-client-tls http-conduit
    http-types servant servant-client servant-server text wai wai-cors
    wai-lambda warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.agpl3;
}
