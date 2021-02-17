{ mkDerivation
, aeson
, base
, bytestring
, hpack
, http-client
, http-client-tls
, http-conduit
, jose
, lens
, mtl
, network-uri
, pem
, servant
, servant-client-core
, servant-server
, servant-swagger
, stdenv
, text
, unordered-containers
, wai
, word8
, x509
}:
mkDerivation {
  pname = "firebase-login";
  version = "0.0.0";
  src = ./.;
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  doHaddock = false;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    http-client
    http-client-tls
    http-conduit
    jose
    lens
    mtl
    network-uri
    pem
    servant
    servant-client-core
    servant-server
    servant-swagger
    text
    unordered-containers
    wai
    word8
    x509
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.mit;
}
