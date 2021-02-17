{ mkDerivation
, aeson
, base
, bytestring
, hpack
, http-client
, http-client-tls
, http-conduit
, http-types
, servant
, servant-client
, servant-server
, stdenv
, text
, wai
, wai-cors
, wai-lambda
, warp
, pkgsMusl
}:
mkDerivation {
  pname = "unsplash-proxy";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    http-client
    http-client-tls
    http-conduit
    http-types
    servant
    servant-client
    servant-server
    text
    wai
    wai-cors
    wai-lambda
    warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.agpl3;
  configureFlags = [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgsMusl.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgsMusl.zlib.static}/lib"
    "--extra-lib-dirs=${pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
  ];
}
