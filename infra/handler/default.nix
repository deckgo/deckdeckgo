{ mkDerivation, aeson, amazonka, amazonka-cloudfront, amazonka-core
, amazonka-s3, amazonka-sqs, base, bytestring, cases, conduit
, conduit-extra, contravariant, cryptonite, directory, filepath
, firebase-login, hasql, hpack, http-client, http-client-tls
, http-types, lens, memory, mime-types, mtl, port-utils, pureMD5
, random, resourcet, servant, servant-client, servant-server
, servant-swagger, servant-swagger-ui, stdenv, swagger2, tagsoup
, tasty, tasty-hunit, temporary, text, time, unliftio
, unordered-containers, wai, wai-cors, wai-lambda, warp
, zip-archive, pkgsMusl
}:
mkDerivation {
  pname = "deckdeckgo-handler";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-cloudfront amazonka-core amazonka-s3
    amazonka-sqs base bytestring cases conduit conduit-extra
    contravariant cryptonite directory filepath firebase-login hasql
    http-client http-types lens memory mime-types mtl pureMD5 random
    resourcet servant servant-server servant-swagger servant-swagger-ui
    swagger2 tagsoup temporary text time unliftio unordered-containers
    wai wai-cors wai-lambda zip-archive
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson amazonka amazonka-cloudfront amazonka-core amazonka-s3
    amazonka-sqs base bytestring cases conduit conduit-extra
    contravariant cryptonite directory filepath firebase-login hasql
    http-client http-client-tls http-types lens memory mime-types mtl
    port-utils pureMD5 random resourcet servant servant-client
    servant-server servant-swagger servant-swagger-ui swagger2 tagsoup
    tasty tasty-hunit temporary text time unliftio unordered-containers
    wai wai-cors wai-lambda warp zip-archive
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
