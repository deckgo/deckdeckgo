{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import Control.Concurrent.MVar
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Bifunctor
import Data.Function (fix)
import Network.Wai (Application)
import System.Directory (renameFile)
import System.IO
import System.IO.Unsafe
import System.Timeout
import Data.IORef
import Text.Read (readMaybe)
import qualified Data.Binary.Builder as Binary
import qualified API
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HMap
import qualified Data.IP as IP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Types as H
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Servant as Servant
import qualified System.IO.Temp as Temp

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  run $ Servant.serve API.api server

server :: Servant.Server API.API
server = pure []

decodeInput :: BS.ByteString -> Either (Aeson.JSONPath, String) (FilePath, Wai.Request)
decodeInput = Aeson.eitherDecodeStrictWith Aeson.jsonEOF $ Aeson.iparse $
    Aeson.withObject "input" $ \obj ->
      (,) <$>
        obj .: "responseFile" <*>
        (obj .: "request" >>= parseRequest)

-- https://docs.aws.amazon.com/lambda/latest/dg/eventsources.html#eventsources-api-gateway-request
-- https://www.stackage.org/haddock/lts-13.10/wai-3.2.2/src/Network.Wai.Internal.html#Request
parseRequest :: Aeson.Value -> Aeson.Parser Wai.Request
parseRequest = Aeson.withObject "request" $ \obj -> do

    -- "httpMethod": "GET"
    requestMethod <- obj .: "httpMethod" >>=
      Aeson.withText "requestMethod" (pure . T.encodeUtf8)

    -- We don't get data about the version, just assume
    httpVersion <- pure H.http11

    -- "queryStringParameters": {
    --    "name": "me"
    --  },
    -- XXX: default to empty object for the query params as Lambda doesn't set
    -- 'queryStringParameters' if there are no query parameters
    queryParams <- obj .:? "queryStringParameters" .!= Aeson.Object HMap.empty >>=
      Aeson.withObject "queryParams" (
        fmap
          (fmap (first T.encodeUtf8) . HMap.toList ) .
          traverse (Aeson.withText "queryParam" (pure . T.encodeUtf8))
      )

    rawQueryString <- pure $ H.renderSimpleQuery True queryParams

    -- "path": "/test/hello",
    path <- obj .: "path" >>=
      Aeson.withText "path" (pure . T.encodeUtf8)

    rawPathInfo <- pure $ path <> rawQueryString

    --  "headers": {
    --    "Accept": "text/html,application/xhtml+xml,...",
    --    ...
    --    "X-Forwarded-Proto": "https"
    --  },
    requestHeaders <- obj .: "headers" >>=
      Aeson.withObject "headers" (
        fmap
          (fmap (first (CI.mk . T.encodeUtf8)) . HMap.toList) .
          traverse (Aeson.withText "header" (pure . T.encodeUtf8))
      )

    isSecure <- pure $ case lookup "X-Forwarded-Proto" requestHeaders of
      Just "https" -> True
      _ -> False


    --  "requestContext": {
    --    ...
    --    "identity": {
    --      ...
    --      "sourceIp": "192.168.100.1",
    --    },
    --    ...
    --  },
    remoteHost <- obj .: "requestContext" >>=
      Aeson.withObject "requestContext" (\obj' ->
        obj' .: "identity" >>=
          Aeson.withObject "identity" (\idt -> do
              sourceIp <- case HMap.lookup "sourceIp" idt of
                Nothing -> fail "no sourceIp"
                Just (Aeson.String x) -> pure $ T.unpack x
                Just _ -> fail "bad type for sourceIp"
              ip <- case readMaybe sourceIp of
                Just ip -> pure ip
                Nothing -> fail "cannot parse sourceIp"

              pure $ case ip of
                IP.IPv4 ip4 ->
                  Socket.SockAddrInet
                    0 -- default port
                    (IP.toHostAddress ip4)
                IP.IPv6 ip6 ->
                  Socket.SockAddrInet6
                    0 -- default port
                    0 -- flow info
                    (IP.toHostAddress6 ip6)
                    0 -- scope id
          )
      )

    pathInfo <- pure $ H.decodePathSegments path
    queryString <- pure $ H.parseQuery rawQueryString

    -- XXX: default to empty body as Lambda doesn't always set one (e.g. GET
    -- requests)
    requestBodyRaw <- obj .:? "body" .!= Aeson.String "" >>=
      Aeson.withText "body" (pure . T.encodeUtf8)
    (requestBody, requestBodyLength) <- pure
      ( pure requestBodyRaw
      , Wai.KnownLength $ fromIntegral $ BS.length requestBodyRaw)

    vault <- pure $ Vault.insert originalRequestKey obj Vault.empty

    requestHeaderHost <- pure $ lookup "host" requestHeaders
    requestHeaderRange <- pure $ lookup "range" requestHeaders
    requestHeaderReferer <- pure $ lookup "referer" requestHeaders
    requestHeaderUserAgent <- pure $ lookup "User-Agent" requestHeaders

    pure $ Wai.Request {..}

originalRequestKey :: Vault.Key Aeson.Object
originalRequestKey = unsafePerformIO Vault.newKey
{-# NOINLINE originalRequestKey #-}

-- https://docs.aws.amazon.com/lambda/latest/dg/eventsources.html#eventsources-api-gateway-response
encodeResponse :: Wai.Response -> IO BS.ByteString
encodeResponse resp = do
    respObj <- toJSONResponse resp
    pure $ BL.toStrict $ Aeson.encode $ Aeson.Object respObj

toJSONResponse :: Wai.Response -> IO Aeson.Object
toJSONResponse (Wai.responseToStream -> (st,hdrs,mkBody)) = do
    body <- mkBody drainBody
    pure $ HMap.fromList
      [ ("statusCode", Aeson.Number (fromIntegral (H.statusCode st)))
      , ("headers", Aeson.toJSON $ HMap.fromList $
          (bimap T.decodeUtf8 T.decodeUtf8 . first CI.original) <$> hdrs)
      , ("body", Aeson.String (T.decodeUtf8 body))
      ]
  where
    drainBody :: Wai.StreamingBody -> IO BS.ByteString
    drainBody body = do
      ioref <- newIORef Binary.empty
      body
        (\b -> atomicModifyIORef ioref (\b' -> (b <> b', ())))
        (pure ())
      BL.toStrict . Binary.toLazyByteString <$> readIORef ioref

run :: Application -> IO ()
run app = xif BS.empty $ \loop leftover ->
    -- XXX: we don't use getLine because it errors out on EOF; here we deal
    -- with this explicitly
    BS.hGetSome stdin 1024 >>= \bs ->
      if BS.null bs
      then putStrLn "Reached EOF! Good bye :)"
      else case second BS8.uncons $ BS8.break (== '\n') (leftover <> bs) of
        (_tmpLine, Nothing) -> loop (leftover <> bs)
        (line, Just ('\n', rest)) -> do
          handle line
          loop rest
        -- TODO: proper error message here
        (_tmpLine, Just{}) -> error "Something terrible happened"
  where
    handle = handleRequest app

handleRequest :: Application -> BS.ByteString -> IO ()
handleRequest app bs = case decodeInput bs of
    Left err -> putStrLn $ "Cannot decode! " <> show err
    Right (fp, req) -> do
      mvar <- newEmptyMVar
      mresp <- timeout 1000000 $ app req $ \resp -> do
        putMVar mvar resp
        pure Wai.ResponseReceived
      case mresp of
        Nothing -> putStrLn "Didn't get a response in time!"
        Just Wai.ResponseReceived -> do
          resp <- takeMVar mvar
          putStrLn $ "Writing to " <> fp
          Temp.withSystemTempFile "temp-response" $ \tmpFp h -> do
            hClose h
            BS.writeFile tmpFp =<< encodeResponse resp
            renameFile tmpFp fp

xif :: b -> ((b -> c) -> b -> c) -> c
xif = flip fix
