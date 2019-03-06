{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import qualified API
import Control.Concurrent.MVar
import Network.Wai (Application)
import Data.Aeson ((.:))
import Data.Bifunctor
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMap
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.HTTP.Types as H
import qualified Servant as Servant

main :: IO ()
main = run $ Servant.serve API.api server

server :: Servant.Server API.API
server = pure []

-- https://docs.aws.amazon.com/lambda/latest/dg/eventsources.html#eventsources-api-gateway-request
-- https://www.stackage.org/haddock/lts-13.10/wai-3.2.2/src/Network.Wai.Internal.html#Request
decodeRequest :: BS.ByteString -> Maybe Wai.Request
decodeRequest =
    Aeson.decodeStrictWith Aeson.jsonEOF $
      Aeson.parse $ Aeson.withObject "request" $ \obj -> do

        -- "httpMethod": "GET"
        requestMethod <- obj .: "httpMethod" >>=
          Aeson.withText "requestMethod" (pure . T.encodeUtf8)

        -- We don't get data about the version, just assume
        httpVersion <- pure H.http11

        -- "queryStringParameters": {
        --    "name": "me"
        --  },
        queryParams <- obj .: "queryStringParameters" >>=
          Aeson.withObject "queryParams" (
            fmap
              (fmap (first T.encodeUtf8) . HMap.toList )
              . traverse (Aeson.withText "queryParam" (pure . T.encodeUtf8))
          )

        rawQueryString <- pure $ H.renderSimpleQuery True queryParams

        rawPathInfo <- do

          -- "path": "/test/hello",
          path <- obj .: "path" >>=
            Aeson.withText "path" (pure . T.encodeUtf8)

          pure $ path <> rawQueryString

        pure $ Wai.Request {..}

-- https://docs.aws.amazon.com/lambda/latest/dg/eventsources.html#eventsources-api-gateway-response
encodeResponse :: Wai.Response -> BS.ByteString
encodeResponse _ = BL.toStrict $ Aeson.encode $
    Aeson.object
      [ ("statusCode", "200")
      , ("headers", Aeson.object
          [ ("Content-Type", "text/html; charset=utf-8") ]
        )
      , ("body", "<h1>Hello from Servant!</h1>")
      ]

run :: Application -> IO ()
run app = let loop = pure () in -- fix $ \loop ->
    -- BS.getLine >>= \bs ->
      -- if BS.null bs
      -- then pure ()
      -- else case decodeRequest bs of
      case decodeRequest "{}" of
        Nothing -> pure () -- woopsies!
        Just req -> do
          mvar <- newEmptyMVar
          Wai.ResponseReceived <- app req $ \resp -> do
            putMVar mvar resp
            pure Wai.ResponseReceived
          resp <- takeMVar mvar
          BS.putStr (encodeResponse resp)
          loop
