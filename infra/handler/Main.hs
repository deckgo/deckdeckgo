{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified API
import Control.Concurrent.MVar
import Network.Wai (Application)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Servant as Servant

main :: IO ()
main = run $ Servant.serve API.api server

server :: Servant.Server API.API
server = pure []

-- https://docs.aws.amazon.com/lambda/latest/dg/eventsources.html#eventsources-api-gateway-request
decodeRequest :: BS.ByteString -> Maybe Wai.Request
decodeRequest _ = pure Wai.defaultRequest

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
