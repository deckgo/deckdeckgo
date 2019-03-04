{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Concurrent.MVar
-- import Data.Aeson ((.:))
-- import Data.Function (fix)
import Data.Proxy
import Network.Wai (Application)
import Servant.API ((:>), JSON, Get)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Servant as Servant

data Deck

instance Aeson.ToJSON Deck where
  toJSON = undefined

type API = "decks" :> Get '[JSON] [Deck]

main :: IO ()
main = run $ Servant.serve api server

server :: Servant.Server API
server = pure []

api :: Proxy API
api = Proxy

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

decodeRequest :: BS.ByteString -> Maybe Wai.Request
decodeRequest _ = pure Wai.defaultRequest

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
