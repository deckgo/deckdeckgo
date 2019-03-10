{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import System.IO
import qualified Servant as Servant
import qualified Network.Wai.Handler.Lambda as Lambda
import Data.Proxy
import Servant.API ((:>), JSON, Get)
import qualified Data.Aeson as Aeson

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

data Deck = Deck String


instance Aeson.ToJSON Deck where
  toJSON _ = Aeson.object []

instance Aeson.FromJSON Deck where
  parseJSON _ = pure $ Deck "some-deck"

type API = "decks" :> Get '[JSON] [Deck]

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Lambda.run $ Servant.serve api server

server :: Servant.Server API
server = pure [Deck "<h1>hello!</h1>"]
