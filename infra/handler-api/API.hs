{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Proxy
import Servant.API ((:>), JSON, Get)
import qualified Data.Aeson as Aeson

data Deck = Deck String


instance Aeson.ToJSON Deck where
  toJSON = undefined

instance Aeson.FromJSON Deck where
  parseJSON = undefined

type API = "decks" :> Get '[JSON] [Deck]

api :: Proxy API
api = Proxy
