{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Proxy
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Lambda as Lambda
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Servant as Servant
import Servant.API
import qualified Servant.Client as Client
import System.Environment (getEnv)
import System.IO

-- https://api.unsplash.com/search/photos/?query={searchTerm}&page={next}&client_id={secret_key}
-- https://api.unsplash.com/photos/{photoId}/download/?client_id={secret_key}

newtype UnsplashQuery = UnsplashQuery {_unUnsplashQuery :: T.Text}
  deriving newtype (FromHttpApiData, ToHttpApiData)

newtype UnsplashCliendId = UnsplashCliendId {_unUnsplashClientId :: T.Text}
  deriving newtype (FromHttpApiData, ToHttpApiData)

newtype UnsplashPhotoId = UnsplashPhotoId {_unUnsplashPhotoId :: T.Text}
  deriving newtype (FromHttpApiData, ToHttpApiData)

type ServerAPI = "unsplash" :> ClientAPI

type ClientAPI =
  "search"
    :> "photos"
    :> QueryParam "query" UnsplashQuery
    :> QueryParam "client_id" UnsplashCliendId
    :> QueryParam "page" T.Text
    :> Get '[JSON] Aeson.Value
    :<|> "photos"
    :> Capture "photoId" UnsplashPhotoId
    :> "download"
    :> QueryParam "client_id" UnsplashCliendId
    :> Get '[JSON] Aeson.Value

runClient ::
  MonadIO io =>
  Client.ClientM a ->
  io (Either Client.ClientError a)
runClient act = liftIO $ do
  mgr <- HTTP.newManager HTTP.tlsManagerSettings
  Client.runClientM act (clientEnv mgr)
  where
    clientEnv mgr =
      Client.mkClientEnv
        mgr
        (Client.BaseUrl Client.Https "api.unsplash.com" 443 "")

getUnsplashClientId :: IO UnsplashCliendId
getUnsplashClientId =
  (UnsplashCliendId . T.pack) <$> getEnv "UNSPLASH_CLIENT_ID"

proxySearch ::
  Maybe UnsplashQuery ->
  Maybe UnsplashCliendId ->
  Maybe T.Text ->
  Servant.Handler Aeson.Value
proxySearch mq Nothing mPage = do
  c <- liftIO getUnsplashClientId
  liftIO $ putStrLn "proxySearch: calling"
  runClient (proxySearch' mq (Just c) mPage) >>= \case
    Left e -> do
      liftIO $ print e
      Servant.throwError Servant.err500
    Right bs -> pure bs
proxySearch mq (Just c) mPage = do
  liftIO $ putStrLn "proxySearch: calling"
  runClient (proxySearch' mq (Just c) mPage) >>= \case
    Left e -> do
      liftIO $ print e
      Servant.throwError Servant.err500
    Right bs -> pure bs

proxySearch' ::
  Maybe UnsplashQuery ->
  Maybe UnsplashCliendId ->
  Maybe T.Text ->
  Client.ClientM Aeson.Value

proxyDownload' ::
  UnsplashPhotoId ->
  Maybe UnsplashCliendId ->
  Client.ClientM Aeson.Value
proxySearch' :<|> proxyDownload' = Client.client clientApi

proxyDownload ::
  UnsplashPhotoId ->
  Maybe UnsplashCliendId ->
  Servant.Handler Aeson.Value
proxyDownload photId Nothing = do
  c <- liftIO getUnsplashClientId
  liftIO $ putStrLn "proxyDownload: calling"
  runClient (proxyDownload' photId (Just c)) >>= \case
    Left e -> do
      liftIO $ print e
      Servant.throwError Servant.err500
    Right bs -> pure bs
proxyDownload photId (Just c) = do
  liftIO $ putStrLn "proxyDownload: calling"
  runClient (proxyDownload' photId (Just c)) >>= \case
    Left e -> do
      liftIO $ print e
      Servant.throwError Servant.err500
    Right bs -> pure bs

serverApi :: Proxy ServerAPI
serverApi = Proxy

clientApi :: Proxy ClientAPI
clientApi = Proxy

server :: Servant.Server ServerAPI
server = proxySearch :<|> proxyDownload

application :: Wai.Application
application = Servant.serve serverApi server

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Lambda.runSettings settings $ cors $ application
  where
    settings =
      Lambda.defaultSettings
        { Lambda.timeoutValue = 9 * 1000 * 1000
        }

cors :: Wai.Middleware
cors =
  Cors.cors
    $ const
    $ Just Cors.simpleCorsResourcePolicy {Cors.corsMethods = methods}

methods :: [HTTP.Method]
methods =
  [ "GET",
    "HEAD"
  ]
