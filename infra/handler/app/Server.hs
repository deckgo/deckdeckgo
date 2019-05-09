{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import UnliftIO
import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClient
import qualified Network.AWS as Aws
import qualified DeckGo.Handler
import qualified Network.Wai.Handler.Warp as Warp
import qualified Hasql.Connection as Hasql
import qualified Servant.Auth.Firebase as Firebase
import qualified Data.Text as T
import System.Environment (getEnv)

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    mgr <- HTTPClient.newManager HTTPClient.tlsManagerSettings
            { HTTPClient.managerModifyRequest =
                pure . rerouteDynamoDB
            }
    conn <- getPostgresqlConnection
    env <- Aws.newEnv Aws.Discover <&> Aws.envManager .~ mgr
    settings <- getFirebaseSettings
    Warp.run 8080 $ DeckGo.Handler.application settings env conn

getFirebaseSettings :: IO Firebase.FirebaseLoginSettings
getFirebaseSettings = do
    pkeys <- getEnv "GOOGLE_PUBLIC_KEYS"
    pid <- getEnv "FIREBASE_PROJECT_ID"
    keyMap <- Aeson.decodeFileStrict pkeys >>= \case
      Nothing -> error "Could not decode key file"
      Just keyMap -> pure keyMap
    pure Firebase.FirebaseLoginSettings
      { Firebase.firebaseLoginProjectId = Firebase.ProjectId (T.pack pid)
      , Firebase.firebaseLoginGetKeys = pure keyMap
      }

getPostgresqlConnection :: IO Hasql.Connection
getPostgresqlConnection = do
    user <- getEnv "PGUSER"
    password <- getEnv "PGPASSWORD"
    host <- getEnv "PGHOST"
    db <- getEnv "PGDATABASE"
    port <- getEnv "PGPORT"
    Hasql.acquire (
      Hasql.settings
        (BS8.pack host)
        (read port)
        (BS8.pack user)
        (BS8.pack password)
        (BS8.pack db)
      ) >>= \case
        Left e -> error (show e)
        Right c -> pure c

rerouteDynamoDB :: HTTPClient.Request -> HTTPClient.Request
rerouteDynamoDB req =
    case HTTPClient.host req of
      "dynamodb.us-east-1.amazonaws.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 8000
          , HTTPClient.secure = False
          }
      _ -> req
