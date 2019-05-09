{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified DeckGo.Handler
import qualified Network.AWS as Aws
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Lambda as Lambda
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Hasql.Connection as Hasql
import System.Environment (getEnv)
import qualified Servant.Auth.Firebase as Firebase
import qualified Data.Text as T

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv Aws.Discover

    liftIO $ putStrLn "Booted!"

    settings <- getFirebaseSettings
    conn <- getPostgresqlConnection

    Lambda.run $ cors $ DeckGo.Handler.application settings env conn


-- TODO: factor out
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

cors :: Wai.Middleware
cors = Cors.cors $
      const $
      Just Cors.simpleCorsResourcePolicy { Cors.corsMethods = methods }

methods :: [HTTP.Method]
methods =
    [ "GET"
    , "HEAD"
    , "POST"
    , "DELETE"
    , "PUT"
    ]
