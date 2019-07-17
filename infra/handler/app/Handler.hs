{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit.Binary as C
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
import qualified Data.Text.Encoding as T
import qualified Network.AWS.Data as Data
import qualified Network.AWS.S3 as S3
import Control.Lens
import Control.Monad.Trans.Resource

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv Aws.Discover

    liftIO $ putStrLn "Booted!"

    settings <- getFirebaseSettings env
    conn <- getPostgresqlConnection

    Lambda.run $ cors $ DeckGo.Handler.application settings env conn

-- | Transforms the request to hit the region-specific S3, otherwise this
-- doesn't go through the VPC endpoint.
-- TODO: move this to 'fixupEnv'
fixupEnv' :: Aws.Env -> Aws.Env
fixupEnv' = Aws.configure $ S3.s3
  { Aws._svcEndpoint = \reg -> do
      let new = "s3." <> Data.toText reg <> ".amazonaws.com"
      (Aws._svcEndpoint S3.s3 reg) & Aws.endpointHost .~ T.encodeUtf8 new
  }

-- TODO: factor out
getFirebaseSettings :: Aws.Env -> IO Firebase.FirebaseLoginSettings
getFirebaseSettings (fixupEnv' -> env) = do
    pid <- getEnv "FIREBASE_PROJECT_ID"

    metaBucketName <- getEnv "META_BUCKET_NAME"
    let metaBucket = S3.BucketName (T.pack metaBucketName)
    putStrLn $ "META Bucket is: " <> show metaBucket

    let fetchKeys = Aws.runResourceT $ do
          let okey = "google-public-keys"

          runAWS' env (
            Aws.send $ S3.getObject metaBucket okey
            ) >>= \case
              Right gors -> do
                keysRaw <- (gors ^. S3.gorsBody) `Aws.sinkBody` C.sinkLbs
                liftIO $ putStrLn $ "got new keys"
                case Aeson.decode keysRaw of
                  Nothing -> error "Could not decode key file"
                  Just keyMap -> pure keyMap
              Left e -> error $ "Error in get: " <> show e

    pure Firebase.FirebaseLoginSettings
      { Firebase.firebaseLoginProjectId = Firebase.ProjectId (T.pack pid)
      , Firebase.firebaseLoginGetKeys = fetchKeys
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

runAWS'
  :: (MonadResource m, MonadIO m, MonadUnliftIO m)
  => Aws.Env -> Aws.AWS a -> m (Either SomeException a)
runAWS' env =
    tryAny .
    Aws.runAWS env .
    Aws.within Aws.NorthVirginia
