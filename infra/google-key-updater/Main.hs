{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Network.AWS.Data.Body as Body
import Control.Monad
import Data.Aeson ((.:))
import System.Environment (getEnv)
import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.AWS as Aws
import qualified Network.AWS.S3 as S3
import qualified Network.HTTP.Simple as HTTP
import qualified Network.Wai.Handler.Lambda as Lambda

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv Aws.Discover

    bucketName <- getEnv "META_BUCKET_NAME"
    let bucket = S3.BucketName (T.pack bucketName)
    putStrLn $ "Bucket is: " <> show bucket

    liftIO $ putStrLn "Booted!"

    putStrLn "entering loop..."
    forever $ do
      line <- BS8.getLine
      putStrLn $ "Got line!: " <> show line
      case decodeInput (pure . const ()) line of
        Right (fp, ()) -> do
          putStrLn $ "Got input!"
          keys <- HTTP.getResponseBody <$>
            HTTP.httpBS ( HTTP.parseRequest_
            "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"
            )

          print keys

          let body = Body.toBody keys
          let okey = "google-public-keys"

          runAWS env (
            Aws.send $ S3.putObject bucket okey body
            ) >>= \case
              Right {} -> do
                putStrLn $ "uploaded new keys"
              Left e -> error $ "Error in put: " <> show e

          Lambda.writeFileAtomic fp (BL.toStrict $ Aeson.encode ())
        Left e -> error $ show e

decodeInput
  :: (Aeson.Value -> Aeson.Parser a)
  -> BS.ByteString
  -> Either (Aeson.JSONPath, String) (FilePath, a)
decodeInput parseEvent =
    Aeson.eitherDecodeStrictWith Aeson.jsonEOF $ Aeson.iparse $
      Aeson.withObject "input" $ \obj ->
        (,) <$>
          obj .: "responseFile" <*>
          (obj .: "request" >>= parseEvent)

runAWS :: MonadIO m => Aws.Env -> Aws.AWS a -> m (Either SomeException a)
runAWS env =
    liftIO .
    tryAny .
    Aws.runResourceT .
    Aws.runAWS env .
    Aws.within Aws.NorthVirginia
