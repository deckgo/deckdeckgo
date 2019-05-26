{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import UnliftIO
import qualified Data.Aeson as Aeson
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
-- import qualified DeckGo.Handler
-- import qualified Network.AWS as Aws
-- import qualified Network.HTTP.Types as HTTP
-- import qualified Network.Wai as Wai
-- import qualified Network.Wai.Handler.Lambda as Lambda
-- import qualified Network.Wai.Middleware.Cors as Cors
-- import qualified Hasql.Connection as Hasql
-- import System.Environment (getEnv)
-- import qualified Servant.Auth.Firebase as Firebase
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    -- env <- Aws.newEnv Aws.Discover

    liftIO $ putStrLn "Booted!"

    putStrLn "entering loop..."
    forever $ do
      line <- BS8.getLine
      putStrLn $ "Got line!: " <> show line
      case Aeson.eitherDecodeStrict line of
        Right hmap -> case HMS.lookup ("responseFile" :: T.Text) hmap of
          Just fp -> do
            putStrLn $ "Got input! " <> show hmap
            Aeson.encodeFile fp ()
          Nothing -> error "No responseFile :("
        Left e -> error $ show e
