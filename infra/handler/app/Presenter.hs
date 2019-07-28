{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Aeson ((.:))
import DeckGo.Handler
import DeckGo.Presenter
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
import qualified Data.Text.Encoding as T
import qualified Hasql.Connection as Hasql
import qualified Network.AWS.Extended as Aws
import qualified Network.Wai.Handler.Lambda as Lambda

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv

    liftIO $ putStrLn "Booted!"

    putStrLn "entering loop..."
    forever $ do
      line <- BS8.getLine
      putStrLn $ "Got line!: " <> show line
      case decodeInput parseSQSRequest line of
        Right (fp, deckIds) -> do
          putStrLn $ "Got input!"
          putStrLn $ "deck ids: " <> concatMap (T.unpack . unDeckId) deckIds
          when (length deckIds /= 1) $
            putStrLn $ "Warning: got " <> show (length deckIds) <> "deck IDs"
          conn <- getPostgresqlConnection
          forM_ deckIds (deployDeck env conn)
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

parseSQSRequest :: Aeson.Value -> Aeson.Parser [DeckId]
parseSQSRequest = Aeson.withObject "request" $ \o -> do
    records <- o .: "Records"
    forM records $ Aeson.withObject "record" $ \o' -> do
      jsonEncodedBody <- o' .: "body"
      case  Aeson.decodeStrict (T.encodeUtf8 jsonEncodedBody) of
        Nothing -> mzero
        Just deckId -> pure deckId

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
