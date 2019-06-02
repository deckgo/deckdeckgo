{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- import System.Environment (getEnv)
-- import qualified Hasql.Connection as Hasql
-- import qualified Network.HTTP.Types as HTTP
-- import qualified Network.Wai as Wai
-- import qualified Network.Wai.Middleware.Cors as Cors
-- import qualified Servant.Auth.Firebase as Firebase
import Control.Monad
import DeckGo.Handler
import DeckGo.Presenter
import UnliftIO
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Network.AWS as Aws
import qualified Network.Wai.Handler.Lambda as Lambda

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv Aws.Discover

    liftIO $ putStrLn "Booted!"

    putStrLn "entering loop..."
    forever $ do
      line <- BS8.getLine
      putStrLn $ "Got line!: " <> show line
      case decodeInput parseSQSRequest line of
        Right (fp, deckIds) -> do
          putStrLn $ "Got input!"
          putStrLn $ "deck ids: " <> concatMap (T.unpack . unDeckId) deckIds
          deployPresentation env
            (Username "nmattia")
            (Deckname "my-crazy-deck")
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
    forM records $ Aeson.withObject "record" $ \o' ->
      o' .: "body" >>= Aeson.parseJSON
