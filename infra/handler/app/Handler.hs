{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Lens
import Servant.Auth.Firebase (ProjectId(..))
import UnliftIO
import qualified Data.ByteString.Char8 as BS8
import qualified DeckGo.Handler
import qualified Network.AWS as Aws
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Lambda as Lambda
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Hasql.Connection as Hasql
import System.Environment (getEnv)

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv Aws.Discover

    liftIO $ putStrLn "Booted!"

    -- TODO: from env
    let projectId = ProjectId "deckdeckgo-studio-beta"

    conn <- getPostgresqlConnection

    Lambda.run $ cors $ DeckGo.Handler.application (env ^. Aws.envManager) projectId env conn

-- TODO: factor out
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
