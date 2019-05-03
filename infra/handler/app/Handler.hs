{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Servant.Auth.Firebase (ProjectId(..))
import UnliftIO
import qualified DeckGo.Handler
import qualified Network.AWS as Aws
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Lambda as Lambda
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Hasql.Connection as Hasql

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    liftIO $ putStrLn "Booting..."
    env <- Aws.newEnv Aws.Discover

    liftIO $ putStrLn "Booted!"

    -- TODO: from env
    let projectId = ProjectId "deckdeckgo-studio-beta"

    Right conn <- Hasql.acquire connectionSettings

    Lambda.run $ cors $ DeckGo.Handler.application (env ^. Aws.envManager) projectId env conn
  where
    connectionSettings = Hasql.settings "localhost" 5432 "nicolas" "" "nicolas"

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
