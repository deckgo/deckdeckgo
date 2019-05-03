{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import UnliftIO
import Control.Lens
import Servant.Auth.Firebase (ProjectId(..))
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClient
import qualified Network.AWS as Aws
import qualified DeckGo.Handler
import qualified Network.Wai.Handler.Warp as Warp
import qualified Hasql.Connection as Hasql
import System.Environment (getEnv)

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    mgr <- HTTPClient.newManager HTTPClient.tlsManagerSettings
            { HTTPClient.managerModifyRequest =
                pure . rerouteDynamoDB . rerouteGoogleApis
            }

    user <- getEnv "POSTGRES_USER"
    password <- getEnv "POSTGRES_PASSWORD"
    db <- getEnv "POSTGRES_DB"
    conn <- Hasql.acquire (
      Hasql.settings "localhost" 5432 (BS8.pack user) (BS8.pack password) (BS8.pack db)
      ) >>= \case
        Left e -> error (show e)
        Right c -> pure c

    env <- Aws.newEnv Aws.Discover <&> Aws.envManager .~ mgr
    let projectId = ProjectId "my-project-id"
    Warp.run 8080 $ DeckGo.Handler.application mgr projectId env conn

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

rerouteGoogleApis :: HTTPClient.Request -> HTTPClient.Request
rerouteGoogleApis req =
    case HTTPClient.host req of
      "www.googleapis.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 8081
          , HTTPClient.secure = False
          }
      _ -> req
