{-# LANGUAGE OverloadedStrings #-}

import UnliftIO
import Control.Lens
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClient
import qualified Network.AWS as Aws
import qualified DeckGo.Handler
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  mgr <- HTTPClient.newManager HTTPClient.tlsManagerSettings
          { HTTPClient.managerModifyRequest =
              pure . rerouteDynamoDB . rerouteGoogleApis
          }
  env <- Aws.newEnv Aws.Discover <&> Aws.envManager .~ mgr
  let projectId = DeckGo.Handler.FirebaseProjectId "my-project-id"
  Warp.run 8080 $ DeckGo.Handler.application mgr projectId env

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
