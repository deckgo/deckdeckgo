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
          { HTTPClient.managerModifyRequest = rerouteDynamoDB
          }
  env <- Aws.newEnv Aws.Discover <&> Aws.envManager .~ mgr
  Warp.run 8080 $ DeckGo.Handler.application env

rerouteDynamoDB :: HTTPClient.Request -> IO HTTPClient.Request
rerouteDynamoDB req =
    case HTTPClient.host req of
      "dynamodb.us-east-1.amazonaws.com" ->
        pure req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 8000
          , HTTPClient.secure = False
          }
      _ -> pure req
