{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Proxy
import qualified Data.Swagger as Swagger
import qualified DeckGo.Handler
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant as Servant
import qualified Servant.Swagger as Servant
import qualified Servant.Swagger.UI.Extended as Servant

-- | API type with bells and whistles, i.e. schema file and swagger-ui.
type SwaggerAPI = Servant.SwaggerSchemaUI "" "swagger.json"

swaggerApi :: Proxy SwaggerAPI
swaggerApi = Proxy

main :: IO ()
main = serverSwagger

swagger :: Swagger.Swagger
swagger = Servant.toSwagger (Proxy :: Proxy DeckGo.Handler.SlidesAPI)

dumpSwagger :: FilePath -> IO ()
dumpSwagger out = Servant.swaggerSchemaUiDump out swaggerApi (Proxy :: Proxy DeckGo.Handler.SlidesAPI)

serverSwagger :: IO ()
serverSwagger =
  Warp.run 3000 $
    Servant.serve swaggerApi $
    Servant.swaggerSchemaUIServer swagger
