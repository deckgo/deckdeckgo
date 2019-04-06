{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Servant.Swagger.UI.Extended
  ( module Servant.Swagger.UI.Extended
  , module Servant.Swagger.UI
  ) where

import Data.Proxy
import GHC.TypeLits
import Control.Monad
import Data.Bifunctor (first)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Servant.Swagger as Servant
import Servant.Swagger.UI
import qualified Servant.Swagger.UI as SwaggerUI
import qualified System.Directory as Directory

-- | Dump the swagger schema and swagger-ui files to a directory.
swaggerSchemaUiDump
  :: forall dir api schema
  . (KnownSymbol dir, KnownSymbol schema, Servant.HasSwagger api)
  => FilePath -- ^ directory in which to write
  -> Proxy (SwaggerUI.SwaggerSchemaUI dir schema)
  -> Proxy api
  -> IO ()
swaggerSchemaUiDump outDir Proxy p = do
    let dir = symbolVal @dir Proxy
        schema = symbolVal @schema Proxy
        index = T.encodeUtf8 $
          T.replace "SERVANT_SWAGGER_UI_SCHEMA" (T.pack schema) $
          T.replace "SERVANT_SWAGGER_UI_DIR" (T.pack dir) $
          SwaggerUI.swaggerUiIndexTemplate
        swagger = Servant.toSwagger p
        -- The paths are prepended with '/' which confuses </>
        uiFiles = first (dropWhile (== '/')) <$> SwaggerUI.swaggerUiFiles
        prefix = case dir of
          "" -> outDir
          _ -> outDir </> dir
    let allFiles =
          [(outDir </> schema, BL.toStrict $ Aeson.encode swagger)] <>
          [(prefix </> "index.html", index)] <>
          (first (prefix </>) <$> uiFiles)

    forM_ allFiles $ \(path, content) -> do
      Directory.createDirectoryIfMissing
        True (FilePath.takeDirectory path)
      BS.writeFile path content
