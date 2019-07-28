{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Extended
  ( module Network.AWS.Extended
  , module Network.AWS
  ) where

import Control.Lens
import Network.AWS hiding (newEnv)
import qualified Data.Text.Encoding as T
import qualified Network.AWS as AWS
import qualified Network.AWS.Data as Data
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.SQS as SQS

newEnv :: IO AWS.Env
newEnv = fixupEnv <$> AWS.newEnv AWS.Discover

-- | Transforms the request to hit the region-specific service, otherwise this
-- doesn't go through the VPC endpoint.
fixupEnv :: AWS.Env -> AWS.Env
fixupEnv = AWS.configure s3 . AWS.configure sqs
  where
    s3 = S3.s3
      { AWS._svcEndpoint = \reg -> do
          let new = "s3." <> Data.toText reg <> ".amazonaws.com"
          (AWS._svcEndpoint S3.s3 reg) & AWS.endpointHost .~ T.encodeUtf8 new
      }
    sqs = SQS.sqs
      { AWS._svcEndpoint = \reg -> do
          let new = "sqs." <> Data.toText reg <> ".amazonaws.com"
          (AWS._svcEndpoint SQS.sqs reg) & AWS.endpointHost .~ T.encodeUtf8 new
      }
