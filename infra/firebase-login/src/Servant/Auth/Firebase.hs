{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Auth.Firebase where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Data.Proxy
import Data.Word8 (isSpace, toLower)
import Servant.API
import qualified Crypto.JOSE.JWK as JWK
import qualified Network.URI as URI
import qualified Crypto.JWT as JWT
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMS
import qualified Data.PEM as PEM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.X509 as X509
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.Wai as Wai
import qualified Servant as Servant
import qualified Servant.Client.Core as Servant
import qualified Servant.Client.Core as Servant.Client
import qualified Servant.Server.Internal.RoutingApplication as Servant

data Protected

newtype ProjectId = ProjectId { unFirebaseProjectId :: T.Text }
data ServerContext = ServerContext { firebaseProjectId :: ProjectId }

newtype UserId = UserId { unUserId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Show, Eq)

newtype UnverifiedJWT = UnverifiedJWT JWT.SignedJWT

-- TODO: MAKE SURE PATTERN MATCH FAILURES AREN'T PROPAGATED TO CLIENT!!!
verifyUser :: HTTP.Manager -> ProjectId -> UnverifiedJWT -> IO UserId
verifyUser mgr (ProjectId projectId) (UnverifiedJWT jwt) = do

  -- TODO: proper error handling here
  let req =
        HTTP.setRequestSecure True .
        HTTP.setRequestHost "www.googleapis.com" .
        HTTP.setRequestPath "/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com" .
        HTTP.setRequestManager mgr $
        HTTP.defaultRequest
  jwkmap <- HTTP.getResponseBody <$> HTTP.httpJSON req

  t <- case jwt ^.. JWT.signatures . JWT.header . JWT.kid of
    [Just (JWT.HeaderParam () t)] -> pure t
    xs -> error $ "Expected exactly one signature with 'kid', got: " <> show xs

  jwkct <- case HMS.lookup t jwkmap of
    Nothing -> error $ "Could not find key " <> show t <> " in response"
    Just ct -> pure ct

  -- TODO: get rid of 'error'
  pem <- case PEM.pemParseBS (T.encodeUtf8 jwkct) of
    Left e -> error $ show e
    Right [e] -> pure e
    Right xs -> error $ show xs

  cert <- case X509.decodeSignedCertificate (PEM.pemContent pem) of
    Left e -> error $ show e
    Right c -> pure c

  jwk <- runExceptT (JWK.fromX509Certificate cert) >>= \case
    Left (e :: JWT.JWTError) -> error $ show e
    Right jwk -> pure jwk

  issUri <- case URI.parseURI $ "https://securetoken.google.com/" <> T.unpack projectId of
    Just issUri -> pure issUri
    Nothing -> error $ "Could not use project ID in URI"

  let config =
        JWT.defaultJWTValidationSettings
          (\sou -> Just projectId == sou ^? JWT.string) & -- aud
          JWT.issuerPredicate .~ (\sou -> Just issUri == sou ^? JWT.uri) -- iss
  runExceptT (JWT.verifyClaims config jwk jwt) >>= \case
    Right cs -> do
      case cs ^. JWT.claimSub of
        Nothing -> error "Could not get a subject from claim set"
        Just sou -> case sou ^? JWT.string of
          Nothing -> error "Expected subject to be string"
          Just u -> pure (UserId u)
    Left (e :: JWT.JWTError) -> error (show e)

instance FromHttpApiData UnverifiedJWT where
  parseUrlPiece = const $ Left "No support for JWT"
  parseHeader bs = case JWT.decodeCompact (BL.fromStrict bs) of
    Left (e :: JWT.Error) -> Left $ T.pack $ show e
    Right jwt -> Right $ UnverifiedJWT jwt

instance
    ( Servant.HasClient m sub
    , Servant.RunClient m ) => Servant.HasClient m (Protected :> sub) where
  -- TODO: something better than just Text
  type Client m (Protected :> sub) = T.Text -> Servant.Client m sub
  clientWithRoute p1 Proxy req = \bs ->
    Servant.clientWithRoute
      p1 (Proxy :: Proxy sub)
      (Servant.Client.addHeader "Authorization" ("Bearer " <> bs) req)
  hoistClientMonad p1 Proxy hoist c = \bs ->
    Servant.Client.hoistClientMonad p1 (Proxy :: Proxy sub) hoist (c bs)

-- | Find and decode an 'Authorization' header from the request as JWT
decodeJWTHdr :: Wai.Request -> Either String UnverifiedJWT
decodeJWTHdr req = do
    ah <- case lookup "Authorization" (Wai.requestHeaders req) of
      Just x -> Right x
      Nothing -> Left "No authorization header"
    let (b, rest) = BS.break isSpace ah
    guard (BS.map toLower b == "bearer")
    tok <- case snd <$> BS.uncons rest of
      Nothing -> Left "No token"
      Just x -> Right x
    case JWT.decodeCompact (BL.fromStrict tok) of
      Left (e :: JWT.Error) -> Left $ show e <> ": " <> show rest
      Right jwt -> Right (UnverifiedJWT jwt)

runJWTAuth :: HTTP.Manager -> ProjectId -> Wai.Request -> Servant.DelayedIO UserId
runJWTAuth mgr projectId req = case decodeJWTHdr req of
    Left e -> error $ "bad auth: " <> e -- TODO: delayedFailFatal
    Right ujwt -> liftIO $ verifyUser mgr projectId ujwt

instance
    ( Servant.HasContextEntry context ProjectId
    , Servant.HasContextEntry context HTTP.Manager
    , Servant.HasServer sub context
    ) => Servant.HasServer (Protected :> sub) context where
  type ServerT (Protected :> sub) m = UserId -> Servant.ServerT sub m

  route Proxy c subserver = do
      Servant.route (Proxy :: Proxy sub)
        c (subserver `Servant.addAuthCheck` authCheck)
    where
      authCheck :: Servant.DelayedIO UserId
      authCheck = Servant.withRequest $ runJWTAuth
        (Servant.getContextEntry c) (Servant.getContextEntry c)

  hoistServerWithContext Proxy p hoist s = \uid ->
    Servant.hoistServerWithContext (Proxy :: Proxy sub) p hoist (s uid)
