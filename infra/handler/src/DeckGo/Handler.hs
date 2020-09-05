{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DeckGo.Handler where

-- TODO: nullable slide content
-- TODO: feed API

-- TODO: check permissions
-- TODO: TTL on anonymous users
-- TODO: get rid of anonymous users

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Data.Aeson ((.=), (.:), (.!=), (.:?))
import Data.Bifunctor
import Data.Char
import Data.Int
import Data.List (find)
import Data.List (foldl')
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Swagger hiding (Tag)
import DeckGo.Prelude
import GHC.Generics
import Hasql.Statement (Statement(..))
import Servant (Context ((:.)))
import Servant.API
import Servant.Auth.Firebase (Protected)
import System.Environment
import System.FilePath
import UnliftIO
import qualified Cases
import qualified Codec.Archive.Zip as Zip
import qualified Crypto.Hash as Hash
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Hasql.Connection as HC
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Body as Body
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.SQS as SQS
import qualified Network.Mime as Mime
import qualified Network.Wai as Wai
import qualified Servant as Servant
import qualified Servant.Auth.Firebase as Firebase
import qualified System.Directory as Dir
import qualified System.IO.Temp as Temp
import qualified System.Random as Random
import qualified Text.HTML.TagSoup as TagSoup

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

-- COMMON

data Item id a = Item { itemId :: id, itemContent :: a }
  deriving (Show, Eq, Generic)

class ToJSONObject a where
  toJSONObject :: a -> Aeson.Object

instance (Aeson.ToJSON id, ToJSONObject a) => Aeson.ToJSON (Item id a) where
  toJSON i = Aeson.Object $
    HMS.fromList [ "id" .= itemId i ] <>
    toJSONObject (itemContent i)

class FromJSONObject a where
  parseJSONObject :: Aeson.Object -> Aeson.Parser a

instance (Aeson.FromJSON id, FromJSONObject a) => Aeson.FromJSON (Item id a) where
  parseJSON = Aeson.withObject "FromJSONObject" $ \o -> Item <$>
    o .: "id" <*> parseJSONObject o

-- USERS

type UsersAPI =
    Get '[JSON] [Item UserId User] :<|>
    Capture "user_id" UserId :> Get '[JSON] (Item UserId User) :<|>
    Protected :>
      ReqBody '[JSON] UserInfo :>
      Post '[JSON] (Item UserId User) :<|>
    Protected :>
      Capture "user_id" UserId :>
      ReqBody '[JSON] User :> Put '[JSON] (Item UserId User) :<|>
    Protected :> Capture "user_id" UserId :> Delete '[JSON] ()

newtype Username = Username { unUsername :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data UserInfo = UserInfo
  { userInfoFirebaseId :: FirebaseId
  , userInfoEmail :: Maybe T.Text
  } deriving (Show, Eq)

data User = User
  { userFirebaseId :: FirebaseId
  , userUsername :: Maybe Username -- + return anonymous
  } deriving (Show, Eq)

newtype UserId = UserId { unUserId :: FirebaseId }
  deriving newtype
    ( Aeson.FromJSON
    , Aeson.ToJSON
    , FromHttpApiData
    , ToHttpApiData
    , Show
    , Eq
    )
  deriving stock
    ( Generic )

newtype FirebaseId = FirebaseId { unFirebaseId :: T.Text }
  deriving newtype
    ( Aeson.FromJSON
    , Aeson.ToJSON
    , FromHttpApiData
    , ToHttpApiData
    , Show
    , Eq
    )
  deriving stock
    ( Generic )

-- XXX !!?!??!?!! pattern match failures are propagated to the client!!!
instance FromJSONObject UserInfo where
  parseJSONObject = \obj ->
    UserInfo
      <$> obj .: "firebase_uid"
      <*> (
        (do
          True <- obj .: "anonymous"
          (Nothing :: Maybe T.Text) <- obj .:? "email"
          pure Nothing
        ) <|> (do
          False <- obj .: "anonymous"
          obj .:? "email"
        )
        )

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" parseJSONObject

instance FromJSONObject User where
  parseJSONObject = \obj ->
    User
      <$> obj .: "firebase_uid"
      <*> (
        (do
          True <- obj .: "anonymous"
          (Nothing :: Maybe Username) <- obj .:? "username"
          pure Nothing
        ) <|> (do
          False <- obj .: "anonymous"
          obj .:? "username"
        )
        )

instance ToJSONObject UserInfo where
  toJSONObject uinfo = HMS.fromList
    [ "anonymous" .= isNothing (userInfoEmail uinfo)
    , "email" .= userInfoEmail uinfo
    , "firebase_uid" .= userInfoFirebaseId uinfo
    ]

instance Aeson.ToJSON User where
  toJSON = Aeson.Object . toJSONObject

instance Aeson.FromJSON UserInfo where
  parseJSON = Aeson.withObject "UserInfo" parseJSONObject

instance ToJSONObject User where
  toJSONObject user = HMS.fromList
    [ "anonymous" .= isNothing (userUsername user)
    , "username" .= userUsername user
    , "firebase_uid" .= userFirebaseId user
    ]

instance Aeson.ToJSON UserInfo where
  toJSON = Aeson.Object . toJSONObject

instance ToSchema (Item UserId User) where
  declareNamedSchema _ = pure $ NamedSchema (Just "UserWithId") mempty

instance ToSchema PresResponse where
  declareNamedSchema _ = pure $ NamedSchema (Just "PresResponse") mempty

instance ToSchema User where
  declareNamedSchema _ = pure $ NamedSchema (Just "User") mempty

instance ToParamSchema (Item UserId User) where
  toParamSchema _ = mempty

instance ToParamSchema UserId where
  toParamSchema _ = mempty

instance ToSchema UserInfo where
  declareNamedSchema _ = pure $ NamedSchema (Just "UserInfo") mempty

instance ToParamSchema (Item UserId UserInfo) where
  toParamSchema _ = mempty

newtype PresentationName = PresentationName { unPresentationName :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

newtype PresentationBackground = PresentationBackground { unPresentationBackground :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

newtype PresentationHeader = PresentationHeader { unPresentationHeader :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

newtype PresentationFooter = PresentationFooter { unPresentationFooter :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

-- SLIDES

instance ToSchema Slide where
  declareNamedSchema _ = pure $ NamedSchema (Just "Slide") mempty

data Slide = Slide
  { slideContent :: Maybe T.Text
  , slideTemplate :: T.Text
  , slideAttributes :: HMS.HashMap T.Text T.Text
  } deriving (Show, Eq)

instance FromJSONObject Slide where
  parseJSONObject = \obj ->
    Slide <$>
      obj .:? "content" .!= Nothing <*>
      obj .: "template" <*>
      obj .:? "attributes" .!= HMS.empty

instance ToJSONObject Slide where
  toJSONObject slide = HMS.fromList
    [ "template" .= slideTemplate slide
    , "attributes" .= slideAttributes slide
    , "content" .= slideContent slide
    ]

instance Aeson.FromJSON Slide where
  parseJSON = Aeson.withObject "Slide" parseJSONObject
instance Aeson.ToJSON Slide where
  toJSON = Aeson.Object . toJSONObject

-- PRESENTATIONS

type PresentationsAPI =
    Protected :>
      ReqBody '[JSON] PresentationInfo :>
      Post '[JSON] (Item PresentationId PresentationResult) :<|>
    Protected :>
      Capture "presentation_id" PresentationId :>
      ReqBody '[JSON] PresentationInfo :>
      Put '[JSON] (Item PresentationId PresentationResult)

instance ToSchema (Item PresentationId PresentationResult) where
  declareNamedSchema _ = pure $ NamedSchema (Just "PresentationWithId") mempty

instance ToSchema PresentationInfo where
  declareNamedSchema _ = pure $ NamedSchema (Just "Presentation") mempty

-- instance ToParamSchema (Item PresentationId Presentation) where
  -- toParamSchema _ = mempty

newtype PresentationId = PresentationId { unPresentationId :: T.Text }
  deriving newtype
    ( Aeson.FromJSON
    , Aeson.ToJSON
    , FromHttpApiData
    , ToHttpApiData
    , Show
    , Eq
    )
  deriving stock
    ( Generic )

instance ToParamSchema PresentationId

data PresentationInfo = PresentationInfo
  { presentationName :: PresentationName
  , presentationOwner :: UserId
  , presentationBackground :: Maybe PresentationBackground
  , presentationHeader :: Maybe PresentationHeader
  , presentationFooter :: Maybe PresentationFooter
  , presentationAttributes :: HMS.HashMap T.Text T.Text
  , presentationSlides :: [Slide]
  , presentationDescription :: T.Text
  , presentationHeadExtra :: Maybe T.Text
  } deriving (Show, Eq)

data PresentationResult = PresentationResult
  { presentationUrl :: T.Text
  } deriving (Show, Eq)

instance ToJSONObject PresentationInfo where
  toJSONObject = undefined -- \obj ->

instance FromJSONObject PresentationInfo where
  parseJSONObject = \obj ->
    PresentationInfo <$>
      obj .: "name" <*>
      obj .: "owner_id" <*>
      obj .:? "background" <*>
      obj .:? "header" <*>
      obj .:? "footer" <*>
      obj .:? "attributes" .!= HMS.empty <*>
      obj .: "slides" <*>
      obj .: "description" <*>
      obj .:? "head_extra"

instance FromJSONObject PresentationResult where
  parseJSONObject = undefined

instance ToJSONObject PresentationResult where
  toJSONObject pres = HMS.fromList
    [ "url" .= presentationUrl pres
    ]

instance Aeson.ToJSON PresentationInfo where
  toJSON = Aeson.Object . toJSONObject
instance Aeson.FromJSON PresentationInfo where
  parseJSON = Aeson.withObject "Presentation" parseJSONObject
instance Aeson.ToJSON PresentationResult where
  toJSON = Aeson.Object . toJSONObject
instance Aeson.FromJSON PresentationResult where
  parseJSON = Aeson.withObject "Presentation" parseJSONObject

type API = "api" :> (
    "users" :> UsersAPI :<|>
    "presentations" :> PresentationsAPI
    )

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

application
  :: Firebase.FirebaseLoginSettings
  -> AWS.Env
  -> HC.Connection
  -> Wai.Application
application settings env conn =
    Servant.serveWithContext
      api
      (settings :. Servant.EmptyContext)
      (server env conn)

server :: AWS.Env -> HC.Connection -> Servant.Server API
server env conn =
    serveUsers :<|>
    servePresentations
  where
    serveUsers =
      usersGet conn :<|>
      usersGetUserId conn :<|>
      usersPost conn :<|>
      usersPut conn :<|>
      usersDelete conn
    servePresentations =
      presentationsPost env conn :<|>
      presentationsPut env conn

presentationsPost
  :: AWS.Env -> HC.Connection
  -> Firebase.UserId
  -> PresentationInfo
  -> Servant.Handler (Item PresentationId PresentationResult)
presentationsPost env conn _userId pinfo = do
    liftIO $ putStrLn $ unwords
      [ "POST presentation"
      , show pinfo
      , show _userId
      ]

    iface <- liftIO $ getDbInterface conn
    user <- liftIO (dbGetUserById iface (presentationOwner pinfo)) >>= \case
      Nothing -> do
        liftIO $ putStrLn "Presentation user does not exist in DB"
        Servant.throwError Servant.err404
      Just u -> pure u

    let userId = presentationOwner pinfo

    uname <- case userUsername (itemContent user) of
      Nothing -> do
        liftIO $ putStrLn "User is anonymous"
        Servant.throwError Servant.err422
      Just uname -> pure uname

    liftIO $ putStrLn $ "Got user: " <> show uname

    let psname = sanitizePresentationName (presentationName pinfo)

    liftIO $ deployPresentation env uname psname pinfo

    presId <- liftIO $ PresentationId <$> newId

    presUrl <- do
      purl <- liftIO (getEnv "DECKGO_PRESENTATIONS_URL")
      pure $ mconcat
        [ "https://"
        , T.pack purl
        , "/"
        , unPresentationPrefix $
            presentationPrefix uname psname
        ]

    liftIO $ putStrLn $ unwords
      [ "Presentation info:"
      , show (presUrl, presId, psname)
      ]

    -- TODO: make unique
    liftIO (dbCreatePresentation iface presId psname presUrl userId)

    pure $ Item
      { itemId = presId
      , itemContent = PresentationResult { presentationUrl = presUrl }
      }

presentationsPut
  :: AWS.Env -> HC.Connection
  -> Firebase.UserId
  -> PresentationId
  -> PresentationInfo
  -> Servant.Handler (Item PresentationId PresentationResult)
presentationsPut env conn _uid pid pinfo = do
    liftIO $ putStrLn $ "PUT PRESENTATION" <> show pid
    liftIO $ putStrLn $ unwords
      [ "PUT presentation"
      , show pinfo
      , "(" <> show pid <> ")"
      , show _uid
      ]

    liftIO $ print pinfo
    iface <- liftIO $ getDbInterface conn

    user <- liftIO (dbGetUserById iface (presentationOwner pinfo)) >>= \case
      Nothing -> do
        liftIO $ putStrLn "Presentation user does not exist in DB"
        Servant.throwError Servant.err404
      Just u -> pure u

    uname <- case userUsername (itemContent user) of
      Nothing -> do
        liftIO $ putStrLn "User is anonymous"
        Servant.throwError Servant.err422
      Just uname -> pure uname

    liftIO $ putStrLn $ "Got user: " <> show uname

    (presName, presUrl) <- liftIO (dbGetPresentationById iface pid) >>= \case
      Nothing -> do
        liftIO $ putStrLn "Presentation does not exist in DB"
        Servant.throwError Servant.err404
      Just res -> pure res

    liftIO $ putStrLn $ "Got presentation: " <> show (presName, presUrl)

    liftIO $ deployPresentation env uname presName pinfo

    presUrl' <- do
      purl <- liftIO (getEnv "DECKGO_PRESENTATIONS_URL")
      pure $ mconcat
        [ "https://"
        , T.pack purl
        , "/"
        , unPresentationPrefix $
            presentationPrefix uname presName
        ]

    liftIO $ putStrLn $ "Replying with url: " <> show presUrl'

    pure $ Item
      { itemId = pid
      , itemContent = PresentationResult { presentationUrl = presUrl' }
      }

-- USERS

usersGet :: MonadIO io => HC.Connection -> io [Item UserId User]
usersGet conn = do
    iface <- liftIO $ getDbInterface conn
    liftIO $ dbGetAllUsers iface -- TODO: to Servant err500 on error

usersGetSession :: HS.Session [Item UserId User]
usersGetSession = do
    HS.statement () usersGetStatement

usersGetStatement :: Statement () [Item UserId User]
usersGetStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT account.id, account.firebase_id, account.username"
      ,   "FROM account"
      ]
    encoder = HE.unit
    decoder = HD.rowList $
      Item <$>
        ((UserId . FirebaseId) <$> HD.column HD.text) <*>
        ( User <$>
          (FirebaseId <$> HD.column HD.text) <*>
          HD.nullableColumn (Username <$> HD.text)
        )

usersGetUserId :: HC.Connection -> UserId -> Servant.Handler (Item UserId User)
usersGetUserId conn userId = do
    iface <- liftIO $ getDbInterface conn
    liftIO (dbGetUserById iface userId) >>= \case
      Nothing -> Servant.throwError Servant.err404
      Just u -> pure u

usersGetUserIdSession :: UserId -> HS.Session (Maybe (Item UserId User))
usersGetUserIdSession userId = do
    HS.statement userId usersGetUserIdStatement

usersGetUserIdStatement :: Statement UserId (Maybe (Item UserId User))
usersGetUserIdStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT account.id, account.firebase_id, account.username"
      ,   "FROM account"
      ,   "WHERE account.id = $1"
      ]
    encoder = contramap
        (unFirebaseId . unUserId)
        (HE.param HE.text)
    decoder = HD.rowMaybe $
      Item <$>
        ((UserId . FirebaseId) <$> HD.column HD.text) <*>
        ( User <$>
          (FirebaseId <$> HD.column HD.text) <*>
          HD.nullableColumn (Username <$> HD.text)
        )

usersPost
  :: HC.Connection
  -> Firebase.UserId
  -> UserInfo
  -> Servant.Handler (Item UserId User)
usersPost conn fuid uinfo = do

    when (Firebase.unUserId fuid /= unFirebaseId (userInfoFirebaseId uinfo)) $ do
      Servant.throwError Servant.err403

    iface <- liftIO $ getDbInterface conn
    liftIO $ putStrLn "got DB interface"

    let userId = UserId (userInfoFirebaseId uinfo)
    user <- case userInfoToUser uinfo of
      Left e -> Servant.throwError Servant.err400
        { Servant.errBody = BL.fromStrict $ T.encodeUtf8 e }
      Right user -> pure user
    liftIO (dbCreateUser iface userId user) >>= \case
      Left () ->
          Servant.throwError $ Servant.err409
            { Servant.errBody = Aeson.encode (Item userId user) }
      Right user' -> pure $ Item userId user'

userInfoToUser :: UserInfo -> Either T.Text User
userInfoToUser uinfo = User <$>
    pure (userInfoFirebaseId uinfo) <*>
    (traverse emailToUsername (userInfoEmail uinfo))

emailToUsername :: T.Text -> Either T.Text Username
emailToUsername t = case T.breakOn "@" t of
    ("", _) -> Left ("Invalid email: " <> t)
    (out', _) -> case dropBadChars (T.toLower out') of
      "" -> Left ("No valid char found: " <> out')
      out -> Right $ Username out
  where
    dropBadChars :: T.Text -> T.Text
    dropBadChars = T.concatMap
      $ \case
        c | isAscii c && isAlphaNum c -> T.singleton c
          | otherwise -> ""

usersPostSession :: UserId -> User -> HS.Session (Either () User)
usersPostSession uid u = do
    HS.sql "BEGIN"
    liftIO $ putStrLn "Creating user in DB"
    HS.statement (uid,u) usersPostStatement >>= \case
      1 -> do
        liftIO $ putStrLn "User was created"
        case userUsername u of
          Just uname -> do
            liftIO $ putStrLn "Creating username"
            let success unam = do
                    liftIO $ putStrLn "User created successfully"
                    HS.sql "COMMIT"
                    pure $ Right $ u { userUsername = Just unam }
            HS.statement (uname, uid) usersPostStatement' >>= \case
              1 -> success uname
              _ -> do
                liftIO $ putStrLn "Couldn't create username"
                rand <- liftIO $ randomText 4 ['0' .. '9' ]
                let uname' = Username $ unUsername uname <> rand
                liftIO $ putStrLn $ "Retrying with username " <> (T.unpack $ unUsername uname')
                HS.statement (uname', uid) usersPostStatement' >>= \case
                  1 -> success uname'
                  _ -> do
                    liftIO $ putStrLn "Couldn't create username again"
                    HS.sql "ROLLBACK"
                    pure $ Left ()
          Nothing -> do
            liftIO $ putStrLn "No username"
            HS.sql "COMMIT"
            pure $ Right u
      _ -> do
        liftIO $ putStrLn "Couldn't create exactly one user"
        HS.sql "ROLLBACK"
        pure $ Left ()

usersPostStatement :: Statement (UserId, User) Int64
usersPostStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "INSERT INTO account"
      ,   "(id, firebase_id, username)"
      ,   "VALUES ($1, $2, NULL)"
      ,   "ON CONFLICT DO NOTHING"
      ]
    encoder =
      contramap
        (unFirebaseId . unUserId . view _1)
        (HE.param HE.text) <>
      contramap (unFirebaseId . userFirebaseId . view _2) (HE.param HE.text)
    decoder = HD.rowsAffected

usersPostStatement' :: Statement (Username, UserId) Int64
usersPostStatement' = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "UPDATE account SET username = $1 WHERE id = $2 AND NOT EXISTS (SELECT 1 FROM account WHERE username = $1)" ]
    encoder =
      contramap
        (unUsername . view _1)
        (HE.param HE.text) <>
      contramap
        (unFirebaseId . unUserId . view _2)
        (HE.param HE.text)
    decoder = HD.rowsAffected

usersPostStatement'' :: Statement Username () -- TODO: check was deleted?
usersPostStatement'' = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "UPDATE account SET username = NULL WHERE id = $1"
      ]
    encoder =
      contramap
        (unUsername)
        (HE.param HE.text)
    decoder = HD.unit

usersPut
  :: HC.Connection
  -> Firebase.UserId
  -> UserId
  -> User
  -> Servant.Handler (Item UserId User)
usersPut conn fuid userId user = do

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId userId)) $ do
      liftIO $ putStrLn $ unwords
        [ "User is trying to update another uinfo:", show (fuid, userId, user) ]
      Servant.throwError Servant.err404

    when (Firebase.unUserId fuid /= unFirebaseId (userFirebaseId user)) $ do
      liftIO $ putStrLn $ unwords
        [ "Client used the wrong uinfo ID on uinfo", show (fuid, userId, user) ]
      Servant.throwError Servant.err400

    iface <- liftIO $ getDbInterface conn
    liftIO (dbUpdateUser iface userId user) >>= \case
      UserUpdateOk -> pure $ Item userId user -- TODO: check # of affected rows
      e -> do -- TODO: handle not found et al.
        liftIO $ print e
        Servant.throwError Servant.err400

data UserUpdateResult
  = UserUpdateOk
  | UserUpdateNotExist
  | UserUpdateClash
  deriving Show

usersPutSession :: UserId -> User -> HS.Session UserUpdateResult
usersPutSession uid u = do
    HS.sql "BEGIN"
    usersGetUserIdSession uid >>= \case
      Nothing -> do
        HS.sql "ROLLBACK"
        pure UserUpdateNotExist

      -- XXX: no handling of updating firebase id
      Just (Item _ oldUser) -> case (userUsername oldUser, userUsername u) of
        (Nothing, Nothing) -> do
          HS.sql "ROLLBACK" -- doesn't matter if rollback or commit
          pure UserUpdateOk
        (oldUname, newUname) -> do
          case oldUname of
            Nothing -> pure ()
            Just uname -> do
              HS.statement uname usersPostStatement''
          case newUname of
            Nothing -> do
              HS.sql "COMMIT"
              pure UserUpdateOk
            Just uname -> do
              HS.statement (uname, uid) usersPostStatement' >>= \case
                1 -> do
                  HS.sql "COMMIT"
                  pure UserUpdateOk
                _ -> do
                  HS.sql "ROLLBACK"
                  pure UserUpdateClash

usersDelete :: HC.Connection -> Firebase.UserId -> UserId -> Servant.Handler ()
usersDelete conn fuid userId = do

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId userId)) $ do
      Servant.throwError Servant.err403

    iface <- liftIO $ getDbInterface conn
    liftIO (dbDeleteUser iface userId) >>= \case
      Left () -> Servant.throwError Servant.err404
      Right () -> pure ()

usersDeleteSession :: UserId -> HS.Session ()
usersDeleteSession uid = do
    HS.statement uid usersDeleteStatement

usersDeleteStatement :: Statement UserId ()
usersDeleteStatement = Statement sql encoder decoder True
  where
    sql = "DELETE FROM account WHERE id = $1"
    encoder =
      contramap
        (unFirebaseId . unUserId)
        (HE.param HE.text)
    decoder = HD.unit -- TODO: affected rows

-- PRESENTATIONS


presentationsPostSession :: PresentationId -> PresShortname -> T.Text -> UserId -> HS.Session ()
presentationsPostSession pid pnam purl uid = do
    liftIO $ putStrLn "Creating presentation in DB"
    HS.statement (pid, pnam, purl, uid) presentationsPostStatement

presentationsPostStatement :: Statement (PresentationId, PresShortname, T.Text, UserId) ()
presentationsPostStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "INSERT INTO presentation"
      ,   "(id, name, url, owner)"
      ,   "VALUES ($1, $2, $3, $4)"
      ]
    encoder =
      contramap (unPresentationId . view _1) (HE.param HE.text) <>
      contramap (unPresShortname . view _2) (HE.param HE.text) <>
      contramap (view _3) (HE.param HE.text) <>
      contramap
        (unFirebaseId . unUserId . view _4)
        (HE.param HE.text)
    decoder = HD.unit

presentationsGetByIdSession :: PresentationId -> HS.Session (Maybe (PresShortname, T.Text))
presentationsGetByIdSession pid = do
    liftIO $ putStrLn $ "Getting presentation by id"
    HS.statement pid presentationsGetByIdStatement

presentationsGetByIdStatement :: Statement PresentationId (Maybe (PresShortname, T.Text))
presentationsGetByIdStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT name, url, owner FROM presentation"
      ,   "WHERE id = $1"
      ]
    encoder = contramap unPresentationId (HE.param HE.text)
    decoder = HD.rowMaybe $ (,) <$>
      (PresShortname <$> HD.column HD.text) <*>
      HD.column HD.text -- <*>
      -- TODO: return user ID

data PresResponse = PresResponse T.Text

instance Aeson.ToJSON PresResponse where
  toJSON (PresResponse t) = Aeson.object [ "url" .= t ]

instance Aeson.FromJSON PresResponse where
  parseJSON = Aeson.withObject "pres-response" $ \o ->
    PresResponse <$> o .: "url"

-------------------------------------------------------------------------------
-- DATABASE
-------------------------------------------------------------------------------

data DbInterface = DbInterface
  { dbGetAllUsers :: IO [Item UserId User]
  , dbGetUserById :: UserId -> IO (Maybe (Item UserId User))
  , dbCreateUser :: UserId -> User -> IO (Either () User)
  , dbUpdateUser :: UserId -> User -> IO UserUpdateResult
  , dbDeleteUser :: UserId -> IO (Either () ())

  -- TODO: dbCreateSlide: if duplicated, no error !?
  , dbCreatePresentation :: PresentationId -> PresShortname -> T.Text -> UserId -> IO ()
  , dbGetPresentationById :: PresentationId -> IO (Maybe (PresShortname, T.Text))
  }

data DbVersion
  = DbVersion0
  | DbVersion1
  | DbVersion2
  | DbVersion3
  | DbVersion4
  | DbVersion5
  | DbVersion6
  | DbVersion7
  deriving stock (Enum, Bounded, Ord, Eq)

-- | Migrates from ver to latest
migrateFrom :: DbVersion -> HS.Session ()
migrateFrom = \frm -> do
    liftIO $ putStrLn $ "Migration: " <>
      show (dbVersionToText <$> [frm ..maxBound])
    HS.sql "BEGIN"
    forM_ [frm .. maxBound] $ \ver -> do
      migrateTo ver
      HS.statement (dbVersionToText ver) $ Statement
        (BS8.unwords
          [ "INSERT INTO db_meta (key, value) VALUES ('version', $1)"
          , "ON CONFLICT (key) DO UPDATE SET value = $1"
          ]
        ) (HE.param HE.text) HD.unit True
    HS.sql "COMMIT"
  where
    -- | Migrates from (ver -1) to ver
    migrateTo :: DbVersion -> HS.Session ()
    migrateTo = \case
      DbVersion0 -> do
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE account ("
            ,   "id TEXT UNIQUE,"
            ,   "firebase_id TEXT UNIQUE,"
            ,   "anonymous BOOLEAN"
            , ");"
            ]
          ) HE.unit HD.unit True
      DbVersion1 -> do
        HS.statement () $ Statement
          (BS8.unwords
            [ "DROP TABLE IF EXISTS account CASCADE"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE account ("
            ,   "id TEXT UNIQUE NOT NULL,"
            ,   "firebase_id TEXT UNIQUE NOT NULL"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE username ("
            ,   "id TEXT UNIQUE NOT NULL,"
            ,   "account TEXT REFERENCES account (id) ON DELETE CASCADE UNIQUE NOT NULL"
            , ");"
            ]
          ) HE.unit HD.unit True
      DbVersion2 -> do
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE slide ("
            ,   "id TEXT UNIQUE NOT NULL,"
            ,   "content TEXT,"
            ,   "template TEXT,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
      DbVersion3 -> do
        HS.sql "DROP TABLE IF EXISTS username"
        HS.sql "DROP TABLE IF EXISTS account CASCADE"
        HS.sql "DROP TABLE IF EXISTS slide"
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE account ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "firebase_id TEXT UNIQUE,"
            ,   "username TEXT UNIQUE NULL"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE deck ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "name TEXT NOT NULL,"
            ,   "background TEXT NULL,"
            ,   "owner TEXT NOT NULL REFERENCES account (id),"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE slide ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "deck TEXT NOT NULL REFERENCES deck (id) ON DELETE CASCADE,"
            ,   "index INT2 NULL,"
            ,   "content TEXT," -- TODO: is any of this nullable?
            ,   "template TEXT,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
      DbVersion4 -> do
        HS.sql "DROP TABLE IF EXISTS username"
        HS.sql "DROP TABLE IF EXISTS account CASCADE"
        HS.sql "DROP TABLE IF EXISTS slide"
        HS.sql "DROP TABLE IF EXISTS deck"
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE account ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "firebase_id TEXT UNIQUE,"
            ,   "username TEXT UNIQUE NULL"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE deck ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "name TEXT NOT NULL,"
            ,   "background TEXT NULL,"
            ,   "owner TEXT NOT NULL REFERENCES account (id),"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE slide ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "deck TEXT NOT NULL REFERENCES deck (id) ON DELETE CASCADE,"
            ,   "index INT2 NULL,"
            ,   "content TEXT," -- TODO: is any of this nullable?
            ,   "template TEXT,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
      DbVersion5 -> do
        HS.sql "DROP TABLE IF EXISTS username"
        HS.sql "DROP TABLE IF EXISTS account CASCADE"
        HS.sql "DROP TABLE IF EXISTS slide"
        HS.sql "DROP TABLE IF EXISTS deck"
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE account ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "firebase_id TEXT UNIQUE,"
            ,   "username TEXT UNIQUE NULL"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE deck ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "name TEXT NOT NULL,"
            ,   "background TEXT NULL,"
            ,   "owner TEXT NOT NULL REFERENCES account (id) ON DELETE CASCADE,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE slide ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "deck TEXT NOT NULL REFERENCES deck (id) ON DELETE CASCADE,"
            ,   "index INT2 NULL,"
            ,   "content TEXT," -- TODO: is any of this nullable?
            ,   "template TEXT,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
      DbVersion6 -> do
        HS.sql "DROP TABLE IF EXISTS username"
        HS.sql "DROP TABLE IF EXISTS account CASCADE"
        HS.sql "DROP TABLE IF EXISTS slide"
        HS.sql "DROP TABLE IF EXISTS deck"
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE account ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "firebase_id TEXT UNIQUE,"
            ,   "username TEXT UNIQUE NULL"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE deck ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "name TEXT NOT NULL,"
            ,   "background TEXT NULL,"
            ,   "owner TEXT NOT NULL REFERENCES account (id) ON DELETE CASCADE,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE slide ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "deck TEXT NOT NULL REFERENCES deck (id) ON DELETE CASCADE,"
            ,   "index INT2 NULL,"
            ,   "content TEXT," -- TODO: is any of this nullable?
            ,   "template TEXT,"
            ,   "attributes JSON"
            , ");"
            ]
          ) HE.unit HD.unit True
      -- TODO: unique on (owner, name)
      -- TODO: drop deck, slide tables
      DbVersion7 -> do
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE presentation ("
            ,   "id TEXT PRIMARY KEY,"
            ,   "name TEXT NOT NULL,"
            ,   "url TEXT NOT NULL,"
            ,   "owner TEXT NOT NULL REFERENCES account (id) ON DELETE CASCADE"
            , ");"
            ]
          ) HE.unit HD.unit True

readDbVersion :: HS.Session (Either String (Maybe DbVersion))
readDbVersion = do
    res <- HS.statement () $ Statement
      (BS8.unwords
        [ "SELECT EXISTS ("
        ,   "SELECT 1"
        ,   "FROM   information_schema.tables"
        ,   "WHERE  table_schema = 'public'"
        ,   "AND    table_name = 'db_meta'"
        ,   ");"
        ]
      ) HE.unit (HD.singleRow (HD.column HD.bool)) True
    case res of
      True -> do
        mtver <- HS.statement () $ Statement
          "SELECT value FROM db_meta WHERE key = 'version'"
          HE.unit (HD.rowMaybe (HD.column HD.text)) True
        case mtver of
          Nothing -> pure (Right Nothing)
          Just tver -> case dbVersionFromText tver of
            Nothing -> error $ "could not decode DB version: " <> T.unpack tver
            Just ver -> pure (Right $ Just ver)
      False -> do
        HS.statement () $ Statement
          (BS8.unwords
            [ "CREATE TABLE db_meta ("
            ,   "key TEXT UNIQUE NOT NULL,"
            ,   "value TEXT NOT NULL"
            ,   ");"
            ]
          ) HE.unit HD.unit True
        pure $ Right Nothing

dbVersionToText :: DbVersion -> T.Text
dbVersionToText = \case
  DbVersion0 -> "0"
  DbVersion1 -> "1"
  DbVersion2 -> "2"
  DbVersion3 -> "3"
  DbVersion4 -> "4"
  DbVersion5 -> "5"
  DbVersion6 -> "6"
  DbVersion7 -> "7"

dbVersionFromText :: T.Text -> Maybe DbVersion
dbVersionFromText t =
    find (\ver -> dbVersionToText ver == t) [minBound .. maxBound]

migrate :: HS.Session ()
migrate = do
    readDbVersion >>= \case
      Left e -> error $ show e
      Right Nothing -> do
        liftIO $ putStrLn "Migrating from beginning"
        migrateFrom minBound
      Right (Just v) ->
        if
          | v == maxBound -> pure ()
          | v > maxBound -> error "V greater than maxbound"
          | v < maxBound -> migrateFrom (succ v)
          | True -> error $ unwords
              [ "impossible:"
              , "version must be equal to,"
              , "greater than or smaller than maxBound."
              ]

getDbInterface :: HC.Connection -> IO DbInterface
getDbInterface conn = do
    HS.run migrate conn >>= \case
      Left e -> error $ show e
      Right () -> pure ()

    pure $ DbInterface
      { dbGetAllUsers = wrap usersGetSession
      , dbGetUserById = \uid -> wrap (usersGetUserIdSession uid)
      , dbCreateUser = \uid uinfo -> wrap (usersPostSession uid uinfo)
      , dbUpdateUser = \uid uinfo -> wrap (usersPutSession uid uinfo)

      -- TODO: proper return type on delete
      , dbDeleteUser = \uid -> Right <$> wrap (usersDeleteSession uid)

      , dbCreatePresentation = \pid pname purl uid  -> wrap (presentationsPostSession pid pname purl uid)
      , dbGetPresentationById = \pid -> wrap (presentationsGetByIdSession pid)
      }
  where
    wrap :: forall b. HS.Session b -> IO b
    wrap act = HS.run act conn >>= \case
      Left e -> error $ "getDbInterface: error: " <> show e -- TODO
      Right x -> pure x

-- AUX

runAWS :: MonadIO m => AWS.Env -> AWS.AWS a -> m (Either SomeException a)
runAWS env =
    liftIO .
    tryAny .
    AWS.runResourceT .
    AWS.runAWS env .
    AWS.within AWS.NorthVirginia

randomString :: Int -> [Char] -> IO String
randomString len allowedChars =
  replicateM len $ do
    idx <- Random.randomRIO (0, length allowedChars - 1)
    pure $ allowedChars !! idx

randomText :: Int -> [Char] -> IO T.Text
randomText len allowedChars = T.pack <$> randomString len allowedChars

newId :: IO T.Text
newId = randomText 32 (['0' .. '9'] <> ['a' .. 'z'])

tshow :: Show a => a -> T.Text
tshow = T.pack . show

newtype PresentationPrefix = PresentationPrefix { unPresentationPrefix :: T.Text }

-- TODO: what happens when the deckname is "-" ?
presentationPrefix :: Username -> PresShortname -> PresentationPrefix
presentationPrefix uname psname =
    PresentationPrefix $
      unUsername uname <> "/" <> unPresShortname psname <> "/"

sanitizePresentationName :: PresentationName -> PresShortname
sanitizePresentationName = PresShortname . T.toLower . strip . dropBadChars . unPresentationName
  where
    strip :: T.Text -> T.Text
    strip = T.dropAround ( == '-' )
    dropBadChars :: T.Text -> T.Text
    dropBadChars = T.concatMap
      $ \case
        c | isAscii c && isAlphaNum c -> T.singleton c
          | c == '-' -> T.singleton c
          | c == ' ' -> T.singleton '-'
          | otherwise -> ""

data Err = Err T.Text SomeException
  deriving (Show, Exception)

err :: T.Text -> SomeException -> IO a
err msg e = throwIO $ Err msg e

-- | Diffs the bucket objects.
-- Returns:
--  * fst: the files to add
--  * snd: the files to delete
diffObjects
  :: [(FilePath, S3.ObjectKey, S3.ETag)]
  -> [(S3.ObjectKey, S3.ETag)]
  -> ([(FilePath, S3.ObjectKey, S3.ETag)], [S3.ObjectKey])
diffObjects news0 (HMS.fromList -> olds0) = second HMS.keys $
    foldl' (
      \(news, olds) obj@(_fp, okey, etag) -> do
          case HMS.lookup okey olds of
            Nothing -> (obj : news, olds)
            Just etag' ->
              if etag' == etag
              then (news, HMS.delete okey olds)
              else (obj : news, olds)
      ) ([], olds0) news0

listPresentationObjects
  :: AWS.Env
  -> S3.BucketName
  -> PresentationPrefix
  -> IO [S3.Object]
listPresentationObjects env bucket pprefix =
    listObjects env bucket (Just $ unPresentationPrefix pprefix)

zipExtract :: FilePath -> FilePath -> IO ()
zipExtract dest archivePath = do
    archive <- Zip.toArchive <$> BL.readFile archivePath
    Zip.extractFilesFromArchive [ Zip.OptDestination dest ] archive

withPresentationFiles
  :: Username
  -> PresShortname
  -> PresentationInfo
  -> ([(FilePath, S3.ObjectKey, S3.ETag)] -> IO a)
  -> IO a
withPresentationFiles uname psname presentationInfo act = do
    deckgoStarterDist <- getEnv "DECKGO_STARTER_DIST"
    Temp.withSystemTempDirectory "dist" $ \dir -> do
      zipExtract dir deckgoStarterDist

      -- Here we deal with workbox' precache by updating index.html and then
      -- propagate the new etag (md5) through precache-manifest.js and
      -- service-worker.js.

      oldIndexMd5 <- fileETag $ dir </> "index.html"
      let serviceWorkerFile = dir </> "service-worker.js"
      mapFile processIndex $ dir </> "index.html"
      newIndexMd5 <- fileETag $ dir </> "index.html"
      putStrLn $ "Changing index.html MD5 from " <>
        show oldIndexMd5 <> " to " <> show newIndexMd5
      mapFile (T.replace oldIndexMd5 newIndexMd5) $ dir </> serviceWorkerFile

      mapFile interpol $ dir </> "manifest.json"
      putStrLn "Listing files..."
      files <- listDirectoryRecursive dir
      files' <- forM files $ \(fp, components) -> do
        etag <- fileETag fp
        let okey = mkObjectKey uname psname components
        pure (fp, okey, etag)
      act files'
  where
    pname = presentationName presentationInfo
    processIndex :: T.Text -> T.Text
    processIndex input =
      let content =
            interpol . TagSoup.renderTags . processTags presentationInfo . TagSoup.parseTags $ input
          scriptShas = (pad . sha256digest) <$> (findScripts content)
          pad = \x -> "'sha256-" <> x <> "'"
      in T.replace "{{DECKDECKGO_EXTRA_SHAS}}" (T.intercalate " " scriptShas) content
    interpol =
      T.replace "{{DECKDECKGO_TITLE}}" (unPresentationName pname) .
      T.replace "{{DECKDECKGO_TITLE_SHORT}}" (T.take 12 $ unPresentationName pname) .
      T.replace "{{DECKDECKGO_AUTHOR}}" (unUsername uname) .
      T.replace "{{DECKDECKGO_USERNAME}}" (unUsername uname) .
      T.replace "{{DECKDECKGO_DESCRIPTION}}" (presentationDescription presentationInfo) .
      T.replace "{{DECKDECKGO_USER_ID}}"
        (unFirebaseId . unUserId $ presentationOwner presentationInfo) .
      T.replace "{{DECKDECKGO_DECKNAME}}" (unPresShortname psname) .
      T.replace "{{DECKDECKGO_BASE_HREF}}"
        ("/" <> unPresentationPrefix (presentationPrefix uname psname)) .
      T.replace "{{DECKDECKGO_BASE_HREF}}"
        ("/" <> unPresentationPrefix (presentationPrefix uname psname)) .
      T.replace "{{DECKDECKGO_HEAD_EXTRA}}"
        (fromMaybe "" (presentationHeadExtra presentationInfo))

sha256digest :: T.Text -> T.Text
sha256digest =
    T.decodeUtf8 .
    convertToBase Base64 .
    Hash.hashWith Hash.SHA256 .
    T.encodeUtf8

-- | finds all inlined javascript
findScripts :: T.Text -> [T.Text]
findScripts (TagSoup.parseTags -> ts) = xif ts
    (\f -> \case
      (TagSoup.TagOpen "script" _):(TagSoup.TagText t):rest ->
        t:(f rest)
      -- tagsoup guarantees that text nodes are not empty
      (TagSoup.TagOpen "script" _):(TagSoup.TagClose _):rest ->
        "":(f rest)
      [] -> []
      _:rest -> f rest
    )

mapFile :: (T.Text -> T.Text) -> FilePath -> IO ()
mapFile f fp = do
    T.readFile fp >>= T.writeFile fp . f

type Tag = TagSoup.Tag T.Text

processTags :: PresentationInfo -> [Tag] -> [Tag]
processTags presentationInfo = concatMap $ \case
  TagSoup.TagOpen str (HMS.fromList -> attrs)
    -- If the tag is 'deckgo-deck', we add the slides and the "background",
    -- "header" and "footer" divs
    | str == "deckgo-deck" -> do
        [ TagSoup.TagOpen str (HMS.toList (presentationAttributes presentationInfo <> attrs)) ] <>
          (concatMap slideTags (presentationSlides presentationInfo)) <>
          (maybe [] presentationBackgroundTags
            (presentationBackground presentationInfo)) <>
          (maybe [] presentationHeaderTags
            (presentationHeader presentationInfo)) <>
          (maybe [] presentationFooterTags
            (presentationFooter presentationInfo))
  t -> [t]

presentationBackgroundTags :: PresentationBackground -> [Tag]
presentationBackgroundTags (unPresentationBackground -> bg) =
    [ TagSoup.TagOpen "div" (HMS.toList $ HMS.singleton "slot" "background")
    ] <> TagSoup.parseTags bg <>
    [ TagSoup.TagClose "div"
    ]

presentationHeaderTags :: PresentationHeader -> [Tag]
presentationHeaderTags (unPresentationHeader -> bg) =
    [ TagSoup.TagOpen "div" (HMS.toList $ HMS.singleton "slot" "header")
    ] <> TagSoup.parseTags bg <>
    [ TagSoup.TagClose "div"
    ]

presentationFooterTags :: PresentationFooter -> [Tag]
presentationFooterTags (unPresentationFooter -> bg) =
    [ TagSoup.TagOpen "div" (HMS.toList $ HMS.singleton "slot" "footer")
    ] <> TagSoup.parseTags bg <>
    [ TagSoup.TagClose "div"
    ]

slideTags :: Slide -> [Tag]
slideTags slide =
    [ TagSoup.TagOpen
        ("deckgo-slide-" <> slideTemplate slide)
        (first Cases.spinalize <$> HMS.toList (slideAttributes slide))
    ] <> maybe [] TagSoup.parseTags (slideContent slide) <>
    [ TagSoup.TagClose
        ("deckgo-slide-" <> slideTemplate slide)
    ]


listObjects :: AWS.Env -> S3.BucketName -> Maybe T.Text -> IO [S3.Object]
listObjects env bname mpref = xif ([],Nothing) $ \f (es, ct) ->
    runAWS env (AWS.send $ S3.listObjectsV2 bname &
      S3.lovContinuationToken .~ ct &
      S3.lovPrefix .~ mpref
      ) >>= \case
      Right r -> do
        putStrLn "Listed objects..."
        let objs = r ^. S3.lovrsContents
        case (r ^. S3.lovrsIsTruncated, r ^. S3.lovrsNextContinuationToken) of
          (Just True, Just ct') -> f (es <> objs, Just ct')
          _ -> pure (es <> objs)
      Left e -> err "Could not list objects" e

deleteObjects :: AWS.Env -> S3.BucketName -> Maybe T.Text -> IO ()
deleteObjects env bname mpref = do
    es <- listObjects env bname mpref
    putStrLn $ "Deleting " <> show (length es) <> " objects..."
    deleteObjects' env bname $ map (^. S3.oKey) es

deleteObjects' :: AWS.Env -> S3.BucketName -> [S3.ObjectKey] -> IO ()
deleteObjects' env bname okeys =
    forConcurrentlyN_ 10 okeys $ \okey -> runAWS env (
      AWS.send $ S3.deleteObject bname okey) >>= \case
        Right {} -> pure ()
        Left e -> error $ "Could not delete object: " <> show e

newtype PresShortname = PresShortname { unPresShortname :: T.Text }
    deriving (Show)

deployPresentation
  :: AWS.Env
  -> Username
  -> PresShortname
  -> PresentationInfo
  -> IO ()
deployPresentation env uname psname presentationInfo = do
    let pprefix = presentationPrefix uname psname
    bucketName <- getEnv "BUCKET_NAME"
    let bucket = S3.BucketName (T.pack bucketName)
    putStrLn "Listing current objects"
    currentObjs <- listPresentationObjects env bucket pprefix
    putStrLn "Listing presentations files"

    withPresentationFiles uname psname presentationInfo $ \files -> do
      let
        currentObjs' =
          (\obj ->
            (obj ^. S3.oKey, fixupS3ETag $ obj ^. S3.oETag)
          ) <$> currentObjs
        (toPut, toDelete) = diffObjects files currentObjs'
      putStrLn $ "Deleting " <> show (length toDelete) <> " old files"
      deleteObjects' env bucket toDelete
      putStrLn $ "Uploading " <> show (length toPut) <> " new files"
      putObjects env bucket toPut

    queueName <- liftIO $ T.pack <$> getEnv "QUEUE_NAME"

    -- TODO: cleaner error handling down here

    liftIO $ putStrLn $ "Forwarding to queue: " <> T.unpack queueName
    queueUrl <- runAWS env (AWS.send $ SQS.getQueueURL queueName) >>= \case
      Right e -> pure $ e ^. SQS.gqursQueueURL
      Left e -> do
        liftIO $ print e
        error "Failed"

    liftIO $ print queueUrl

    res <- runAWS env $ AWS.send $ SQS.sendMessage queueUrl $
      T.decodeUtf8 $ BL.toStrict $ Aeson.encode $
        unPresentationPrefix pprefix

    case res of
      Right r -> do
        liftIO $ print r
      Left e -> do
        liftIO $ print e
        error "Failed!!"

putObjects
  :: AWS.Env
  -> S3.BucketName
  -> [(FilePath, S3.ObjectKey, S3.ETag)]
  -> IO ()
putObjects env bucket objs = forConcurrentlyN_ 10 objs $ putObject env bucket

putObject
  :: AWS.Env
  -> S3.BucketName
  -> (FilePath, S3.ObjectKey, S3.ETag)
  -> IO ()
putObject env bucket (fp, okey, etag) = do
    body <- Body.toBody <$> BS.readFile fp
    runAWS env (
      AWS.send $ S3.putObject bucket okey body &
          -- XXX: partial, though technically should never fail
          S3.poContentType .~ inferContentType (T.pack fp) &
          S3.poCacheControl .~ Just "no-cache"
      ) >>= \case
        Right r -> do
          putStrLn $ "Copied: " <> fp <> " to " <> show okey <> " with ETag " <> show etag
          case r ^. S3.porsETag of
            Just (fixupS3ETag -> s3ETag) ->
              when (etag /= s3ETag) $ do
                putStrLn $ "Warning: mismatched MD5: expected : actual: " <>
                  show etag <> " : " <> show s3ETag
            Nothing -> putStrLn "Warning: no ETag"
        Left e -> error $ "Error in put: " <> show e

-- | Some of the ETags we receive from S3 are surrounded with /"/ chars so we
-- strip them
fixupS3ETag :: S3.ETag -> S3.ETag
fixupS3ETag (S3.ETag etag) =
    S3.ETag $
      T.encodeUtf8 $
      T.dropWhileEnd (== '"') $
      T.dropWhile (== '"') $
      T.decodeUtf8 etag

mkObjectKey :: Username -> PresShortname -> [T.Text] -> S3.ObjectKey
mkObjectKey uname pname components = S3.ObjectKey $
    unPresentationPrefix (presentationPrefix uname pname) <> T.intercalate "/" components

-- | calculates the MD5 sum of a file, hex representation.
fileETag :: IsString a => FilePath -> IO a
fileETag fp =
    -- XXX: The 'show' step is very import, it's what converts the Digest to
    -- the Hex representation
    (fromString . show . MD5.md5 . BL.fromStrict) <$> BS.readFile fp

inferContentType :: T.Text -> Maybe T.Text
inferContentType = Just . T.decodeUtf8 .
    Mime.mimeByExt Mime.defaultMimeMap Mime.defaultMimeType

listDirectoryRecursive :: FilePath -> IO [(FilePath, [T.Text])]
listDirectoryRecursive = fix $ \f dir -> do
    -- XXX: /so/ not tail recursive
    (dirs, fs) <- Dir.listDirectory dir >>=
      mapM (\component -> pure (dir </> component, [T.pack component])) >>=
      partitionM (Dir.doesDirectoryExist . fst)
    fs' <- concatMapM
      (\(dir', components) -> fmap (second (components <>)) <$> (f dir'))
      dirs
    pure (fs <> fs')

-- Data.List for Monad

-- | A version of 'partition' that works with a monadic predicate.
--
-- > partitionM (Just . even) [1,2,3] == Just ([2], [1,3])
-- > partitionM (const Nothing) [1,2,3] == Nothing
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM act = foldr f (return [])
    where f x xs = do x' <- act x; if null x' then xs else do xs' <- xs; return $ x' ++xs'

forConcurrentlyN_
  :: (MonadUnliftIO m) => Int -> [a] -> (a -> m b) -> m ()
forConcurrentlyN_ n xs act = forConcurrently_ (nChunks n xs) (mapM_ act)

nChunks :: Int -> [a] -> [[a]]
nChunks n xs = HMS.elems $ snd $ foldl'
    (\(i, m) v -> (i+1, HMS.insertWith (<>) (i `mod` n) [v] m) )
    (0, HMS.empty) xs
