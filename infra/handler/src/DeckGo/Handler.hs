{-# LANGUAGE DataKinds #-}
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

-- TODO: created_at, updated_at
-- TODO: nullable slide content
-- TODO: improve swagger description
-- TODO: feed API

-- TODO: check permissions
-- TODO: TTL on anonymous users
-- TODO: enforce uniqueness on deck_name (per user)
-- TODO: return 500 on all DB errors

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
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
import qualified Codec.Archive.Tar as Tar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HMS
-- import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Hasql.Connection as HC
-- import qualified Hasql.Connection as HC
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS
import qualified Network.AWS as AWS
-- import qualified Network.AWS as AWS
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

newtype BucketName = BucketName { unBucketName :: T.Text }

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

-- DECKS

type DecksAPI =
    Protected :> QueryParam "owner_id" UserId :> Get '[JSON] [Item DeckId Deck] :<|>
    Protected :>
      Capture "deck_id" DeckId :>
      Get '[JSON] (Item DeckId Deck) :<|>
    Protected :>
      Capture "deck_id" DeckId :>
      "publish" :>
      Post '[JSON] PresResponse :<|> -- XXX
    Protected :> ReqBody '[JSON] Deck :> Post '[JSON] (Item DeckId Deck) :<|>
    Protected :>
      Capture "deck_id" DeckId :>
      ReqBody '[JSON] Deck :> Put '[JSON] (Item DeckId Deck) :<|>
    Protected :> Capture "deck_id" DeckId :> Delete '[JSON] ()

newtype DeckId = DeckId { unDeckId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, FromHttpApiData, ToHttpApiData, Show, Eq)

newtype Deckname = Deckname { unDeckname :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

newtype Deckbackground = Deckbackground { unDeckbackground :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data Deck = Deck
  { deckSlides :: [SlideId]
  , deckDeckname :: Deckname
  , deckDeckbackground :: Maybe Deckbackground
  , deckOwnerId :: UserId
  , deckAttributes :: HMS.HashMap T.Text T.Text
  } deriving (Show, Eq)

instance FromJSONObject Deck where
  parseJSONObject = \obj ->
    Deck
      <$> obj .: "slides"
      <*> obj .: "name"
      <*> obj .:? "background"
      <*> obj .: "owner_id"
      <*> obj .:? "attributes" .!= HMS.empty

instance ToJSONObject Deck where
  toJSONObject deck = HMS.fromList
    [ "slides" .= deckSlides deck
    , "name" .= deckDeckname deck
    , "background" .= deckDeckbackground deck
    , "owner_id" .= deckOwnerId deck
    , "attributes" .= deckAttributes deck
    ]

instance Aeson.FromJSON Deck where
  parseJSON = Aeson.withObject "Deck" parseJSONObject
instance Aeson.ToJSON Deck where
  toJSON = Aeson.Object . toJSONObject

instance ToSchema (Item DeckId Deck) where
  declareNamedSchema _ = pure $ NamedSchema (Just "DeckWithId") mempty

instance ToSchema Deck where
  declareNamedSchema _ = pure $ NamedSchema (Just "Deck") mempty

instance ToParamSchema (Item DeckId Deck) where
  toParamSchema _ = mempty

instance ToParamSchema DeckId where
  toParamSchema _ = mempty

-- SLIDES

type SlidesAPI =
    Protected :> Capture "deck_id" DeckId :> "slides" :>
      Capture "slide_id" SlideId :> Get '[JSON] (Item SlideId Slide) :<|>
    Protected :> Capture "deck_id" DeckId :> "slides" :>
      ReqBody '[JSON] Slide :> Post '[JSON] (Item SlideId Slide) :<|>
    Protected :> Capture "deck_id" DeckId :> "slides" :>
      Capture "slide_id" SlideId :>
      ReqBody '[JSON] Slide :>
      Put '[JSON] (Item SlideId Slide) :<|>
    Protected :> Capture "deck_id" DeckId :> "slides" :>
      Capture "slide_id" SlideId :> Delete '[JSON] ()

instance ToSchema (Item SlideId Slide) where
  declareNamedSchema _ = pure $ NamedSchema (Just "SlideWithId") mempty

instance ToSchema Slide where
  declareNamedSchema _ = pure $ NamedSchema (Just "Slide") mempty

instance ToParamSchema (Item SlideId Slide) where
  toParamSchema _ = mempty

newtype SlideId = SlideId { unSlideId :: T.Text }
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

instance ToParamSchema SlideId

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
  { presentationName :: Deckname
  , presentationOwner :: UserId
  , presentationBackground :: Maybe Deckbackground
  , presentationAttributes :: HMS.HashMap T.Text T.Text
  , presentationSlides :: [Slide]
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
      obj .:? "attributes" .!= HMS.empty <*>
      obj .: "slides"

instance FromJSONObject PresentationResult where
  parseJSONObject = undefined -- \obj ->

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
    "decks" :> DecksAPI :<|>
    "decks" :> SlidesAPI :<|>
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
    serveDecks :<|>
    serveSlides :<|>
    servePresentations
  where
    serveUsers =
      usersGet conn :<|>
      usersGetUserId conn :<|>
      usersPost conn :<|>
      usersPut conn :<|>
      usersDelete conn
    serveDecks =
      decksGet conn :<|>
      decksGetDeckId conn :<|>
      decksPostPublish env conn :<|>
      decksPost conn :<|>
      decksPut conn :<|>
      decksDelete conn
    serveSlides =
      slidesGetSlideId conn :<|>
      slidesPost conn :<|>
      slidesPut conn :<|>
      slidesDelete conn
    servePresentations =
      presentationsPost env conn :<|>
      presentationsPut env conn

presentationsPost
  :: AWS.Env -> HC.Connection
  -> Firebase.UserId
  -> PresentationInfo
  -> Servant.Handler (Item PresentationId PresentationResult)
presentationsPost env conn _userId pinfo = do
    liftIO $ print pinfo
    iface <- liftIO $ getDbInterface conn
    user <- liftIO (dbGetUserById iface (presentationOwner pinfo)) >>= \case
      Nothing -> Servant.throwError Servant.err404
      Just u -> pure u

    let userId = presentationOwner pinfo

    uname <- case userUsername (itemContent user) of
      Nothing -> error "User has no name"
      Just uname -> pure uname

    liftIO $ deployPresentation env uname pinfo

    presId <- liftIO $ PresentationId <$> newId
    let presName = presentationPrefix uname (presentationName pinfo)

    -- something fishy going on here
    let presName' = sanitizeDeckname (presentationName pinfo)

    presUrl <- do
      purl <- liftIO (getEnv "DECKGO_PRESENTATIONS_URL")
      pure $ "https://" <> T.pack purl <> "/" <> presName

    -- TODO: make unique
    liftIO (dbCreatePresentation iface presId presName' presUrl userId)

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
    liftIO $ print pinfo
    iface <- liftIO $ getDbInterface conn

    user <- liftIO (dbGetUserById iface (presentationOwner pinfo)) >>= \case
      Nothing -> Servant.throwError Servant.err404
      Just u -> pure u

    uname <- case userUsername (itemContent user) of
      Nothing -> error "User has no name"
      Just uname -> pure uname

    (presName, presUrl) <- liftIO (dbGetPresentationById iface pid) >>= \case
      Nothing -> Servant.throwError Servant.err404
      Just res -> pure res

    -- XXX: huge hack because we know we stored the "correct" presentation name
    let pinfo' = pinfo { presentationName = Deckname presName }
    liftIO $ deployPresentation env uname pinfo'

    pure $ Item
      { itemId = pid
      , itemContent = PresentationResult { presentationUrl = presUrl }
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
      Left () -> Servant.throwError $ Servant.err409
        { Servant.errBody = Aeson.encode (Item userId user) }
      Right () -> pure $ Item userId user

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

usersPostSession :: UserId -> User -> HS.Session (Either () ())
usersPostSession uid u = do
    HS.sql "BEGIN"
    liftIO $ putStrLn "Creating user in DB"
    HS.statement (uid,u) usersPostStatement >>= \case
      1 -> do
        liftIO $ putStrLn "User was created"
        case userUsername u of
          Just uname -> do
            liftIO $ putStrLn "Creating username"
            HS.statement (uname, uid) usersPostStatement' >>= \case
              1 -> do
                liftIO $ putStrLn "User created successfully"
                HS.sql "COMMIT"
                pure $ Right ()
              _ -> do
                liftIO $ putStrLn "Couldn't create username"
                HS.sql "ROLLBACK"
                pure $ Left ()
          Nothing -> do
            liftIO $ putStrLn "No username"
            HS.sql "COMMIT"
            pure $ Right ()
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

-- TODO: deal with conflict error
usersPostStatement' :: Statement (Username, UserId) Int64
usersPostStatement' = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "UPDATE account SET username = $1 WHERE id = $2" ]
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


presentationsPostSession :: PresentationId -> T.Text -> T.Text -> UserId -> HS.Session ()
presentationsPostSession pid pnam purl uid = do
    liftIO $ putStrLn "Creating presentation in DB"
    HS.statement (pid, pnam, purl, uid) presentationsPostStatement

presentationsPostStatement :: Statement (PresentationId, T.Text, T.Text, UserId) ()
presentationsPostStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "INSERT INTO presentation"
      ,   "(id, name, url, owner)"
      ,   "VALUES ($1, $2, $3, $4)"
      ]
    encoder =
      contramap (unPresentationId . view _1) (HE.param HE.text) <>
      contramap (view _2) (HE.param HE.text) <>
      contramap (view _3) (HE.param HE.text) <>
      contramap
        (unFirebaseId . unUserId . view _4)
        (HE.param HE.text)
    decoder = HD.unit

presentationsGetByIdSession :: PresentationId -> HS.Session (Maybe (T.Text, T.Text))
presentationsGetByIdSession pid = do
    liftIO $ putStrLn $ "Getting presentation by id"
    HS.statement pid presentationsGetByIdStatement

presentationsGetByIdStatement :: Statement PresentationId (Maybe (T.Text, T.Text))
presentationsGetByIdStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT name, url, owner FROM presentation"
      ,   "WHERE id = $1"
      ]
    encoder = contramap unPresentationId (HE.param HE.text)
    decoder = HD.rowMaybe $ (,) <$>
      HD.column HD.text <*>
      HD.column HD.text -- <*>
      -- TODO: return user ID

-- DECKS

decksGetSession :: HS.Session [Item DeckId Deck]
decksGetSession = do
    HS.statement () decksGetStatement

decksGetStatement :: Statement () [Item DeckId Deck]
decksGetStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT"
      ,   "id,"
      ,   "array(SELECT id FROM slide WHERE deck = deck.id ORDER BY index),"
      ,   "name,"
      ,   "background,"
      ,   "owner,"
      ,   "attributes"
      ,   "FROM deck"
      ]
    encoder = HE.unit
    decoder = HD.rowList $ Item <$>
      (DeckId <$> HD.column HD.text) <*>
      ( Deck <$>
      (let listArray = (HD.array . HD.dimension replicateM . HD.element)
        in HD.column (listArray (SlideId <$> HD.text))) <*>
      (Deckname <$> (HD.column HD.text)) <*>
      ((fmap Deckbackground) <$> (HD.nullableColumn HD.text)) <*>
      ((UserId . FirebaseId) <$> HD.column HD.text) <*>
      HD.column (HD.jsonBytes (\bs ->
        first T.pack (Aeson.eitherDecode $ BL.fromStrict bs)
        ))
      )

decksPostSession :: DeckId -> Deck -> HS.Session ()
decksPostSession did d = do
    liftIO $ putStrLn "Creating deck in DB"
    HS.sql "BEGIN"
    HS.statement (did, d) decksPostStatement
    unless (deckSlides d == []) $
      error "A fresh deck cannot have slides"
    HS.sql "COMMIT"

decksPostStatement :: Statement (DeckId, Deck) ()
decksPostStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "INSERT INTO deck"
      ,   "(id, name, background, owner, attributes)"
      ,   "VALUES ($1, $2, $3, $4, $5)"
      ]
    encoder =
      contramap (unDeckId . view _1) (HE.param HE.text) <>
      contramap (unDeckname . deckDeckname . view _2) (HE.param HE.text) <>
      contramap
        (fmap unDeckbackground . deckDeckbackground . view _2)
        (HE.nullableParam HE.text) <>
      contramap
        (unFirebaseId . unUserId . deckOwnerId . view _2)
        (HE.param HE.text) <>
      contramap (Aeson.toJSON . deckAttributes . view _2) (HE.param HE.json)
    decoder = HD.unit

decksPutSession :: DeckId -> Deck -> HS.Session ()
decksPutSession did d = do
    liftIO $ putStrLn "Creating deck in DB"
    HS.sql "BEGIN"
    HS.statement (did, d) decksPutStatement
    forM_ (zip (deckSlides d) [0..]) $ \(sid, idx) ->
      HS.statement (sid, idx) slideReindexStatement
    HS.sql "COMMIT"

decksPutStatement :: Statement (DeckId, Deck) ()
decksPutStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "UPDATE deck"
      ,   "SET name = $2, background = $3, owner = $4, attributes = $5"
      ,   "WHERE id = $1"
      ]
    encoder =
      contramap (unDeckId . view _1) (HE.param HE.text) <>
      contramap (unDeckname . deckDeckname . view _2) (HE.param HE.text) <>
      contramap
        (fmap unDeckbackground . deckDeckbackground . view _2)
        (HE.nullableParam HE.text) <>
      contramap
        (unFirebaseId . unUserId . deckOwnerId . view _2)
        (HE.param HE.text) <>
      contramap (Aeson.toJSON . deckAttributes . view _2) (HE.param HE.json)
    decoder = HD.unit

slideReindexStatement :: Statement (SlideId, Int16 {- the slide index -}) ()
slideReindexStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "UPDATE slide"
      ,   "SET index = $2"
      ,   "WHERE id = $1"
      ]
    encoder =
      contramap (unSlideId . view _1) (HE.param HE.text) <>
      contramap (view _2) (HE.param HE.int2)
    decoder = HD.unit

decksGetByIdSession :: DeckId -> HS.Session (Maybe Deck)
decksGetByIdSession did = do
    liftIO $ putStrLn $ "Getting deck by id"
    HS.statement did decksGetByIdStatement

decksGetByIdStatement :: Statement DeckId (Maybe Deck)
decksGetByIdStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT"
      ,   "array(SELECT id FROM slide WHERE deck = $1 ORDER BY index),"
      ,   "name,"
      ,   "background,"
      ,   "owner,"
      ,   "attributes"
      ,   "FROM deck WHERE id = $1"
      ]
    encoder = contramap unDeckId (HE.param HE.text)
    decoder = HD.rowMaybe $ Deck <$>
      (let listArray = (HD.array . HD.dimension replicateM . HD.element)
        in HD.column (listArray (SlideId <$> HD.text))) <*>
      (Deckname <$> (HD.column HD.text)) <*>
      ((fmap Deckbackground) <$> (HD.nullableColumn HD.text)) <*>
      ((UserId . FirebaseId) <$> HD.column HD.text) <*>
      HD.column (HD.jsonBytes (\bs ->
        first T.pack (Aeson.eitherDecode $ BL.fromStrict bs)
        ))

decksGet :: HC.Connection -> Firebase.UserId -> Maybe UserId -> Servant.Handler [Item DeckId Deck]
decksGet conn fuid mUserId = do

    userId <- case mUserId of
      Nothing -> do
        liftIO $ putStrLn $ unwords
          [ "No user specified when GETting decks:", show fuid ]
        Servant.throwError Servant.err400
      Just userId -> pure userId

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId userId)) $ do
      liftIO $ putStrLn $ unwords
        [ "Client asking for decks as another user", show (fuid, userId) ]
      Servant.throwError Servant.err403
    iface <- liftIO $ getDbInterface conn

    liftIO $ dbGetAllDecks iface

-- TODO: auth?
decksGetDeckId
    :: HC.Connection
    -> Firebase.UserId
    -> DeckId
    -> Servant.Handler (Item DeckId Deck)
decksGetDeckId conn fuid deckId = do

    iface <- liftIO $ getDbInterface conn
    deck <- liftIO (dbGetDeckById iface deckId) >>= \case
      Nothing -> do
        liftIO $ putStrLn $ "Deck not found: " <> show deckId
        Servant.throwError Servant.err404
      Just d -> pure d

    let ownerId = deckOwnerId deck

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId ownerId)) $ do
      liftIO $ putStrLn $ unwords $
        [ "Deck was found", show deck, "but requester is not the owner", show fuid ]
      Servant.throwError Servant.err404

    pure (Item deckId deck)

decksDeleteSession :: DeckId -> HS.Session ()
decksDeleteSession did = do
    liftIO $ putStrLn $ "Deleting deck by id"
    HS.statement did decksDeleteStatement

decksDeleteStatement :: Statement DeckId ()
decksDeleteStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "DELETE FROM deck"
      ,   "WHERE id = $1"
      ]
    encoder = contramap unDeckId (HE.param HE.text)
    decoder = HD.unit

data PresResponse = PresResponse T.Text

instance Aeson.ToJSON PresResponse where
  toJSON (PresResponse t) = Aeson.object [ "url" .= t ]

instance Aeson.FromJSON PresResponse where
  parseJSON = Aeson.withObject "pres-response" $ \o ->
    PresResponse <$> o .: "url"


decksPostPublish
  :: AWS.Env
  -> HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> Servant.Handler PresResponse
  -- TODO: AUTH!!!!
decksPostPublish env conn _ deckId = do

    -- TODO: check auth

    liftIO $ putStrLn "Your PRESENTATION WAS PUBLISHED!!!!"

    queueName <- liftIO $ T.pack <$> getEnv "QUEUE_NAME"

    liftIO $ putStrLn $ "Forwarding to queue: " <> T.unpack queueName
    queueUrl <- runAWS env (AWS.send $ SQS.getQueueURL queueName) >>= \case
      Right e -> pure $ e ^. SQS.gqursQueueURL
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    liftIO $ print queueUrl

    res <- runAWS env $ AWS.send $ SQS.sendMessage queueUrl $
      T.decodeUtf8 $ BL.toStrict $ Aeson.encode deckId

    case res of
      Right r -> do
        liftIO $ print r
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    presUrl <- liftIO (getEnv "DECKGO_PRESENTATIONS_URL")
    iface <- liftIO $ getDbInterface conn
    liftIO (dbGetDeckById iface deckId) >>= \case
      Nothing -> Servant.throwError Servant.err500
      Just deck -> do
        let dname = deckDeckname deck
        liftIO (fmap itemContent <$> dbGetUserById iface (deckOwnerId deck)) >>= \case
          Nothing -> do
            liftIO $ putStrLn "No User Id"
            Servant.throwError Servant.err500
          Just user -> case userUsername user of
            Nothing -> do
              liftIO $ putStrLn "No username"
              Servant.throwError Servant.err500
            Just uname ->
              pure $ PresResponse $
                "https://" <>
                T.pack presUrl <>
                "/" <>
                presentationPrefix uname dname

decksPost
  :: HC.Connection
  -> Firebase.UserId
  -> Deck
  -> Servant.Handler (Item DeckId Deck)
decksPost conn fuid deck = do

    let ownerId = deckOwnerId deck

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId ownerId)) $ do
      liftIO $ putStrLn $ unwords $
        [ "Deck was POSTed", show deck, "but requester is not the owner", show fuid ]
      Servant.throwError Servant.err400

    deckId <- liftIO $ DeckId <$> newId

    iface <- liftIO $ getDbInterface conn

    liftIO $ dbCreateDeck iface deckId deck

    pure $ Item deckId deck

decksPut
    :: HC.Connection
    -> Firebase.UserId
    -> DeckId
    -> Deck
    -> Servant.Handler (Item DeckId Deck)
decksPut conn fuid deckId deck = do

    getDeck conn deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to PUT", show deckId, "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just Deck{deckOwnerId} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Deck was PUTed", show deck, "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

    iface <- liftIO $ getDbInterface conn

    liftIO $ dbUpdateDeck iface deckId deck

    pure $ Item deckId deck

decksDelete :: HC.Connection -> Firebase.UserId -> DeckId -> Servant.Handler ()
decksDelete conn fuid deckId = do

    getDeck conn deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to DELETE", show deckId, "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just Deck{deckOwnerId} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Deck was DELETEd", show deckId, "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

    iface <- liftIO $ getDbInterface conn

    liftIO $ dbDeleteDeck iface deckId

-- | Reads a Deck from the database.
getDeck :: HC.Connection -> DeckId -> Servant.Handler (Maybe Deck)
getDeck conn deckId = do

    iface <- liftIO $ getDbInterface conn

    liftIO $ dbGetDeckById iface deckId

-- SLIDES

slidesPostSession :: SlideId -> DeckId -> Slide -> HS.Session ()
slidesPostSession sid did s = do
    liftIO $ putStrLn "Creating slide in DB"
    HS.statement (sid, did, s) slidesPostStatement

slidesPostStatement :: Statement (SlideId, DeckId, Slide) ()
slidesPostStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "INSERT INTO slide"
      ,   "(id, deck, content, template, attributes)"
      ,   "VALUES ($1, $2, $3, $4, $5)"
      ]
    encoder =
      contramap (unSlideId . view _1) (HE.param HE.text) <>
      contramap (unDeckId . view _2) (HE.param HE.text) <>
      contramap (slideContent . view _3) (HE.nullableParam HE.text) <>
      contramap (slideTemplate . view _3) (HE.param HE.text) <>
      contramap (Aeson.toJSON . slideAttributes . view _3) (HE.param HE.json)
    decoder = HD.unit

slidesPutSession :: SlideId -> Slide -> HS.Session ()
slidesPutSession sid s = do
    liftIO $ putStrLn "Updating slide in DB"
    HS.statement (sid, s) slidesPutStatement

slidesPutStatement :: Statement (SlideId, Slide) ()
slidesPutStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "UPDATE slide"
      ,   "SET content = $2, template = $3, attributes = $4"
      ,   "WHERE id = $1"
      ]
    encoder =
      contramap (unSlideId . view _1) (HE.param HE.text) <>
      contramap (slideContent . view _2) (HE.nullableParam HE.text) <>
      contramap (slideTemplate . view _2) (HE.param HE.text) <>
      contramap (Aeson.toJSON . slideAttributes . view _2) (HE.param HE.json)
    decoder = HD.unit

slidesGetSlideId
  :: HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> SlideId
  -> Servant.Handler (Item SlideId Slide)
slidesGetSlideId conn fuid deckId slideId = do

    getDeck conn deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to GET slide", show slideId, "of deck",  show deckId
          , "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just deck@Deck{deckOwnerId, deckSlides} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to GET slide", show slideId, "of deck", show deck
            , "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

        unless (slideId `elem` deckSlides) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to GET slide", show slideId, "of deck", show deck
            , "but slide doesn't belong to deck owned by", show fuid ]
          Servant.throwError Servant.err404


    iface <- liftIO $ getDbInterface conn

    liftIO (dbGetSlideById iface slideId) >>= \case
      Nothing -> Servant.throwError Servant.err404
      Just slide -> pure $ Item slideId slide

slidesGetByIdSession :: SlideId -> HS.Session (Maybe Slide)
slidesGetByIdSession sid = do
    liftIO $ putStrLn $ "Getting slide by id"
    HS.statement sid slidesGetByIdStatement

slidesGetByIdStatement :: Statement SlideId (Maybe Slide)
slidesGetByIdStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "SELECT content, template, attributes FROM slide"
      ,   "WHERE id = $1"
      ]
    encoder = contramap unSlideId (HE.param HE.text)
    decoder = HD.rowMaybe $ Slide <$>
      HD.nullableColumn HD.text <*>
      HD.column HD.text <*>
      HD.column (HD.jsonBytes (\bs ->
        first T.pack (Aeson.eitherDecode $ BL.fromStrict bs)
        ))

slidesDeleteSession :: SlideId -> HS.Session ()
slidesDeleteSession sid = do
    liftIO $ putStrLn $ "Deleting slide by id"
    HS.statement sid slidesDeleteStatement

slidesDeleteStatement :: Statement SlideId ()
slidesDeleteStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "DELETE FROM slide"
      ,   "WHERE id = $1"
      ]
    encoder = contramap unSlideId (HE.param HE.text)
    decoder = HD.unit

slidesPost
  :: HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> Slide -- TODO: slide index
  -> Servant.Handler (Item SlideId Slide)
slidesPost conn fuid deckId slide = do
    iface <- liftIO $ getDbInterface conn

    getDeck conn deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to POST slide", show slide, "of deck",  show deckId
          , "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just deck@Deck{deckOwnerId} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to POST slide", show slide, "of deck", show deck
            , "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

    slideId <- liftIO $ SlideId <$> newId

    liftIO (dbCreateSlide iface slideId deckId slide)

    pure $ Item slideId slide

slidesPut
  :: HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> SlideId
  -> Slide
  -> Servant.Handler (Item SlideId Slide)
slidesPut conn fuid deckId slideId slide = do

    iface <- liftIO $ getDbInterface conn

    getDeck conn deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to PUT slide", show slideId, "of deck",  show deckId
          , "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just deck@Deck{deckOwnerId, deckSlides} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to PUT slide", show slideId, "of deck", show deck
            , "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

        unless (slideId `elem` deckSlides) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to PUT slide", show slideId, "of deck", show deck
            , "but slide doesn't belong to deck owned by", show fuid ]
          Servant.throwError Servant.err404

    liftIO (dbUpdateSlide iface slideId slide)

    pure $ Item slideId slide

slidesDelete :: HC.Connection -> Firebase.UserId -> DeckId -> SlideId -> Servant.Handler ()
slidesDelete conn fuid deckId slideId = do

    getDeck conn deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to DELETE slide", show slideId, "of deck",  show deckId
          , "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just deck@Deck{deckOwnerId, deckSlides} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to DELETE slide", show slideId, "of deck", show deck
            , "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

        unless (slideId `elem` deckSlides) $ do
          liftIO $ putStrLn $ unwords $
            [ "Trying to DELETE slide", show slideId, "of deck", show deck
            , "but slide doesn't belong to deck owned by", show fuid ]
          Servant.throwError Servant.err404

    iface <- liftIO $ getDbInterface conn
    liftIO $ dbDeleteSlide iface slideId

-------------------------------------------------------------------------------
-- DATABASE
-------------------------------------------------------------------------------

data DbInterface = DbInterface
  { dbGetAllUsers :: IO [Item UserId User]
  , dbGetUserById :: UserId -> IO (Maybe (Item UserId User))
  , dbCreateUser :: UserId -> User -> IO (Either () ())
  , dbUpdateUser :: UserId -> User -> IO UserUpdateResult
  , dbDeleteUser :: UserId -> IO (Either () ())

  , dbGetAllDecks :: IO [Item DeckId Deck]
  , dbCreateDeck :: DeckId -> Deck -> IO ()
  , dbUpdateDeck :: DeckId -> Deck -> IO ()
  , dbGetDeckById :: DeckId -> IO (Maybe Deck)
  , dbDeleteDeck :: DeckId -> IO () -- TODO: either () () for not found

  , dbGetSlideById :: SlideId -> IO (Maybe Slide)
  , dbCreateSlide :: SlideId -> DeckId -> Slide -> IO ()
  , dbUpdateSlide :: SlideId -> Slide -> IO ()
  , dbDeleteSlide :: SlideId -> IO () -- TODO: either () () for not found

  -- TODO: dbCreateSlide: if duplicated, no error !?
  , dbCreatePresentation :: PresentationId -> T.Text -> T.Text -> UserId -> IO ()
  , dbGetPresentationById :: PresentationId -> IO (Maybe (T.Text, T.Text))
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

latestDbVersion :: DbVersion
latestDbVersion = maxBound

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

      , dbGetAllDecks = wrap decksGetSession
      , dbCreateDeck = \did d -> wrap (decksPostSession did d)
      , dbUpdateDeck = \did d -> wrap (decksPutSession did d)
      , dbGetDeckById = \did -> wrap (decksGetByIdSession did)
      , dbDeleteDeck = \did -> wrap (decksDeleteSession did)

      , dbGetSlideById = \sid -> wrap (slidesGetByIdSession sid)
      , dbCreateSlide = \sid did s -> wrap (slidesPostSession sid did s)
      , dbUpdateSlide = \sid s -> wrap (slidesPutSession sid s)
      , dbDeleteSlide = \sid -> wrap (slidesDeleteSession sid)

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

-- TODO: what happens when the deckname is "-" ?
presentationPrefix :: Username -> Deckname -> T.Text
presentationPrefix uname dname =
    unUsername uname <> "/" <> sanitizeDeckname dname <> "/"

sanitizeDeckname :: Deckname -> T.Text
sanitizeDeckname = T.toLower . strip . dropBadChars . unDeckname
  where
    strip :: T.Text -> T.Text
    strip = T.dropAround ( == '-' )
    dropBadChars :: T.Text -> T.Text
    dropBadChars = T.concatMap
      $ \case
        c | isAscii c && isAlphaNum c -> T.singleton c
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
  -> Username
  -> Deckname
  -> IO [S3.Object]
listPresentationObjects env bucket uname dname =
    listObjects env bucket (Just $ presentationPrefix uname dname)

withPresentationFiles
  :: Username
  -> PresentationInfo
  -- -> Deck
  -- -> [Slide
  -> ([(FilePath, S3.ObjectKey, S3.ETag)] -> IO a)
  -> IO a
withPresentationFiles uname presentationInfo act = do
    deckgoStarterDist <- getEnv "DECKGO_STARTER_DIST"
    Temp.withSystemTempDirectory "dist" $ \dir -> do
      Tar.extract dir deckgoStarterDist
      mapFile processIndex $ dir </> "index.html"
      mapFile interpol $ dir </> "manifest.json"
      putStrLn "Listing files..."
      files <- listDirectoryRecursive dir
      files' <- forM files $ \(fp, components) -> do
        etag <- fileETag fp
        let okey = mkObjectKey uname dname components
        pure (fp, okey, etag)
      act files'
  where
    dname = presentationName presentationInfo
    processIndex :: T.Text -> T.Text
    processIndex =
      TagSoup.renderTags . processTags presentationInfo . TagSoup.parseTags .
      interpol
    interpol =
      T.replace "{{DECKDECKGO_TITLE}}" (unDeckname dname) .
      T.replace "{{DECKDECKGO_TITLE_SHORT}}" (T.take 12 $ unDeckname dname) .
      T.replace "{{DECKDECKGO_AUTHOR}}" (unUsername uname) .
      T.replace "{{DECKDECKGO_USERNAME}}" (unUsername uname) .
      T.replace "{{DECKDECKGO_USER_ID}}"
        (unFirebaseId . unUserId $ presentationOwner presentationInfo) .
      T.replace "{{DECKDECKGO_DECKNAME}}" (sanitizeDeckname dname) .
      -- TODO: description
      T.replace "{{DECKDECKGO_DESCRIPTION}}" "(no description given)" .
      T.replace "{{DECKDECKGO_BASE_HREF}}"
        ("/" <> presentationPrefix uname dname)

mapFile :: (T.Text -> T.Text) -> FilePath -> IO ()
mapFile f fp = do
    T.readFile fp >>= T.writeFile fp . f

type Tag = TagSoup.Tag T.Text

processTags :: PresentationInfo -> [Tag] -> [Tag]
processTags presentationInfo = concatMap $ \case
  TagSoup.TagOpen str (HMS.fromList -> attrs)
    | str == "deckgo-deck" -> do
        [ TagSoup.TagOpen str (HMS.toList (presentationAttributes presentationInfo <> attrs)) ] <>
          (concatMap slideTags (presentationSlides presentationInfo)) <>
          (maybe [] deckBackgroundTags (presentationBackground presentationInfo))
  t -> [t]

deckBackgroundTags :: Deckbackground -> [Tag]
deckBackgroundTags (unDeckbackground -> bg) =
    [ TagSoup.TagOpen "div" (HMS.toList $ HMS.singleton "slot" "background")
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

-- TODO: sanitize deck name
deployDeck :: AWS.Env -> HC.Connection -> DeckId -> IO ()
deployDeck env conn deckId = do
    iface <- liftIO $ getDbInterface conn
    dbGetDeckById iface deckId >>= \case
      Nothing -> pure () -- TODO
      Just deck -> do
        liftIO (fmap itemContent <$> dbGetUserById iface (deckOwnerId deck)) >>= \case
          Nothing -> pure () -- TODO
          Just user -> case userUsername user of
            Nothing -> pure () -- TODO
            Just uname -> do
              slides <- catMaybes <$> mapM (dbGetSlideById iface) (deckSlides deck)
              deployPresentation env uname $ deckToPres deck slides

deckToPres :: Deck -> [Slide] -> PresentationInfo
deckToPres deck slides = PresentationInfo
    { presentationName = deckDeckname deck
    , presentationOwner = deckOwnerId deck
    , presentationBackground = deckDeckbackground deck
    , presentationAttributes = deckAttributes deck
    , presentationSlides = slides
    }

deployPresentation :: AWS.Env -> Username -> PresentationInfo -> IO ()
deployPresentation env uname presentationInfo = do
    bucketName <- getEnv "BUCKET_NAME"
    let bucket = S3.BucketName (T.pack bucketName)
    let dname = presentationName presentationInfo
    putStrLn "Listing current objects"
    currentObjs <- listPresentationObjects env bucket uname dname
    putStrLn "Listing presentations files"



    withPresentationFiles uname presentationInfo $ \files -> do
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
      T.decodeUtf8 $ BL.toStrict $ Aeson.encode (presentationPrefix uname dname)

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
          S3.poContentType .~ inferContentType (T.pack fp)
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

mkObjectKey :: Username -> Deckname -> [T.Text] -> S3.ObjectKey
mkObjectKey uname dname components = S3.ObjectKey $
    presentationPrefix uname dname <> T.intercalate "/" components

fileETag :: FilePath -> IO S3.ETag
fileETag fp =
    -- XXX: The 'show' step is very import, it's what converts the Digest to
    -- the Hex representation
    (fromString . show . MD5.md5) <$> BL.readFile fp

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
