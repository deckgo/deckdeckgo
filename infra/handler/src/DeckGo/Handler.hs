{-# LANGUAGE DataKinds #-}
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

-- TODO: double check what is returned on 200 from DynamoDB
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
import Data.Int
import Data.List (find)
import Data.Maybe
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Hasql.Statement (Statement(..))
import Servant (Context ((:.)))
import Servant.API
import Servant.Auth.Firebase (Protected)
import UnliftIO
import Data.Char
import System.Environment
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Hasql.Connection as HC
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS
import qualified Network.AWS as Aws
import qualified Network.AWS.Data as Data
import qualified Network.AWS.DynamoDB as DynamoDB
import qualified Network.AWS.SQS as SQS
import qualified Network.Wai as Wai
import qualified Servant as Servant
import qualified Servant.Auth.Firebase as Firebase
import qualified System.Random as Random

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
      ReqBody '[JSON] UserInfo :> Put '[JSON] (Item UserId User) :<|>
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

type API = "api" :> (
    "users" :> UsersAPI :<|>
    "decks" :> DecksAPI :<|>
    "decks" :> SlidesAPI
    )

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

application
  :: Firebase.FirebaseLoginSettings
  -> Aws.Env
  -> HC.Connection
  -> Wai.Application
application settings env conn =
    Servant.serveWithContext
      api
      (settings :. Servant.EmptyContext)
      (server env conn)

server :: Aws.Env -> HC.Connection -> Servant.Server API
server env conn = serveUsers :<|> serveDecks :<|> serveSlides
  where
    serveUsers =
      usersGet conn :<|>
      usersGetUserId conn :<|>
      usersPost conn :<|>
      usersPut conn :<|>
      usersDelete conn
    serveDecks =
      decksGet env :<|>
      decksGetDeckId env :<|>
      decksPostPublish env conn :<|>
      decksPost env :<|>
      decksPut env :<|>
      decksDelete env
    serveSlides =
      slidesGetSlideId env conn :<|>
      slidesPost env conn :<|>
      slidesPut env conn :<|>
      slidesDelete env conn

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
      [ "SELECT account.id, account.firebase_id, username.id"
      ,   "FROM account"
      ,   "LEFT JOIN username"
      ,     "ON username.account = account.id"
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
      [ "SELECT account.id, account.firebase_id, username.id"
      ,   "FROM account"
      ,   "LEFT JOIN username"
      ,     "ON username.account = account.id"
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
      ,   "(id, firebase_id)"
      ,   "VALUES ($1, $2)"
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
      [ "INSERT INTO username"
      ,   "(id, account)"
      ,   "VALUES ($1, $2)"
      ,   "ON CONFLICT (id) DO NOTHING"
      ]
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
      [ "DELETE FROM username"
      ,   "WHERE id = $1"
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
  -> UserInfo
  -> Servant.Handler (Item UserId User)
usersPut conn fuid userId uinfo = do

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId userId)) $ do
      liftIO $ putStrLn $ unwords
        [ "User is trying to update another uinfo:", show (fuid, userId, uinfo) ]
      Servant.throwError Servant.err404

    when (Firebase.unUserId fuid /= unFirebaseId (userInfoFirebaseId uinfo)) $ do
      liftIO $ putStrLn $ unwords
        [ "Client used the wrong uinfo ID on uinfo", show (fuid, userId, uinfo) ]
      Servant.throwError Servant.err400

    iface <- liftIO $ getDbInterface conn
    user <- case userInfoToUser uinfo of
      Left e -> Servant.throwError Servant.err400
        { Servant.errBody = BL.fromStrict $ T.encodeUtf8 e }
      Right user -> pure user
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

-- DECKS

decksGet :: Aws.Env -> Firebase.UserId -> Maybe UserId -> Servant.Handler [Item DeckId Deck]
decksGet env fuid mUserId = do

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

    res <- runAWS env $ Aws.send $ DynamoDB.scan "Decks" &
            DynamoDB.sFilterExpression .~ Just "DeckOwnerId = :o" &
            DynamoDB.sExpressionAttributeValues .~ HMS.singleton ":o" (userIdToAttributeValue userId)

    case res of
      Right scanResponse ->
        case sequence $ scanResponse ^. DynamoDB.srsItems <&> itemToDeck of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show scanResponse
            Servant.throwError Servant.err500
          Just ids -> pure ids
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

-- | TODO: better errors + merge with decksGetDeckId
deckGetDeckIdDB :: Aws.Env -> DeckId -> IO (Maybe Deck)
deckGetDeckIdDB env deckId = do
    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Decks" &
        DynamoDB.giKey .~ HMS.singleton "DeckId" (deckIdToAttributeValue deckId)

    fmap itemContent <$> case res of
      Right getItemResponse ->
        case getItemResponse ^. DynamoDB.girsResponseStatus of
          200 ->
            case itemToDeck (getItemResponse ^. DynamoDB.girsItem) of
              Nothing -> do
                liftIO $ putStrLn $ "Could not parse response: " <> show getItemResponse
                error "Could not parse"
              Just deck -> pure (Just deck)

          404 -> do
            liftIO $ putStrLn $ "Item not found: " <> show getItemResponse
            pure Nothing
          s -> do
            liftIO $
              putStrLn $ "Unkown response status: " <> show s <>
              " in response " <> show getItemResponse
            error "Unknown response status"
      Left e -> do
        liftIO $ print e
        error "Some error"

decksGetDeckId :: Aws.Env -> Firebase.UserId -> DeckId -> Servant.Handler (Item DeckId Deck)
decksGetDeckId env fuid deckId = do

    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Decks" &
        DynamoDB.giKey .~ HMS.singleton "DeckId" (deckIdToAttributeValue deckId)

    deck@Item{itemContent} <- case res of
      Right getItemResponse -> do
        case getItemResponse ^. DynamoDB.girsResponseStatus of
          200 -> pure ()
          404 -> do
            liftIO $ putStrLn $ "Item not found: " <> show getItemResponse
            Servant.throwError Servant.err404
          s -> do
            liftIO $
              putStrLn $ "Unkown response status: " <> show s <>
              " in response " <> show getItemResponse
            Servant.throwError Servant.err500

        case itemToDeck (getItemResponse ^. DynamoDB.girsItem) of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show getItemResponse
            Servant.throwError Servant.err500
          Just deck -> pure deck
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    let ownerId = deckOwnerId itemContent

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId ownerId)) $ do
      liftIO $ putStrLn $ unwords $
        [ "Deck was found", show deck, "but requester is not the owner", show fuid ]
      Servant.throwError Servant.err404

    pure deck

data PresResponse = PresResponse T.Text

instance Aeson.ToJSON PresResponse where
  toJSON (PresResponse t) = Aeson.object [ "url" .= t ]

instance Aeson.FromJSON PresResponse where
  parseJSON = Aeson.withObject "pres-response" $ \o ->
    PresResponse <$> o .: "url"


decksPostPublish
  :: Aws.Env
  -> HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> Servant.Handler PresResponse
  -- TODO: AUTH!!!!
decksPostPublish (fixupEnv -> env) conn _ deckId = do

    -- TODO: check auth

    liftIO $ putStrLn "Your PRESENTATION WAS PUBLISHED!!!!"

    queueName <- liftIO $ T.pack <$> getEnv "QUEUE_NAME"

    liftIO $ putStrLn $ "Forwarding to queue: " <> T.unpack queueName
    queueUrl <- runAWS env (Aws.send $ SQS.getQueueURL queueName) >>= \case
      Right e -> pure $ e ^. SQS.gqursQueueURL
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    liftIO $ print queueUrl

    res <- runAWS env $ Aws.send $ SQS.sendMessage queueUrl $
      T.decodeUtf8 $ BL.toStrict $ Aeson.encode deckId

    case res of
      Right r -> do
        liftIO $ print r
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    presUrl <- liftIO (getEnv "DECKGO_PRESENTATIONS_URL")
    liftIO (deckGetDeckIdDB env deckId) >>= \case
      Nothing -> Servant.throwError Servant.err500
      Just deck -> do
        let dname = deckDeckname deck
        iface <- liftIO $ getDbInterface conn
        liftIO (fmap itemContent <$> dbGetUserById iface (deckOwnerId deck)) >>= \case
          Nothing -> do
            liftIO $ putStrLn "No User Id"
            Servant.throwError Servant.err500
          Just user -> case userUsername user of
            Nothing -> do
              liftIO $ putStrLn "No username"
              Servant.throwError Servant.err500
            Just uname ->
              pure $ PresResponse $ "https://" <> T.pack presUrl <> "/" <> presentationPrefix uname dname

fixupEnv :: Aws.Env -> Aws.Env
fixupEnv = Aws.configure $ SQS.sqs
  { Aws._svcEndpoint = \reg -> do
      let new = "sqs." <> Data.toText reg <> ".amazonaws.com"
      (Aws._svcEndpoint SQS.sqs reg) & Aws.endpointHost .~ T.encodeUtf8 new
  }

decksPost :: Aws.Env -> Firebase.UserId -> Deck -> Servant.Handler (Item DeckId Deck)
decksPost env fuid deck = do

    let ownerId = deckOwnerId deck

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId ownerId)) $ do
      liftIO $ putStrLn $ unwords $
        [ "Deck was POSTed", show deck, "but requester is not the owner", show fuid ]
      Servant.throwError Servant.err400

    deckId <- liftIO $ DeckId <$> newId

    res <- runAWS env $ Aws.send $ DynamoDB.putItem "Decks" &
        DynamoDB.piItem .~ deckToItem deckId deck

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ Item deckId deck

decksPut :: Aws.Env -> Firebase.UserId -> DeckId -> Deck -> Servant.Handler (Item DeckId Deck)
decksPut env fuid deckId deck = do

    getDeck env deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to PUT", show deckId, "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just Deck{deckOwnerId} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Deck was PUTed", show deck, "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Decks" &
        DynamoDB.uiUpdateExpression .~ Just
          (dynamoSet $
            (if isJust (deckDeckbackground deck)
              then [ Set "DeckBackground" ":b" ]
              else [ Remove "DeckBackground" ]) <>
          [ Set "DeckSlides" ":s"
          , Set "DeckName" ":n"
          , Set "DeckOwnerId" ":o"
          , Set "DeckAttributes" ":a"
          ]) &
        DynamoDB.uiExpressionAttributeValues .~ deckToItem' deck &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew &
        DynamoDB.uiKey .~ HMS.singleton "DeckId"
          (deckIdToAttributeValue deckId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ Item deckId deck

decksDelete :: Aws.Env -> Firebase.UserId -> DeckId -> Servant.Handler ()
decksDelete env fuid deckId = do

    getDeck env deckId >>= \case
      Nothing ->  do
        liftIO $ putStrLn $ unwords
          [ "Trying to DELETE", show deckId, "but deck doesn't exist." ]
        Servant.throwError Servant.err404
      Just Deck{deckOwnerId} -> do
        when (Firebase.unUserId fuid /= unFirebaseId (unUserId deckOwnerId)) $ do
          liftIO $ putStrLn $ unwords $
            [ "Deck was DELETEd", show deckId, "but requester is not the owner", show fuid ]
          Servant.throwError Servant.err404

    res <- runAWS env $ Aws.send $ DynamoDB.deleteItem "Decks" &
        DynamoDB.diKey .~ HMS.singleton "DeckId"
          (deckIdToAttributeValue deckId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

-- | Reads a Deck from the database.
--
-- If the deck is not found, returns Nothing
-- If the deck can't be parsed, throws a 500.
-- If the response status is not 200, throws a 500.
getDeck :: Aws.Env -> DeckId -> Servant.Handler (Maybe Deck)
getDeck env deckId = do

    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Decks" &
        DynamoDB.giKey .~ HMS.singleton "DeckId" (deckIdToAttributeValue deckId)

    mItem <- case res of
      Right r -> do
        case
          ( r ^. DynamoDB.girsResponseStatus
          , itemToDeck (r ^. DynamoDB.girsItem )) of
          (200, Just deck) -> pure $ Just deck
          (200, Nothing) -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show r
            Servant.throwError Servant.err500
          (404, _) -> pure Nothing
          s -> do
            liftIO $
              putStrLn $ "Unkown response status: " <> show s <>
              " in response " <> show r
            Servant.throwError Servant.err500
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    case mItem of
      Just Item{itemId = deckId', itemContent = deck} -> do
        when (deckId' /= deckId) $ do
          liftIO $ putStrLn $ "Mismatched deck IDs " <> show (deckId, deckId')
          Servant.throwError Servant.err500
        pure $ Just deck
      Nothing -> pure Nothing

-- SLIDES

slidesPostSession :: SlideId -> Slide -> HS.Session ()
slidesPostSession sid s = do
    liftIO $ putStrLn "Creating slide in DB"
    HS.statement (sid, s) slidesPostStatement

slidesPostStatement :: Statement (SlideId, Slide) ()
slidesPostStatement = Statement sql encoder decoder True
  where
    sql = BS8.unwords
      [ "INSERT INTO slide"
      ,   "(id, content, template, attributes)"
      ,   "VALUES ($1, $2, $3, $4)"
      ]
    encoder =
      contramap (unSlideId . view _1) (HE.param HE.text) <>
      contramap (slideContent . view _2) (HE.nullableParam HE.text) <>
      contramap (slideTemplate . view _2) (HE.param HE.text) <>
      contramap (Aeson.toJSON . slideAttributes . view _2) (HE.param HE.json)
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
  :: Aws.Env
  -> HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> SlideId
  -> Servant.Handler (Item SlideId Slide)
slidesGetSlideId env conn fuid deckId slideId = do

    getDeck env deckId >>= \case
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
  :: Aws.Env
  -> HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> Slide
  -> Servant.Handler (Item SlideId Slide)
slidesPost env conn fuid deckId slide = do
    iface <- liftIO $ getDbInterface conn

    getDeck env deckId >>= \case
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

    liftIO (dbCreateSlide iface slideId slide)

    pure $ Item slideId slide

slidesPut
  :: Aws.Env
  -> HC.Connection
  -> Firebase.UserId
  -> DeckId
  -> SlideId
  -> Slide
  -> Servant.Handler (Item SlideId Slide)
slidesPut env conn fuid deckId slideId slide = do

    iface <- liftIO $ getDbInterface conn

    getDeck env deckId >>= \case
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

slidesDelete :: Aws.Env -> HC.Connection -> Firebase.UserId -> DeckId -> SlideId -> Servant.Handler ()
slidesDelete env conn fuid deckId slideId = do

    getDeck env deckId >>= \case
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
-- DYNAMODB
-------------------------------------------------------------------------------

-- USER ATTRIBUTES

userIdToAttributeValue :: UserId -> DynamoDB.AttributeValue
userIdToAttributeValue (UserId (FirebaseId userId)) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just userId

-- DECKS

deckToItem :: DeckId -> Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem
  deckId
  Deck{deckSlides, deckDeckname, deckDeckbackground, deckOwnerId, deckAttributes} =
    HMS.singleton "DeckId" (deckIdToAttributeValue deckId) <>
    HMS.singleton "DeckSlides" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton "DeckName" (deckNameToAttributeValue deckDeckname) <>
    (maybe
      HMS.empty
      (\content -> HMS.singleton "DeckBackground"
        (deckBackgroundToAttributeValue content))
      deckDeckbackground) <>
    HMS.singleton "DeckOwnerId" (deckOwnerIdToAttributeValue deckOwnerId) <>
    HMS.singleton "DeckAttributes"
      (deckAttributesToAttributeValue deckAttributes)

deckToItem' :: Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem' Deck{deckSlides, deckDeckname, deckDeckbackground, deckOwnerId, deckAttributes} =
    HMS.singleton ":s" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton ":n" (deckNameToAttributeValue deckDeckname) <>
    (maybe
      HMS.empty
      (HMS.singleton ":b" . deckBackgroundToAttributeValue)
      deckDeckbackground) <>
    HMS.singleton ":o" (deckOwnerIdToAttributeValue deckOwnerId) <>
    HMS.singleton ":a" (deckAttributesToAttributeValue deckAttributes)

itemToDeck
  :: HMS.HashMap T.Text DynamoDB.AttributeValue
  -> Maybe (Item DeckId Deck)
itemToDeck item = do
    deckId <- HMS.lookup "DeckId" item >>= deckIdFromAttributeValue
    deckSlides <- HMS.lookup "DeckSlides" item >>= deckSlidesFromAttributeValue
    deckDeckname <- HMS.lookup "DeckName" item >>= deckNameFromAttributeValue

    deckDeckbackground <- case HMS.lookup "DeckBackground" item of
      Nothing -> Just Nothing
      Just c -> Just <$> deckBackgroundFromAttributeValue c

    deckOwnerId <- HMS.lookup "DeckOwnerId" item >>=
      deckOwnerIdFromAttributeValue
    deckAttributes <- HMS.lookup "DeckAttributes" item >>=
      deckAttributesFromAttributeValue
    pure $ Item deckId Deck{..}

-- DECK ATTRIBUTES

deckIdToAttributeValue :: DeckId -> DynamoDB.AttributeValue
deckIdToAttributeValue (DeckId deckId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just deckId

deckIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe DeckId
deckIdFromAttributeValue attr = DeckId <$> attr ^. DynamoDB.avS

deckNameToAttributeValue :: Deckname -> DynamoDB.AttributeValue
deckNameToAttributeValue (Deckname deckname) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just deckname

deckNameFromAttributeValue :: DynamoDB.AttributeValue -> Maybe Deckname
deckNameFromAttributeValue attr = Deckname <$> attr ^. DynamoDB.avS

deckBackgroundToAttributeValue :: Deckbackground -> DynamoDB.AttributeValue
deckBackgroundToAttributeValue (Deckbackground bg) =
    DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 bg)

deckBackgroundFromAttributeValue :: DynamoDB.AttributeValue -> Maybe Deckbackground
deckBackgroundFromAttributeValue attr = toDeckbackground <$> attr ^. DynamoDB.avB
  where
    toDeckbackground = Deckbackground . T.decodeUtf8

deckSlidesToAttributeValue :: [SlideId] -> DynamoDB.AttributeValue
deckSlidesToAttributeValue deckSlides =
    DynamoDB.attributeValue & DynamoDB.avL .~
      (slideIdToAttributeValue <$> deckSlides)

deckSlidesFromAttributeValue :: DynamoDB.AttributeValue -> Maybe [SlideId]
deckSlidesFromAttributeValue attr =
    traverse slideIdFromAttributeValue (attr ^. DynamoDB.avL)

deckOwnerIdToAttributeValue :: UserId -> DynamoDB.AttributeValue
deckOwnerIdToAttributeValue (UserId (FirebaseId deckOwnerId)) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just deckOwnerId

deckOwnerIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe UserId
deckOwnerIdFromAttributeValue attr =
    (UserId . FirebaseId) <$> attr ^. DynamoDB.avS

deckAttributesToAttributeValue
  :: HMS.HashMap T.Text T.Text
  -> DynamoDB.AttributeValue
deckAttributesToAttributeValue attributes =
    DynamoDB.attributeValue & DynamoDB.avM .~
      HMS.map attributeValueToAttributeValue attributes
  where
    attributeValueToAttributeValue :: T.Text -> DynamoDB.AttributeValue
    attributeValueToAttributeValue attrValue =
      DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 attrValue)

deckAttributesFromAttributeValue
  :: DynamoDB.AttributeValue
  -> Maybe (HMS.HashMap T.Text T.Text)
deckAttributesFromAttributeValue attr =
    traverse attributeValueFromAttributeValue (attr ^. DynamoDB.avM)
  where
    attributeValueFromAttributeValue :: DynamoDB.AttributeValue -> Maybe T.Text
    attributeValueFromAttributeValue attrValue =
      T.decodeUtf8 <$> attrValue ^. DynamoDB.avB

-- SLIDES

-- SLIDE ATTRIBUTES

slideIdToAttributeValue :: SlideId -> DynamoDB.AttributeValue
slideIdToAttributeValue (SlideId slideId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just slideId

slideIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe SlideId
slideIdFromAttributeValue attr = SlideId <$> attr ^. DynamoDB.avS

-------------------------------------------------------------------------------
-- DATABASE
-------------------------------------------------------------------------------

data DbInterface = DbInterface
  { dbGetAllUsers :: IO [Item UserId User]
  , dbGetUserById :: UserId -> IO (Maybe (Item UserId User))
  , dbCreateUser :: UserId -> User -> IO (Either () ())
  , dbUpdateUser :: UserId -> User -> IO UserUpdateResult
  , dbDeleteUser :: UserId -> IO (Either () ())
  , dbGetSlideById :: SlideId -> IO (Maybe Slide)
  , dbCreateSlide :: SlideId -> Slide -> IO ()
  , dbUpdateSlide :: SlideId -> Slide -> IO ()
  , dbDeleteSlide :: SlideId -> IO () -- TODO: either () () for not found
  }

data DbVersion
  = DbVersion0
  | DbVersion1
  | DbVersion2
  deriving stock (Enum, Bounded, Ord, Eq)

-- | Migrates from ver to latest
migrateFrom :: DbVersion -> HS.Session ()
migrateFrom = \frm -> do
    liftIO $ putStrLn $ "Migration: " <>
      show (dbVersionToText <$> [frm ..maxBound])
    forM_ [frm .. maxBound] $ \ver -> do
      migrateTo ver
      HS.statement (dbVersionToText ver) $ Statement
        (BS8.unwords
          [ "INSERT INTO db_meta (key, value) VALUES ('version', $1)"
          , "ON CONFLICT (key) DO UPDATE SET value = $1"
          ]
        ) (HE.param HE.text) HD.unit True
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

      , dbGetSlideById = \sid -> wrap (slidesGetByIdSession sid)
      , dbCreateSlide = \sid s -> wrap (slidesPostSession sid s)
      , dbUpdateSlide = \sid s -> wrap (slidesPutSession sid s)
      , dbDeleteSlide = \sid -> wrap (slidesDeleteSession sid)
      }
  where
    wrap :: forall b. HS.Session b -> IO b
    wrap act = HS.run act conn >>= \case
      Left e -> error $ "getDbInterface: error: " <> show e -- TODO
      Right x -> pure x

-- AUX

runAWS :: MonadIO m => Aws.Env -> Aws.AWS a -> m (Either SomeException a)
runAWS env =
    liftIO .
    tryAny .
    Aws.runResourceT .
    Aws.runAWS env .
    Aws.within Aws.NorthVirginia

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

data DynamoUpdateExpr
  = Set T.Text T.Text
  | Remove T.Text

dynamoSet :: [DynamoUpdateExpr] -> T.Text
dynamoSet exprs = T.unwords exprs'
  where
    exprs' = catMaybes [setExpr, removeExpr]
    setExpr = if length sts == 0 then Nothing else Just $
      "SET " <> T.intercalate "," sts
    removeExpr = if length removes == 0 then Nothing else Just $
      "REMOVE " <> T.intercalate "," removes
    (sts, removes) = foldr f ([], []) exprs
    f (Set l r) (ls, rs) = (ls <> [l <> " = " <> r], rs)
    f (Remove t ) (ls, rs) = (ls, rs <> [t])

presentationPrefix :: Username -> Deckname -> T.Text
presentationPrefix uname dname =
    unUsername uname <> "/" <>  sanitizeDeckname dname <> "/"

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

