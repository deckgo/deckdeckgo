{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module DeckGo.Handler where

-- TODO: created_at, updated_at
-- TODO: improve swagger description
-- TODO: feed API

-- TODO: double check what is returned on 200 from DynamoDB
-- TODO: check permissions
-- TODO: TTL on anonymous users
-- TODO: enforce uniqueness on deck_name (per user)

import Data.List (find)
import Control.Lens hiding ((.=))
-- import Data.Int
-- import Data.Functor.Contravariant
-- import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Connection as HC
-- import Control.Lens hiding ((.=))
import Control.Monad
import Data.Maybe
import Control.Monad.Except
import Data.Aeson ((.=), (.:), (.!=), (.:?))
import qualified Data.ByteString.Char8 as BS8
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Servant (Context ((:.)))
import Servant.API
import Servant.Auth.Firebase (Protected)
import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.AWS as Aws
import qualified Network.AWS.DynamoDB as DynamoDB
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai as Wai
import qualified Servant as Servant
import qualified Servant.Auth.Firebase as Firebase
import qualified System.Random as Random

data ServerContext = ServerContext { firebaseProjectId :: Firebase.ProjectId }

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
      ReqBody '[JSON] User :>
      Post '[JSON] (Item UserId User) :<|>
    Protected :>
      Capture "user_id" UserId :>
      ReqBody '[JSON] User :> Put '[JSON] (Item UserId User) :<|>
    Protected :> Capture "user_id" UserId :> Delete '[JSON] ()

newtype Username = Username { unUsername :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data User = User
  { userFirebaseId :: FirebaseId
  , userAnonymous :: Bool
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

instance FromJSONObject User where
  parseJSONObject = \obj ->
    User
      -- potentially return "error exists" + user object
      <$> obj .: "firebase_uid"
      <*> obj .: "anonymous"

instance ToJSONObject User where
  toJSONObject user = HMS.fromList
    [ "firebase_uid" .= userFirebaseId user
    , "anonymous" .= userAnonymous user
    ]

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" parseJSONObject

instance Aeson.ToJSON User where
  toJSON = Aeson.Object . toJSONObject

instance ToSchema (Item UserId User) where
  declareNamedSchema _ = pure $ NamedSchema (Just "UserWithId") mempty

instance ToSchema User where
  declareNamedSchema _ = pure $ NamedSchema (Just "User") mempty

instance ToParamSchema (Item UserId User) where
  toParamSchema _ = mempty

instance ToParamSchema UserId where
  toParamSchema _ = mempty

-- DECKS

type DecksAPI =
    Protected :> QueryParam "owner_id" UserId :> Get '[JSON] [Item DeckId Deck] :<|>
    Protected :>
      Capture "deck_id" DeckId :>
      Get '[JSON] (Item DeckId Deck) :<|>
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

data Deck = Deck
  { deckSlides :: [SlideId]
  , deckDeckname :: Deckname
  , deckOwnerId :: UserId
  , deckAttributes :: HMS.HashMap T.Text T.Text
  } deriving (Show, Eq)


{-
data Deck = Deck
  { deckSlides :: [SlideId]
  , deckOwnerId :: UserId
  , deckAttributes :: HMS.HashMap T.Text T.Text
  , deckTitle :: T.Text
  , deckDescription :: Maybe T.Text
  , deckAuthor :: Maybe T.Text
  , deckHashTags :: [CI T.Text]
  , deckPublicationDate :: Maybe UTCTime
  } deriving (Show, Eq)
-}



-- /decks/<deck-id>/publish

instance FromJSONObject Deck where
  parseJSONObject = \obj ->
    Deck
      <$> obj .: "slides"
      <*> obj .: "name"
      <*> obj .: "owner_id"
      <*> obj .:? "attributes" .!= HMS.empty

instance ToJSONObject Deck where
  toJSONObject deck = HMS.fromList
    [ "slides" .= deckSlides deck
    , "name" .= deckDeckname deck
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

type API =
    "users" :> UsersAPI :<|>
    "decks" :> DecksAPI :<|>
    "decks" :> SlidesAPI

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

application :: HTTP.Manager -> Firebase.ProjectId -> Aws.Env -> HC.Connection -> Wai.Application
application mgr projectId env conn =
    Servant.serveWithContext
      api
      (mgr :. projectId :. Servant.EmptyContext)
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
      decksPost env :<|>
      decksPut env :<|>
      decksDelete env
    serveSlides =
      slidesGetSlideId env :<|>
      slidesPost env :<|>
      slidesPut env :<|>
      slidesDelete env

-- USERS

usersGet :: HC.Connection -> Servant.Handler [Item UserId User]
usersGet conn = do
    liftIO (HS.run usersGetSession conn) >>= \case
      Right users -> pure users
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

usersGetSession :: HS.Session [Item UserId User]
usersGetSession = do
    HS.statement () usersGetStatement

usersGetStatement :: Statement () [Item UserId User]
usersGetStatement = Statement sql encoder decoder True
  where
    sql = "SELECT * FROM account"
    encoder = HE.unit
    decoder = HD.rowList $
      Item <$>
        ((UserId . FirebaseId) <$> HD.column HD.text) <*>
        ( User <$>
          (FirebaseId <$> HD.column HD.text) <*>
          HD.column HD.bool
        )

usersGetUserId :: HC.Connection -> UserId -> Servant.Handler (Item UserId User)
usersGetUserId conn userId = do
    liftIO (HS.run (usersGetUserIdSession userId) conn) >>= \case
      Right user -> pure user
      Left e -> do -- TODO: handle not found et al.
        liftIO $ print e
        Servant.throwError Servant.err500

usersGetUserIdSession :: UserId -> HS.Session (Item UserId User)
usersGetUserIdSession userId = do
    HS.statement userId usersGetUserIdStatement

usersGetUserIdStatement :: Statement UserId (Item UserId User)
usersGetUserIdStatement = Statement sql encoder decoder True
  where
    sql = "SELECT * FROM account WHERE id = $1"
    encoder = contramap
        (unFirebaseId . unUserId)
        (HE.param HE.text)
    decoder = HD.singleRow $
      Item <$>
        ((UserId . FirebaseId) <$> HD.column HD.text) <*>
        ( User <$>
          (FirebaseId <$> HD.column HD.text) <*>
          HD.column HD.bool
        )

usersPost :: HC.Connection -> Firebase.UserId -> User -> Servant.Handler (Item UserId User)
usersPost conn fuid user = do
    let userId = UserId (userFirebaseId user)

    when (Firebase.unUserId fuid /= unFirebaseId (userFirebaseId user)) $ do
      Servant.throwError Servant.err403

    liftIO (HS.run (usersPostSession userId user) conn) >>= \case
      Right () -> pure $ Item userId user
      Left e -> do -- TODO: handle not found et al.
        liftIO $ print e
        Servant.throwError Servant.err500

usersPostSession :: UserId -> User -> HS.Session ()
usersPostSession uid u = do
    HS.statement (uid,u) usersPostStatement

usersPostStatement :: Statement (UserId, User) ()
usersPostStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO account (id, firebase_id, anonymous) VALUES ($1, $2, $3)"
    encoder =
      contramap
        (unFirebaseId . unUserId . view _1)
        (HE.param HE.text) <>
      contramap (unFirebaseId . userFirebaseId . view _2) (HE.param HE.text) <>
      contramap (userAnonymous . view _2) (HE.param HE.bool)
    decoder = HD.unit -- TODO: affected rows

usersPut :: HC.Connection -> Firebase.UserId -> UserId -> User -> Servant.Handler (Item UserId User)
usersPut conn fuid userId user = do

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId userId)) $ do
      liftIO $ putStrLn $ unwords
        [ "User is trying to update another user:", show (fuid, userId, user) ]
      Servant.throwError Servant.err404

    when (Firebase.unUserId fuid /= unFirebaseId (userFirebaseId user)) $ do
      liftIO $ putStrLn $ unwords
        [ "Client used the wrong user ID on user", show (fuid, userId, user) ]
      Servant.throwError Servant.err400

    liftIO (HS.run (usersPutSession userId user) conn) >>= \case
      Right () -> pure $ Item userId user -- TODO: check # of affected rows
      Left e -> do -- TODO: handle not found et al.
        liftIO $ print e
        Servant.throwError Servant.err500

    -- pure $ Item userId user

usersPutSession :: UserId -> User -> HS.Session ()
usersPutSession uid u = do
    HS.statement (uid,u) usersPutStatement

usersPutStatement :: Statement (UserId, User) ()
usersPutStatement = Statement sql encoder decoder True
  where
    sql = "UPDATE account SET firebase_id = $2, anonymous = $3 WHERE id = $1"
    encoder =
      contramap
        (unFirebaseId . unUserId . view _1)
        (HE.param HE.text) <>
      contramap (unFirebaseId . userFirebaseId . view _2) (HE.param HE.text) <>
      contramap (userAnonymous . view _2) (HE.param HE.bool)
    decoder = HD.unit -- TODO: affected rows

usersDelete :: HC.Connection -> Firebase.UserId -> UserId -> Servant.Handler ()
usersDelete conn fuid userId = do

    when (Firebase.unUserId fuid /= unFirebaseId (unUserId userId)) $ do
      Servant.throwError Servant.err403

    liftIO (HS.run (usersDeleteSession userId) conn) >>= \case
      Right () -> pure () -- TODO: check # of affected rows
      Left e -> do -- TODO: handle not found et al.
        liftIO $ print e
        Servant.throwError Servant.err500

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
        DynamoDB.uiUpdateExpression .~ Just "SET DeckSlides = :s, DeckName = :n, DeckOwnerId = :o, DeckAttributes = :a" &
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

slidesGetSlideId :: Aws.Env -> Firebase.UserId -> DeckId -> SlideId -> Servant.Handler (Item SlideId Slide)
slidesGetSlideId env fuid deckId slideId = do

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

    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Slides" &
        DynamoDB.giKey .~ HMS.singleton "SlideId" (slideIdToAttributeValue slideId)
    case res of
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

        case itemToSlide (getItemResponse ^. DynamoDB.girsItem) of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show getItemResponse
            Servant.throwError Servant.err500
          Just slide -> pure slide
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

slidesPost :: Aws.Env -> Firebase.UserId -> DeckId -> Slide -> Servant.Handler (Item SlideId Slide)
slidesPost env fuid deckId slide = do

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

    res <- runAWS env $
      Aws.send $ DynamoDB.putItem "Slides" &
        DynamoDB.piItem .~ slideToItem slideId slide

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ Item slideId slide

slidesPut
  :: Aws.Env
  -> Firebase.UserId
  -> DeckId
  -> SlideId
  -> Slide
  -> Servant.Handler (Item SlideId Slide)
slidesPut env fuid deckId slideId slide = do

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

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Slides" &
        DynamoDB.uiUpdateExpression .~ Just
          (dynamoSet $
            (if isJust (slideContent slide)
              then [ Set "SlideContent" ":c" ]
              else [ Remove "SlideContent" ]) <>
            [ Set "SlideTemplate" ":t", Set "SlideAttributes" ":a"]) &
        DynamoDB.uiExpressionAttributeValues .~ slideToItem' slide &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew &
        DynamoDB.uiKey .~ HMS.singleton "SlideId"
          (slideIdToAttributeValue slideId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ Item slideId slide

slidesDelete :: Aws.Env -> Firebase.UserId -> DeckId -> SlideId -> Servant.Handler ()
slidesDelete env fuid deckId slideId = do

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

    res <- runAWS env $ Aws.send $ DynamoDB.deleteItem "Slides" &
        DynamoDB.diKey .~  HMS.singleton "SlideId"
          (slideIdToAttributeValue slideId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

-------------------------------------------------------------------------------
-- DYNAMODB
-------------------------------------------------------------------------------

-- USER ATTRIBUTES

userIdToAttributeValue :: UserId -> DynamoDB.AttributeValue
userIdToAttributeValue (UserId (FirebaseId userId)) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just userId

-- DECKS

deckToItem :: DeckId -> Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem deckId Deck{deckSlides, deckDeckname, deckOwnerId, deckAttributes} =
    HMS.singleton "DeckId" (deckIdToAttributeValue deckId) <>
    HMS.singleton "DeckSlides" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton "DeckName" (deckNameToAttributeValue deckDeckname) <>
    HMS.singleton "DeckOwnerId" (deckOwnerIdToAttributeValue deckOwnerId) <>
    HMS.singleton "DeckAttributes"
      (deckAttributesToAttributeValue deckAttributes)

deckToItem' :: Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem' Deck{deckSlides, deckDeckname, deckOwnerId, deckAttributes} =
    HMS.singleton ":s" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton ":n" (deckNameToAttributeValue deckDeckname) <>
    HMS.singleton ":o" (deckOwnerIdToAttributeValue deckOwnerId) <>
    HMS.singleton ":a" (deckAttributesToAttributeValue deckAttributes)

itemToDeck
  :: HMS.HashMap T.Text DynamoDB.AttributeValue
  -> Maybe (Item DeckId Deck)
itemToDeck item = do
    deckId <- HMS.lookup "DeckId" item >>= deckIdFromAttributeValue
    deckSlides <- HMS.lookup "DeckSlides" item >>= deckSlidesFromAttributeValue
    deckDeckname <- HMS.lookup "DeckName" item >>= deckNameFromAttributeValue
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

slideToItem :: SlideId -> Slide -> HMS.HashMap T.Text DynamoDB.AttributeValue
slideToItem slideId Slide{slideContent, slideTemplate, slideAttributes} =
    HMS.singleton "SlideId" (slideIdToAttributeValue slideId) <>
    (maybe
      HMS.empty
      (\content -> HMS.singleton "SlideContent"
        (slideContentToAttributeValue content))
      slideContent) <>
    HMS.singleton "SlideTemplate"
      (slideTemplateToAttributeValue slideTemplate) <>
    HMS.singleton "SlideAttributes"
      (slideAttributesToAttributeValue slideAttributes)

slideToItem' :: Slide -> HMS.HashMap T.Text DynamoDB.AttributeValue
slideToItem' Slide{slideContent, slideTemplate, slideAttributes} =
    (maybe
      HMS.empty
      (\content -> HMS.singleton ":c" (slideContentToAttributeValue content))
      slideContent) <>
    HMS.singleton ":t" (slideTemplateToAttributeValue slideTemplate) <>
    HMS.singleton ":a" (slideAttributesToAttributeValue slideAttributes)

itemToSlide
  :: HMS.HashMap T.Text DynamoDB.AttributeValue
  -> Maybe (Item SlideId Slide)
itemToSlide item = do
    slideId <- HMS.lookup "SlideId" item >>= slideIdFromAttributeValue

    slideContent <- case HMS.lookup "SlideContent" item of
      Nothing -> Just Nothing
      Just c -> Just <$> slideContentFromAttributeValue c

    slideTemplate <- HMS.lookup "SlideTemplate" item >>=
      slideTemplateFromAttributeValue
    slideAttributes <- HMS.lookup "SlideAttributes" item >>=
      slideAttributesFromAttributeValue

    pure $ Item slideId Slide{..}

-- SLIDE ATTRIBUTES

slideIdToAttributeValue :: SlideId -> DynamoDB.AttributeValue
slideIdToAttributeValue (SlideId slideId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just slideId

slideIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe SlideId
slideIdFromAttributeValue attr = SlideId <$> attr ^. DynamoDB.avS

slideContentToAttributeValue :: T.Text -> DynamoDB.AttributeValue
slideContentToAttributeValue content =
    DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 content)

slideContentFromAttributeValue :: DynamoDB.AttributeValue -> Maybe T.Text
slideContentFromAttributeValue attr = toSlideContent <$> attr ^. DynamoDB.avB
  where
    toSlideContent = T.decodeUtf8

slideTemplateToAttributeValue :: T.Text -> DynamoDB.AttributeValue
slideTemplateToAttributeValue content =
    DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 content)

slideTemplateFromAttributeValue :: DynamoDB.AttributeValue -> Maybe T.Text
slideTemplateFromAttributeValue attr = toSlideTemplate <$> attr ^. DynamoDB.avB
  where
    toSlideTemplate = T.decodeUtf8

slideAttributesToAttributeValue
  :: HMS.HashMap T.Text T.Text
  -> DynamoDB.AttributeValue
slideAttributesToAttributeValue attributes =
    DynamoDB.attributeValue & DynamoDB.avM .~
      HMS.map attributeValueToAttributeValue attributes
  where
    attributeValueToAttributeValue :: T.Text -> DynamoDB.AttributeValue
    attributeValueToAttributeValue attrValue =
      DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 attrValue)

slideAttributesFromAttributeValue
  :: DynamoDB.AttributeValue
  -> Maybe (HMS.HashMap T.Text T.Text)
slideAttributesFromAttributeValue attr =
    traverse attributeValueFromAttributeValue (attr ^. DynamoDB.avM)
  where
    attributeValueFromAttributeValue :: DynamoDB.AttributeValue -> Maybe T.Text
    attributeValueFromAttributeValue attrValue =
      T.decodeUtf8 <$> attrValue ^. DynamoDB.avB

-------------------------------------------------------------------------------
-- DATABASE
-------------------------------------------------------------------------------

data DbInterface = DbInterface
  { dbGetAllUsers :: IO [Item UserId User]
  , dbGetUserById :: UserId -> IO (Maybe (Item UserId User))
  , dbCreateUser :: UserId -> User -> IO (Either () ())
  , dbUpdateUser :: UserId -> User -> IO (Either () ())
  , dbDeleteUser :: UserId -> IO (Either () ())
  }

data DbVersion
  = DbVersion0
  deriving stock (Enum, Bounded, Ord, Eq)

-- | Migrates from ver to latest
migrateFrom :: DbVersion -> HS.Session ()
migrateFrom ver = forM_ [ver .. maxBound] migrateTo

-- | Migrates from (ver -1) to ver
migrateTo :: DbVersion -> HS.Session ()
migrateTo = \case
  DbVersion0 -> HS.statement () $ Statement
    (BS8.unwords
      [ "CREATE TABLE account"
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
        tver <- HS.statement () $ Statement
          "SELECT value FROM db_meta WHERE key = 'version'"
          HE.unit (HD.singleRow (HD.column HD.text)) True
        case dbVersionFromText tver of
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
        migrateFrom minBound
        pure $ Right $ Just maxBound

latestDbVersion :: DbVersion
latestDbVersion = maxBound

dbVersionToText :: DbVersion -> T.Text
dbVersionToText = \case
  DbVersion0 -> "0"

dbVersionFromText :: T.Text -> Maybe DbVersion
dbVersionFromText t =
    find (\ver -> dbVersionToText ver == t) [minBound .. maxBound]

getDbInterface :: HC.Connection -> DbInterface
getDbInterface conn = DbInterface
    { dbGetAllUsers = wrap usersGetSession
    , dbGetUserById = \uid -> Just <$> wrap (usersGetUserIdSession uid)
    , dbCreateUser = \uid user -> Right <$> wrap (usersPostSession uid user)
    , dbUpdateUser = \uid user -> Right <$> wrap (usersPutSession uid user)
    , dbDeleteUser = \uid -> Right <$> wrap (usersDeleteSession uid)
    }
  where
    wrap :: forall b. HS.Session b -> IO b
    wrap act = HS.run act conn >>= \case
      Left{} -> undefined -- TODO
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
