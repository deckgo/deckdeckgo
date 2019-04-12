{-# LANGUAGE DataKinds #-}
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

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Data.Aeson ((.=), (.:), (.!=), (.:?))
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
    Get '[JSON] [Item SlideId Slide] :<|>
    Capture "slide_id" SlideId :> Get '[JSON] (Item SlideId Slide) :<|>
    ReqBody '[JSON] Slide :> Post '[JSON] (Item SlideId Slide) :<|>
    Capture "slide_id" SlideId :> ReqBody '[JSON] Slide :> Put '[JSON] (Item SlideId Slide) :<|>
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
  { slideContent :: T.Text
  , slideTemplate :: T.Text
  , slideAttributes :: HMS.HashMap T.Text T.Text
  } deriving (Show, Eq)

instance FromJSONObject Slide where
  parseJSONObject = \obj ->
    Slide <$>
      obj .:? "content" .!= "" <*>
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
    "slides" :> SlidesAPI

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

application :: HTTP.Manager -> Firebase.ProjectId -> Aws.Env -> Wai.Application
application mgr projectId env =
    Servant.serveWithContext
      api
      (mgr :. projectId :. Servant.EmptyContext)
      (server env)

server :: Aws.Env -> Servant.Server API
server env = serveUsers :<|> serveDecks :<|> serveSlides
  where
    serveUsers =
      usersGet env :<|>
      usersGetUserId env :<|>
      usersPost env :<|>
      usersPut env :<|>
      usersDelete env
    serveDecks =
      decksGet env :<|>
      decksGetDeckId env :<|>
      decksPost env :<|>
      decksPut env :<|>
      decksDelete env
    serveSlides =
      slidesGet env :<|>
      slidesGetSlideId env :<|>
      slidesPost env :<|>
      slidesPut env :<|>
      slidesDelete env

-- USERS

usersGet :: Aws.Env -> Servant.Handler [Item UserId User]
usersGet env = do
    res <- runAWS env $ Aws.send $ DynamoDB.scan "Users"
    case res of
      Right scanResponse ->
        case sequence $ scanResponse ^. DynamoDB.srsItems <&> itemToUser of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show scanResponse
            Servant.throwError Servant.err500
          Just ids -> pure ids
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

usersGetUserId :: Aws.Env -> UserId -> Servant.Handler (Item UserId User)
usersGetUserId env userId = do
    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Users" &
        DynamoDB.giKey .~ HMS.singleton "UserFirebaseId" (userIdToAttributeValue userId)
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

        case itemToUser (getItemResponse ^. DynamoDB.girsItem) of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show getItemResponse
            Servant.throwError Servant.err500
          Just user -> pure user
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

usersPost :: Aws.Env -> Firebase.UserId -> User -> Servant.Handler (Item UserId User)
usersPost env fuid user = do

    let userId = UserId (userFirebaseId user)

    when (Firebase.unUserId fuid /= unFirebaseId (userFirebaseId user)) $ do
      Servant.throwError Servant.err403

    res <- runAWS env $ Aws.send $ DynamoDB.putItem "Users" &
        DynamoDB.piItem .~ userToItem userId user &
        DynamoDB.piConditionExpression .~ Just "attribute_not_exists(UserFirebaseId)"

    case res of
      Right {} -> pure ()
      Left e -> case e ^? DynamoDB._ConditionalCheckFailedException of
        Just _e -> do
          u <- usersGetUserId env userId
          Servant.throwError Servant.err409 { Servant.errBody = Aeson.encode u }
        Nothing -> do
          liftIO $ print e
          Servant.throwError Servant.err500

    pure $ Item userId user

usersPut :: Aws.Env -> Firebase.UserId -> UserId -> User -> Servant.Handler (Item UserId User)
usersPut env _ userId user = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Users" &
        DynamoDB.uiUpdateExpression .~
          Just "SET UserDecks = :s, UserUsername = :n" &
        DynamoDB.uiExpressionAttributeValues .~ userToItem' user &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew &
        DynamoDB.uiKey .~ HMS.singleton "UserId"
          (userIdToAttributeValue userId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ Item userId user

usersDelete :: Aws.Env -> Firebase.UserId -> UserId -> Servant.Handler ()
usersDelete env _ userId = do

    res <- runAWS env $ Aws.send $ DynamoDB.deleteItem "Users" &
        DynamoDB.diKey .~ HMS.singleton "UserFirebaseId"
          (userIdToAttributeValue userId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

-- DECKS

decksGet :: Aws.Env -> Firebase.UserId -> Maybe UserId -> Servant.Handler [Item DeckId Deck]
decksGet env _uid mUserId = do

    let updateReq = case mUserId of
          Nothing -> id
          Just userId -> \req -> req &
            DynamoDB.sFilterExpression .~ Just "DeckOwnerId = :o" &
            DynamoDB.sExpressionAttributeValues .~ HMS.singleton ":o" (userIdToAttributeValue userId)

    res <- runAWS env $ Aws.send $ updateReq $ DynamoDB.scan "Decks"
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
decksGetDeckId env _ deckId = do
    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Decks" &
        DynamoDB.giKey .~ HMS.singleton "DeckId" (deckIdToAttributeValue deckId)
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

        case itemToDeck (getItemResponse ^. DynamoDB.girsItem) of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse response: " <> show getItemResponse
            Servant.throwError Servant.err500
          Just deck -> pure deck
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

decksPost :: Aws.Env -> Firebase.UserId -> Deck -> Servant.Handler (Item DeckId Deck)
decksPost env _ deck = do

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
decksPut env _ deckId deck = do

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
decksDelete env _ deckId = do

    res <- runAWS env $ Aws.send $ DynamoDB.deleteItem "Decks" &
        DynamoDB.diKey .~ HMS.singleton "DeckId"
          (deckIdToAttributeValue deckId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

-- SLIDES

slidesGet :: Aws.Env -> Servant.Handler [Item SlideId Slide]
slidesGet env = do
    res <- runAWS env $ Aws.send $ DynamoDB.scan "Slides"
    case res of
      Right scanResponse ->
        case sequence $ scanResponse ^. DynamoDB.srsItems <&> itemToSlide of
          Nothing -> do
            liftIO $ putStrLn $ "Could not parse respose: " <> show scanResponse
            Servant.throwError Servant.err500
          Just ids -> pure ids

      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

slidesGetSlideId :: Aws.Env -> SlideId -> Servant.Handler (Item SlideId Slide)
slidesGetSlideId env slideId = do
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

slidesPost :: Aws.Env -> Slide -> Servant.Handler (Item SlideId Slide)
slidesPost env slide = do
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

slidesPut :: Aws.Env -> SlideId -> Slide -> Servant.Handler (Item SlideId Slide)
slidesPut env slideId slide = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Slides" &
        DynamoDB.uiUpdateExpression .~ Just
          "SET SlideContent = :c, SlideTemplate = :t, SlideAttributes = :a" &
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

slidesDelete :: Aws.Env -> SlideId -> Servant.Handler ()
slidesDelete env slideId = do

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

-- USERS

userToItem :: UserId -> User -> HMS.HashMap T.Text DynamoDB.AttributeValue
userToItem userId User{userAnonymous} =
    HMS.singleton "UserFirebaseId" (userIdToAttributeValue userId) <>
    HMS.singleton "UserAnonymous" (userAnonymousToAttributeValue userAnonymous)

userToItem' :: User -> HMS.HashMap T.Text DynamoDB.AttributeValue
userToItem' User{userAnonymous} =
    HMS.singleton ":a" (userAnonymousToAttributeValue userAnonymous)

itemToUser :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (Item UserId User)
itemToUser item = do
    userId <- HMS.lookup "UserFirebaseId" item >>= userIdFromAttributeValue
    let userFirebaseId = unUserId userId
    userAnonymous <- HMS.lookup "UserAnonymous" item >>= userAnonymousFromAttributeValue
    pure $ Item userId User{..}

-- USER ATTRIBUTES

userIdToAttributeValue :: UserId -> DynamoDB.AttributeValue
userIdToAttributeValue (UserId (FirebaseId userId)) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just userId

userIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe UserId
userIdFromAttributeValue attr = (UserId . FirebaseId) <$> attr ^. DynamoDB.avS

userNameToAttributeValue :: Username -> DynamoDB.AttributeValue
userNameToAttributeValue (Username username) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just username

userNameFromAttributeValue :: DynamoDB.AttributeValue -> Maybe Username
userNameFromAttributeValue attr = Username <$> attr ^. DynamoDB.avS

userFirebaseIdToAttributeValue :: FirebaseId -> DynamoDB.AttributeValue
userFirebaseIdToAttributeValue (FirebaseId userId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just userId

userFirebaseIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe FirebaseId
userFirebaseIdFromAttributeValue attr = FirebaseId <$> attr ^. DynamoDB.avS

userAnonymousToAttributeValue :: Bool -> DynamoDB.AttributeValue
userAnonymousToAttributeValue b =
    DynamoDB.attributeValue & DynamoDB.avBOOL .~ Just b

userAnonymousFromAttributeValue :: DynamoDB.AttributeValue -> Maybe Bool
userAnonymousFromAttributeValue attr = attr ^. DynamoDB.avBOOL

userDecksToAttributeValue :: [DeckId] -> DynamoDB.AttributeValue
userDecksToAttributeValue userDecks =
    DynamoDB.attributeValue & DynamoDB.avL .~
      (deckIdToAttributeValue <$> userDecks)

userDecksFromAttributeValue :: DynamoDB.AttributeValue -> Maybe [DeckId]
userDecksFromAttributeValue attr =
    traverse deckIdFromAttributeValue (attr ^. DynamoDB.avL)

-- DECKS

deckToItem :: DeckId -> Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem deckId Deck{deckSlides, deckDeckname, deckOwnerId, deckAttributes} =
    HMS.singleton "DeckId" (deckIdToAttributeValue deckId) <>
    HMS.singleton "DeckSlides" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton "DeckName" (deckNameToAttributeValue deckDeckname) <>
    HMS.singleton "DeckOwnerId" (deckOwnerIdToAttributeValue deckOwnerId) <>
    HMS.singleton "DeckAttributes" (deckAttributesToAttributeValue deckAttributes)

deckToItem' :: Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem' Deck{deckSlides, deckDeckname, deckOwnerId, deckAttributes} =
    HMS.singleton ":s" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton ":n" (deckNameToAttributeValue deckDeckname) <>
    HMS.singleton ":o" (deckOwnerIdToAttributeValue deckOwnerId) <>
    HMS.singleton ":a" (deckAttributesToAttributeValue deckAttributes)

itemToDeck :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (Item DeckId Deck)
itemToDeck item = do
    deckId <- HMS.lookup "DeckId" item >>= deckIdFromAttributeValue
    deckSlides <- HMS.lookup "DeckSlides" item >>= deckSlidesFromAttributeValue
    deckDeckname <- HMS.lookup "DeckName" item >>= deckNameFromAttributeValue
    deckOwnerId <- HMS.lookup "DeckOwnerId" item >>= deckOwnerIdFromAttributeValue
    deckAttributes <- HMS.lookup "DeckAttributes" item >>= deckAttributesFromAttributeValue
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
deckOwnerIdFromAttributeValue attr = (UserId . FirebaseId) <$> attr ^. DynamoDB.avS

deckAttributesToAttributeValue :: HMS.HashMap T.Text T.Text -> DynamoDB.AttributeValue
deckAttributesToAttributeValue attributes =
    DynamoDB.attributeValue & DynamoDB.avM .~
      HMS.map attributeValueToAttributeValue attributes
  where
    attributeValueToAttributeValue :: T.Text -> DynamoDB.AttributeValue
    attributeValueToAttributeValue attrValue =
      DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 attrValue)

deckAttributesFromAttributeValue :: DynamoDB.AttributeValue -> Maybe (HMS.HashMap T.Text T.Text)
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
    HMS.singleton "SlideContent" (slideContentToAttributeValue slideContent) <>
    HMS.singleton "SlideTemplate" (slideTemplateToAttributeValue slideTemplate) <>
    HMS.singleton "SlideAttributes" (slideAttributesToAttributeValue slideAttributes)

slideToItem' :: Slide -> HMS.HashMap T.Text DynamoDB.AttributeValue
slideToItem' Slide{slideContent, slideTemplate, slideAttributes} =
    HMS.singleton ":c" (slideContentToAttributeValue slideContent) <>
    HMS.singleton ":t" (slideTemplateToAttributeValue slideTemplate) <>
    HMS.singleton ":a" (slideAttributesToAttributeValue slideAttributes)

itemToSlide :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (Item SlideId Slide)
itemToSlide item = do
    slideId <- HMS.lookup "SlideId" item >>= slideIdFromAttributeValue

    slideContent <- HMS.lookup "SlideContent" item >>= slideContentFromAttributeValue

    slideTemplate <- HMS.lookup "SlideTemplate" item >>= slideTemplateFromAttributeValue
    slideAttributes <- HMS.lookup "SlideAttributes" item >>= slideAttributesFromAttributeValue

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

slideAttributesToAttributeValue :: HMS.HashMap T.Text T.Text -> DynamoDB.AttributeValue
slideAttributesToAttributeValue attributes =
    DynamoDB.attributeValue & DynamoDB.avM .~
      HMS.map attributeValueToAttributeValue attributes
  where
    attributeValueToAttributeValue :: T.Text -> DynamoDB.AttributeValue
    attributeValueToAttributeValue attrValue =
      DynamoDB.attributeValue & DynamoDB.avB .~ Just (T.encodeUtf8 attrValue)

slideAttributesFromAttributeValue :: DynamoDB.AttributeValue -> Maybe (HMS.HashMap T.Text T.Text)
slideAttributesFromAttributeValue attr =
    traverse attributeValueFromAttributeValue (attr ^. DynamoDB.avM)
  where
    attributeValueFromAttributeValue :: DynamoDB.AttributeValue -> Maybe T.Text
    attributeValueFromAttributeValue attrValue =
      T.decodeUtf8 <$> attrValue ^. DynamoDB.avB

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
