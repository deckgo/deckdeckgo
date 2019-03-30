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

-- TODO: double check what is returned on 200 from DynamoDB

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


data WithId id a = WithId id a
  deriving (Show, Eq, Generic)

-- USERS

type UsersAPI =
    Get '[JSON] [WithId UserId User] :<|>
    Capture "user_id" UserId :> Get '[JSON] (WithId UserId User) :<|>
    Protected :>
      ReqBody '[JSON] User :>
      Post '[JSON] (WithId UserId User) :<|>
    Protected :>
      Capture "user_id" UserId :>
      ReqBody '[JSON] User :> Put '[JSON] (WithId UserId User) :<|>
    Protected :> Capture "user_id" UserId :> Delete '[JSON] ()

newtype Username = Username { unUsername :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data User = User
  { userDecks :: [DeckId]
  , userFirebaseId :: FirebaseId -- TODO: enforce uniqueness
  , userUsername :: Username
  } deriving (Show, Eq)

newtype UserId = UserId { unUserId :: T.Text }
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

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "user" $ \obj ->
    User
      <$> obj .: "user_username"
      <*> obj .: "user_decks"
      <*> obj .: "user_firebaseid"

instance Aeson.ToJSON User where
  toJSON user = Aeson.object
    [ "user_username" .= userUsername user
    , "user_decks" .= userDecks user
    , "user_firebaseid" .= userFirebaseId user
    ]

-- TODO: deduplicate those instances
instance Aeson.FromJSON (WithId UserId User) where
  parseJSON = Aeson.withObject "WithId UserId User" $ \o ->
    WithId <$>
      (UserId <$> o .: "user_id") <*>
      (User <$> o .: "user_decks" <*> o .: "user_username" <*> o .: "user_firebaseid")

instance Aeson.ToJSON (WithId UserId User) where
  toJSON (WithId userId user) = Aeson.object
    [ "user_id" .= userId
    , "user_decks" .= userDecks user
    , "user_name" .= userUsername user
    ]

-- DECKS

type DecksAPI =
    Protected :> Get '[JSON] [WithId DeckId Deck] :<|>
    Protected :>
      Capture "deck_id" DeckId :>
      Get '[JSON] (WithId DeckId Deck) :<|>
    ReqBody '[JSON] Deck :> Post '[JSON] (WithId DeckId Deck) :<|> --TODO: protect
    Protected :>
      Capture "deck_id" DeckId :>
      ReqBody '[JSON] Deck :> Put '[JSON] (WithId DeckId Deck) :<|>
    Protected :> Capture "deck_id" DeckId :> Delete '[JSON] ()

newtype DeckId = DeckId { unDeckId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, FromHttpApiData, ToHttpApiData, Show, Eq)

newtype Deckname = Deckname { unDeckname :: T.Text }
  deriving stock (Show, Eq)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data Deck = Deck
  { deckSlides :: [SlideId]
  , deckDeckname :: Deckname -- TODO: enforce uniqueness
  } deriving (Show, Eq)

instance Aeson.FromJSON Deck where
  parseJSON = Aeson.withObject "deck" $ \obj ->
    Deck
      <$> obj .: "deck_slides"
      <*> obj .: "deck_name"

instance Aeson.ToJSON Deck where
  toJSON deck = Aeson.object
    [ "deck_slides" .= deckSlides deck
    , "deck_name" .= deckDeckname deck
    ]

-- TODO: deduplicate those instances
instance Aeson.FromJSON (WithId DeckId Deck) where
  parseJSON = Aeson.withObject "WithId DeckId Deck" $ \o ->
    WithId <$>
      (DeckId <$> o .: "deck_id") <*>
      (Deck <$> o .: "deck_slides" <*> o .: "deck_name")

instance Aeson.ToJSON (WithId DeckId Deck) where
  toJSON (WithId deckId deck) = Aeson.object
    [ "deck_id" .= deckId
    , "deck_slides" .= deckSlides deck
    , "deck_name" .= deckDeckname deck
    ]

-- SLIDES

type SlidesAPI =
    Get '[JSON] [WithId SlideId Slide] :<|>
    Capture "slide_id" SlideId :> Get '[JSON] (WithId SlideId Slide) :<|>
    ReqBody '[JSON] Slide :> Post '[JSON] (WithId SlideId Slide) :<|>
    Capture "slide_id" SlideId :> ReqBody '[JSON] Slide :> Put '[JSON] (WithId SlideId Slide) :<|>
    Capture "slide_id" SlideId :> Delete '[JSON] ()

instance ToSchema (WithId  SlideId Slide) where
  declareNamedSchema _ = pure $ NamedSchema (Just "SlideWithId") mempty

instance ToSchema Slide where
  declareNamedSchema _ = pure $ NamedSchema (Just "Slide") mempty

instance ToParamSchema (WithId SlideId Slide) where
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

instance Aeson.FromJSON Slide where
  parseJSON = Aeson.withObject "slide" $ \obj ->
    Slide <$>
      obj .: "slide_content" <*>
      obj .: "slide_template" <*>
      obj .:? "slide_attributes" .!= HMS.empty

instance Aeson.ToJSON Slide where
  toJSON slide = Aeson.object
    [ "slide_template" .= slideTemplate slide
    , "slide_attributes" .= slideAttributes slide
    , "slide_content" .= slideContent slide
    ]

instance Aeson.FromJSON (WithId SlideId Slide) where
  parseJSON = Aeson.withObject "WithId SlideId Slide" $ \o ->
    WithId <$>
      (SlideId <$> o .: "slide_id") <*>
      (Slide <$>
        o .: "slide_content" <*>
        o .: "slide_template" <*>
        o .: "slide_attributes"
      )

instance Aeson.ToJSON (WithId SlideId Slide) where
  toJSON (WithId slideId slide) = Aeson.object
    [ "slide_id" .= slideId
    , "slide_template" .= slideTemplate slide
    , "slide_attributes" .= slideAttributes slide
    , "slide_content" .= slideContent slide
    ]

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

usersGet :: Aws.Env -> Servant.Handler [WithId UserId User]
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

usersGetUserId :: Aws.Env -> UserId -> Servant.Handler (WithId UserId User)
usersGetUserId env userId = do
    res <- runAWS env $ Aws.send $ DynamoDB.getItem "Users" &
        DynamoDB.giKey .~ HMS.singleton "UserId" (userIdToAttributeValue userId)
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

usersPost :: Aws.Env -> Firebase.UserId -> User -> Servant.Handler (WithId UserId User)
usersPost env _uid user = do

    userId <- liftIO $ UserId <$> newId

    res <- runAWS env $ Aws.send $ DynamoDB.putItem "Users" &
        DynamoDB.piItem .~ userToItem userId user

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ WithId userId user

usersPut :: Aws.Env -> Firebase.UserId -> UserId -> User -> Servant.Handler (WithId UserId User)
usersPut env _ userId user = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Users" &
        DynamoDB.uiUpdateExpression .~
          Just "SET UserDecks = :s, UserUsername = :n, UserFirebaseId = :i" &
        DynamoDB.uiExpressionAttributeValues .~ userToItem' user &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew &
        DynamoDB.uiKey .~ HMS.singleton "UserId"
          (userIdToAttributeValue userId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ WithId userId user

usersDelete :: Aws.Env -> Firebase.UserId -> UserId -> Servant.Handler ()
usersDelete env _ userId = do

    res <- runAWS env $ Aws.send $ DynamoDB.deleteItem "Users" &
        DynamoDB.diKey .~ HMS.singleton "UserId"
          (userIdToAttributeValue userId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

-- DECKS

decksGet :: Aws.Env -> Firebase.UserId -> Servant.Handler [WithId DeckId Deck]
decksGet env _uid = do
    res <- runAWS env $ Aws.send $ DynamoDB.scan "Decks"
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

decksGetDeckId :: Aws.Env -> Firebase.UserId -> DeckId -> Servant.Handler (WithId DeckId Deck)
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

decksPost :: Aws.Env -> Deck -> Servant.Handler (WithId DeckId Deck)
decksPost env deck = do

    deckId <- liftIO $ DeckId <$> newId

    res <- runAWS env $ Aws.send $ DynamoDB.putItem "Decks" &
        DynamoDB.piItem .~ deckToItem deckId deck

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ WithId deckId deck

decksPut :: Aws.Env -> Firebase.UserId -> DeckId -> Deck -> Servant.Handler (WithId DeckId Deck)
decksPut env _ deckId deck = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Decks" &
        DynamoDB.uiUpdateExpression .~ Just "SET DeckSlides = :s, DeckName = :n" &
        DynamoDB.uiExpressionAttributeValues .~ deckToItem' deck &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew &
        DynamoDB.uiKey .~ HMS.singleton "DeckId"
          (deckIdToAttributeValue deckId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

    pure $ WithId deckId deck

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

slidesGet :: Aws.Env -> Servant.Handler [WithId SlideId Slide]
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

slidesGetSlideId :: Aws.Env -> SlideId -> Servant.Handler (WithId SlideId Slide)
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

slidesPost :: Aws.Env -> Slide -> Servant.Handler (WithId SlideId Slide)
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

    pure $ WithId slideId slide

slidesPut :: Aws.Env -> SlideId -> Slide -> Servant.Handler (WithId SlideId Slide)
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

    pure $ WithId slideId slide

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
userToItem userId User{userDecks, userUsername, userFirebaseId} =
    HMS.singleton "UserId" (userIdToAttributeValue userId) <>
    HMS.singleton "UserDecks" (userDecksToAttributeValue userDecks) <>
    HMS.singleton "UserFirebaseId" (userFirebaseIdToAttributeValue userFirebaseId) <>
    HMS.singleton "UserUsername" (userNameToAttributeValue userUsername)

userToItem' :: User -> HMS.HashMap T.Text DynamoDB.AttributeValue
userToItem' User{userDecks, userUsername, userFirebaseId} =
    HMS.singleton ":s" (userDecksToAttributeValue userDecks) <>
    HMS.singleton ":i" (userFirebaseIdToAttributeValue userFirebaseId) <>
    HMS.singleton ":n" (userNameToAttributeValue userUsername)

itemToUser :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (WithId UserId User)
itemToUser item = do
    userId <- HMS.lookup "UserId" item >>= userIdFromAttributeValue
    userDecks <- HMS.lookup "UserDecks" item >>= userDecksFromAttributeValue
    userUsername <- HMS.lookup "UserUsername" item >>= userNameFromAttributeValue
    userFirebaseId <- HMS.lookup "UserFirebaseId" item >>= userFirebaseIdFromAttributeValue
    pure $ WithId userId User{..}

-- USER ATTRIBUTES

userIdToAttributeValue :: UserId -> DynamoDB.AttributeValue
userIdToAttributeValue (UserId userId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just userId

userIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe UserId
userIdFromAttributeValue attr = UserId <$> attr ^. DynamoDB.avS

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

userDecksToAttributeValue :: [DeckId] -> DynamoDB.AttributeValue
userDecksToAttributeValue userDecks =
    DynamoDB.attributeValue & DynamoDB.avL .~
      (deckIdToAttributeValue <$> userDecks)

userDecksFromAttributeValue :: DynamoDB.AttributeValue -> Maybe [DeckId]
userDecksFromAttributeValue attr =
    traverse deckIdFromAttributeValue (attr ^. DynamoDB.avL)

-- DECKS

deckToItem :: DeckId -> Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem deckId Deck{deckSlides, deckDeckname} =
    HMS.singleton "DeckId" (deckIdToAttributeValue deckId) <>
    HMS.singleton "DeckSlides" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton "DeckName" (deckNameToAttributeValue deckDeckname)

deckToItem' :: Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem' Deck{deckSlides, deckDeckname} =
    HMS.singleton ":s" (deckSlidesToAttributeValue deckSlides) <>
    HMS.singleton ":n" (deckNameToAttributeValue deckDeckname)

itemToDeck :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (WithId DeckId Deck)
itemToDeck item = do
    deckId <- HMS.lookup "DeckId" item >>= deckIdFromAttributeValue
    deckSlides <- HMS.lookup "DeckSlides" item >>= deckSlidesFromAttributeValue
    deckDeckname <- HMS.lookup "DeckName" item >>= deckNameFromAttributeValue
    pure $ WithId deckId Deck{..}

-- DECK ATTRIBUTES

deckIdToAttributeValue :: DeckId -> DynamoDB.AttributeValue
deckIdToAttributeValue (DeckId deckId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just deckId

deckNameToAttributeValue :: Deckname -> DynamoDB.AttributeValue
deckNameToAttributeValue (Deckname deckname) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just deckname

deckNameFromAttributeValue :: DynamoDB.AttributeValue -> Maybe Deckname
deckNameFromAttributeValue attr = Deckname <$> attr ^. DynamoDB.avS

deckIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe DeckId
deckIdFromAttributeValue attr = DeckId <$> attr ^. DynamoDB.avS

deckSlidesToAttributeValue :: [SlideId] -> DynamoDB.AttributeValue
deckSlidesToAttributeValue deckSlides =
    DynamoDB.attributeValue & DynamoDB.avL .~
      (slideIdToAttributeValue <$> deckSlides)

deckSlidesFromAttributeValue :: DynamoDB.AttributeValue -> Maybe [SlideId]
deckSlidesFromAttributeValue attr =
    traverse slideIdFromAttributeValue (attr ^. DynamoDB.avL)

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

itemToSlide :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (WithId SlideId Slide)
itemToSlide item = do
    slideId <- HMS.lookup "SlideId" item >>= slideIdFromAttributeValue

    slideContent <- HMS.lookup "SlideContent" item >>= slideContentFromAttributeValue

    slideTemplate <- HMS.lookup "SlideTemplate" item >>= slideTemplateFromAttributeValue
    slideAttributes <- HMS.lookup "SlideAttributes" item >>= slideAttributesFromAttributeValue

    pure $ WithId slideId Slide{..}

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
