{-# LANGUAGE DataKinds #-}
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

import Control.Monad
import Control.Monad.Except
import Control.Lens hiding ((.=))
import Data.Proxy
import qualified Data.X509 as X509
import qualified Data.PEM as PEM
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString as BS
import Servant.API
import qualified Crypto.JOSE.JWA.JWS as JOSE
import qualified Crypto.JOSE as JOSE
import qualified Crypto.JWT as JWT
import qualified Crypto.JOSE.JWK as JWK
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HMS
import UnliftIO
import Data.Aeson ((.=), (.:), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import qualified Network.AWS as Aws
import qualified Network.AWS.DynamoDB as DynamoDB
import qualified Network.Wai as Wai
import qualified Servant as Servant
import qualified Servant.Client.Core as Servant
-- import qualified Servant.Auth as Servant
import qualified System.Random as Random

-- | Generate a key suitable for use with 'defaultConfig'.

_sign :: JOSE.JWK -> JWT.ClaimsSet -> IO (Either JWT.JWTError JWT.SignedJWT)
_sign jwk cs = runExceptT $ JWT.signClaims jwk (JOSE.newJWSHeader ((), JOSE.RS256)) cs

newtype UserId = UserId { unUserId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Show, Eq)

newtype UnverifiedJWT = UnverifiedJWT JWT.SignedJWT

-- TODO: MAKE SURE PATTERN MATCH FAILURES AREN'T PROPAGATED TO CLIENT!!!
verifyUser :: UnverifiedJWT -> IO UserId
verifyUser (UnverifiedJWT jwt) = do
  Just jwkmap <- Aeson.decodeFileStrict' "./cert" :: IO (Maybe (HMS.HashMap T.Text T.Text))
  Just jwkct <- pure $ HMS.lookup "1" jwkmap
  pem <- case PEM.pemParseBS (T.encodeUtf8 jwkct) of
    Left e -> error $ show e
    Right [e] -> pure e
    Right xs -> error $ show xs
  cert <- case X509.decodeSignedCertificate (PEM.pemContent pem) of
    Left e -> error $ show e
    Right c -> pure c
  Right jwk <- runExceptT (JWK.fromX509Certificate cert) :: IO (Either JWT.JWTError JWT.JWK)
  let config = JWT.defaultJWTValidationSettings (== "my-project-id")
  runExceptT (JWT.verifyClaims config jwk jwt) >>= \case
    Right {} -> pure (UserId "")
    Left (e :: JWT.JWTError) -> error (show e)

instance FromHttpApiData UnverifiedJWT where
  parseUrlPiece = const $ Left "No support for JWT"
  parseHeader bs = case JWT.decodeCompact (BL.fromStrict bs) of
    Left (e :: JWT.Error) -> Left $ T.pack $ show e
    Right jwt -> Right $ UnverifiedJWT jwt

instance Servant.RunClient m => Servant.HasClient m (Protected :> Get '[JSON] [WithId DeckId Deck]) where
  type Client m (Protected :> Get '[JSON] [WithId DeckId Deck]) = T.Text -> Servant.Client m (Get '[JSON] [WithId DeckId Deck])
  clientWithRoute p Proxy req = \bs ->
    -- TODO: header should be Authorization Bearer
    Servant.clientWithRoute p (Proxy :: Proxy (Header "Authorization" T.Text :> Get '[JSON] [WithId DeckId Deck])) req (Just bs)

  hoistClientMonad Proxy Proxy hoist c = \bs -> hoist (c bs)

instance Servant.HasServer  (Protected :> Get '[JSON] [WithId DeckId Deck]) context where
  type ServerT (Protected :> Get '[JSON] [WithId DeckId Deck]) m = UserId -> Servant.ServerT (Get '[JSON] [WithId DeckId Deck]) m

  route Proxy c sub = do
      Servant.route (Proxy :: Proxy ( Header "Authorization" UnverifiedJWT :>  Get '[JSON] [WithId DeckId Deck])) c (adapt <$> sub)
    where
      adapt f = \case
        Nothing -> error "NO SUCH FOOOO"
        Just jwt -> do
          uid <- liftIO $ verifyUser jwt
          f uid


  hoistServerWithContext = Servant.hoistServerWithContext


------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

data Protected
-- type Protected = Header "Bearer" JWT.SignedJWT

-- protect :: m b -> Maybe JWT.SignedJWT -> m b
-- protect f _ = f

data WithId id a = WithId id a
  deriving (Show, Eq)

newtype DeckId = DeckId { unDeckId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, FromHttpApiData, ToHttpApiData, Show, Eq)

data Deck = Deck
  { deckSlides :: [SlideId]
  } deriving (Show, Eq)

newtype SlideId = SlideId { unSlideId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, FromHttpApiData, ToHttpApiData, Show, Eq)

data Slide = Slide
  { slideContent :: T.Text
  , slideTemplate :: T.Text
  , slideAttributes :: HMS.HashMap T.Text T.Text
  } deriving (Show, Eq)

instance Aeson.FromJSON Deck where
  parseJSON = Aeson.withObject "decK" $ \obj ->
    Deck <$> obj .: "deck_slides"

instance Aeson.ToJSON Deck where
  toJSON deck = Aeson.object
    [ "deck_slides" .= deckSlides deck
    ]

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

instance Aeson.FromJSON (WithId DeckId Deck) where
  parseJSON = Aeson.withObject "WithId DeckId Deck" $ \o ->
    WithId <$>
      (DeckId <$> o .: "deck_id") <*>
      (Deck <$> o .: "deck_slides")

instance Aeson.ToJSON (WithId DeckId Deck) where
  toJSON (WithId deckId deck) = Aeson.object
    [ "deck_id" .= deckId
    , "deck_slides" .= deckSlides deck
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
    "decks" :> DecksAPI :<|>
    "slides" :> SlidesAPI

type DecksAPI =
    Protected :> Get '[JSON] [WithId DeckId Deck] :<|>
    Capture "deck_id" DeckId :> Get '[JSON] (WithId DeckId Deck) :<|>
    ReqBody '[JSON] Deck :> Post '[JSON] (WithId DeckId Deck) :<|>
    Capture "deck_id" DeckId :> ReqBody '[JSON] Deck :> Put '[JSON] (WithId DeckId Deck) :<|>
    Capture "deck_id" DeckId :> Delete '[JSON] ()

type SlidesAPI =
    Get '[JSON] [WithId SlideId Slide] :<|>
    Capture "slide_id" SlideId :> Get '[JSON] (WithId SlideId Slide) :<|>
    ReqBody '[JSON] Slide :> Post '[JSON] (WithId SlideId Slide) :<|>
    Capture "slide_id" SlideId :> ReqBody '[JSON] Slide :> Put '[JSON] (WithId SlideId Slide) :<|>
    Capture "slide_id" SlideId :> Delete '[JSON] ()

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

application :: Aws.Env -> Wai.Application
application env = Servant.serve api (server env)

server :: Aws.Env -> Servant.Server API
server env = serveDecks :<|> serveSlides
  where
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

decksGet :: Aws.Env -> UserId -> Servant.Handler [WithId DeckId Deck]
decksGet env uid = do
    liftIO $ print uid
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

decksGetDeckId :: Aws.Env -> DeckId -> Servant.Handler (WithId DeckId Deck)
decksGetDeckId env deckId = do
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

decksPut :: Aws.Env -> DeckId -> Deck -> Servant.Handler (WithId DeckId Deck)
decksPut env deckId deck = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Decks" &
        DynamoDB.uiUpdateExpression .~ Just "SET DeckSlides = :s" &
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

decksDelete :: Aws.Env -> DeckId -> Servant.Handler ()
decksDelete env deckId = do

    res <- runAWS env $ Aws.send $ DynamoDB.deleteItem "Decks" &
        DynamoDB.diKey .~ HMS.singleton "DeckId"
          (deckIdToAttributeValue deckId)

    case res of
      Right {} -> pure ()
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

runAWS :: MonadIO m => Aws.Env -> Aws.AWS a -> m (Either SomeException a)
runAWS env =
    liftIO .
    tryAny .
    Aws.runResourceT .
    Aws.runAWS env .
    Aws.within Aws.NorthVirginia

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
        DynamoDB.uiKey .~  HMS.singleton "SlideId"
          (slideIdToAttributeValue slideId)

    case res of
      Right x -> liftIO $ print x
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
      Right x -> liftIO $ print x
      Left e -> do
        liftIO $ print e
        Servant.throwError Servant.err500

randomString :: Int -> [Char] -> IO String
randomString len allowedChars =
  replicateM len $ do
    idx <- Random.randomRIO (0, length allowedChars - 1)
    pure $ allowedChars !! idx

randomText :: Int -> [Char] -> IO T.Text
randomText len allowedChars = T.pack <$> randomString len allowedChars

newId :: IO T.Text
newId = randomText 32 (['0' .. '9'] <> ['a' .. 'z'])

deckIdToAttributeValue :: DeckId -> DynamoDB.AttributeValue
deckIdToAttributeValue (DeckId deckId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just deckId

deckIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe DeckId
deckIdFromAttributeValue attr = DeckId <$> attr ^. DynamoDB.avS

deckSlidesToAttributeValue :: [SlideId] -> DynamoDB.AttributeValue
deckSlidesToAttributeValue deckSlides =
    DynamoDB.attributeValue & DynamoDB.avL .~
      (slideIdToAttributeValue <$> deckSlides)

deckSlidesFromAttributeValue :: DynamoDB.AttributeValue -> Maybe [SlideId]
deckSlidesFromAttributeValue attr =
    traverse slideIdFromAttributeValue (attr ^. DynamoDB.avL)

slideIdToAttributeValue :: SlideId -> DynamoDB.AttributeValue
slideIdToAttributeValue (SlideId slideId) =
    DynamoDB.attributeValue & DynamoDB.avS .~ Just slideId

slideIdFromAttributeValue :: DynamoDB.AttributeValue -> Maybe SlideId
slideIdFromAttributeValue attr = SlideId <$> attr ^. DynamoDB.avS

deckToItem :: DeckId -> Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem deckId Deck{deckSlides} =
    HMS.singleton "DeckId" (deckIdToAttributeValue deckId) <>
    HMS.singleton "DeckSlides" (deckSlidesToAttributeValue deckSlides)

deckToItem' :: Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem' Deck{deckSlides} =
    HMS.singleton ":s" (deckSlidesToAttributeValue deckSlides)

itemToDeck :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (WithId DeckId Deck)
itemToDeck item = do
    deckId <- HMS.lookup "DeckId" item >>= deckIdFromAttributeValue
    deckSlides <- HMS.lookup "DeckSlides" item >>= deckSlidesFromAttributeValue
    pure $ WithId deckId Deck{..}

slideToItem :: SlideId -> Slide -> HMS.HashMap T.Text DynamoDB.AttributeValue
slideToItem slideId Slide{slideContent, slideTemplate, slideAttributes} =
    HMS.singleton "SlideId" (slideIdToAttributeValue slideId) <>
    HMS.singleton "SlideContent" (slideContentToAttributeValue slideContent) <>
    HMS.singleton "SlideTemplate" (slideTemplateToAttributeValue slideTemplate) <>
    HMS.singleton "SlideAttributes" (slideAttributesToAttributeValue slideAttributes)

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
