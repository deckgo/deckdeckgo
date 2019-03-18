{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad
import Control.Lens hiding ((.=))
import Data.Proxy
import Servant.API
import Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import UnliftIO
import Data.Aeson ((.=), (.:), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import qualified Network.AWS as Aws
import qualified Network.AWS.DynamoDB as DynamoDB
import qualified Network.Wai.Handler.Lambda as Lambda
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Servant as Servant
import qualified System.Random as Random

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

data WithId id a = WithId id a

newtype DeckId = DeckId { unDeckId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, FromHttpApiData)

data Deck = Deck
  { deckSlides :: [SlideId]
  }

newtype SlideId = SlideId { unSlideId :: T.Text }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, FromHttpApiData)

data Slide = Slide
  { slideContent :: T.Text
  , slideTemplate :: T.Text
  , slideAttributes :: HMS.HashMap T.Text T.Text
  }

instance Aeson.FromJSON Deck where
  parseJSON = Aeson.withObject "decK" $ \obj ->
    Deck <$> obj .: "deck_slides"

instance Aeson.FromJSON Slide where
  parseJSON = Aeson.withObject "slide" $ \obj ->
    Slide <$>
      obj .: "slide_content" <*>
      obj .: "slide_template" <*>
      obj .:? "slide_attributes" .!= HMS.empty

instance Aeson.ToJSON (WithId DeckId Deck) where
  toJSON (WithId deckId deck) = Aeson.object
    [ "deck_id" .= deckId
    , "deck_slides" .= deckSlides deck
    ]

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
    Get '[JSON] [WithId DeckId Deck] :<|>
    ReqBody '[JSON] Deck :> Post '[JSON] (WithId DeckId Deck) :<|>
    Capture "deck_id" DeckId :> ReqBody '[JSON] Deck :> Put '[JSON] (WithId DeckId Deck)

type SlidesAPI =
    Get '[JSON] [WithId SlideId Slide] :<|>
    ReqBody '[JSON] Slide :> Post '[JSON] (WithId SlideId Slide) :<|>
    Capture "slide_id" SlideId :> ReqBody '[JSON] Slide :> Put '[JSON] (WithId SlideId Slide)

api :: Proxy API
api = Proxy

------------------------------------------------------------------------------
-- SERVER
------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  liftIO $ putStrLn "Booting..."
  env <- Aws.newEnv Aws.Discover

  liftIO $ putStrLn "Booted!"
  Lambda.run $ Cors.simpleCors $ Servant.serve api (server env)

server :: Aws.Env -> Servant.Server API
server env = serveDecks :<|> serveSlides
  where
    serveDecks = decksGet env :<|> decksPost env :<|> decksPut env
    serveSlides = slidesGet env :<|> slidesPost env :<|> slidesPut env

decksGet :: Aws.Env -> Servant.Handler [WithId DeckId Deck]
decksGet env = do
    res <- runAWS env $ Aws.send $ DynamoDB.scan "Decks"
    case res of
      Right scanResponse -> pure $ catMaybes $
        scanResponse ^. DynamoDB.srsItems <&> itemToDeck
      Left e -> do
        liftIO $ print e
        pure []

decksPost :: Aws.Env -> Deck -> Servant.Handler (WithId DeckId Deck)
decksPost env deck = do

    deckId <- liftIO $ DeckId <$> newId

    res <- runAWS env $ Aws.send $ DynamoDB.putItem "Decks" &
        DynamoDB.piItem .~ deckToItem deckId deck

    case res of
      Right x -> liftIO $ print x
      Left e -> liftIO $ print e

    pure $ WithId deckId deck

decksPut :: Aws.Env -> DeckId -> Deck -> Servant.Handler (WithId DeckId Deck)
decksPut env deckId deck = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Decks" &
        DynamoDB.uiUpdateExpression .~ Just "DeckSlides = :DeckSlides" &
        DynamoDB.uiExpressionAttributeValues .~ deckToItem deckId deck &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew

    case res of
      Right x -> liftIO $ print x
      Left e -> liftIO $ print e

    pure $ WithId deckId deck

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
      Right scanResponse -> pure $ catMaybes $
        scanResponse ^. DynamoDB.srsItems <&> itemToSlide
      Left e -> do
        liftIO $ print e
        pure []

slidesPost :: Aws.Env -> Slide -> Servant.Handler (WithId SlideId Slide)
slidesPost env slide = do
    slideId <- liftIO $ SlideId <$> newId

    res <- runAWS env $
      Aws.send $ DynamoDB.putItem "Slides" &
        DynamoDB.piItem .~ slideToItem slideId slide

    case res of
      Right x -> liftIO $ print x
      Left e -> liftIO $ print e

    pure $ WithId slideId slide

slidesPut :: Aws.Env -> SlideId -> Slide -> Servant.Handler (WithId SlideId Slide)
slidesPut env slideId slide = do

    res <- runAWS env $ Aws.send $ DynamoDB.updateItem "Slides" &
        DynamoDB.uiUpdateExpression .~ Just
          "SlideContent = :SlideContent, SlideTemplate = :SlideTemplate, SlideAttributes = :SlideAttributes" &
        DynamoDB.uiExpressionAttributeValues .~ slideToItem slideId slide &
        DynamoDB.uiReturnValues .~ Just DynamoDB.UpdatedNew

    case res of
      Right x -> liftIO $ print x
      Left e -> liftIO $ print e

    pure $ WithId slideId slide

randomString :: Int -> [Char] -> IO String
randomString len allowedChars =
  replicateM len $ do
    idx <- Random.randomRIO (0, length allowedChars - 1)
    pure $ allowedChars !! idx

randomText :: Int -> [Char] -> IO T.Text
randomText len allowedChars = T.pack <$> randomString len allowedChars

newId :: IO T.Text
newId = randomText 32 (['0' .. '9'] <> ['a' .. 'z'])

deckToItem :: DeckId -> Deck -> HMS.HashMap T.Text DynamoDB.AttributeValue
deckToItem deckId Deck{deckSlides} =
    HMS.singleton "DeckId"
      (DynamoDB.attributeValue & DynamoDB.avS .~ Just (unDeckId deckId)) <>
    (if null deckSlides
    then HMS.empty
    else
      HMS.singleton "DeckSlides"
        (DynamoDB.attributeValue & DynamoDB.avSS .~ (unSlideId <$> deckSlides))
    )

itemToDeck :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (WithId DeckId Deck)
itemToDeck item = do
    deckIdAttr <- HMS.lookup "DeckId" item
    deckIdString <- deckIdAttr ^. DynamoDB.avS
    deckId <- pure $ DeckId deckIdString
    deckSlides <- pure $ case HMS.lookup "DeckSlides" item of
      Nothing -> []
      Just slides -> slides ^. DynamoDB.avSS <&> SlideId
    pure $ WithId deckId Deck{..}


slideToItem :: SlideId -> Slide -> HMS.HashMap T.Text DynamoDB.AttributeValue
slideToItem slideId Slide{slideContent, slideTemplate, slideAttributes} =
    HMS.singleton "SlideId"
      (DynamoDB.attributeValue & DynamoDB.avS .~ Just (unSlideId slideId)) <>
    HMS.singleton "SlideContent"
      (DynamoDB.attributeValue & DynamoDB.avS .~ Just slideContent) <>
    HMS.singleton "SlideTemplate"
      (DynamoDB.attributeValue & DynamoDB.avS .~ Just slideTemplate) <>
    (if HMS.null slideAttributes
    then HMS.empty
    else
      HMS.singleton "SlideAttributes"
        (DynamoDB.attributeValue & DynamoDB.avM .~ (
          (\txt -> DynamoDB.attributeValue & DynamoDB.avS .~ Just txt) <$>
            slideAttributes
        ))
    )

itemToSlide :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe (WithId SlideId Slide)
itemToSlide item = do
    slideIdAttr <- HMS.lookup "SlideId" item
    slideIdString <- slideIdAttr ^. DynamoDB.avS
    slideId <- pure $ SlideId slideIdString

    slideContentAttr <- HMS.lookup "SlideContent" item
    slideContent <- slideContentAttr ^. DynamoDB.avS

    slideTemplateAttr <- HMS.lookup "SlideTemplate" item
    slideTemplate <- slideTemplateAttr ^. DynamoDB.avS

    slideAttributesAttr <- HMS.lookup "SlideAttributes" item
    slideAttributes <- pure $ slideAttributesAttr ^. DynamoDB.avM &
      HMS.mapMaybe (\attr -> attr ^. DynamoDB.avS)

    pure $ WithId slideId Slide{..}
