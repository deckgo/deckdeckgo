{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import DeckGo.Handler
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HMS
import System.Environment (getArgs)

main :: IO ()
main = do
  [p] <- getArgs

  b <- T.readFile p

  manager' <- newManager defaultManagerSettings

  let clientEnv = mkClientEnv manager' (BaseUrl Http "localhost" 8080 "")

  runClientM (decksGet' b) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right [] -> pure ()
    Right decks -> error $ "Expected 0 decks, got: " <> show decks

  let someDeck = Deck { deckSlides = [] }

  deckId <- runClientM (decksPost' someDeck) clientEnv >>= \case
    Left err -> error $ "Expected new deck, got error: " <> show err
    Right (WithId deckId _) -> pure deckId

  let someSlide = Slide "foo" "bar" HMS.empty

  slideId <- runClientM (slidesPost' someSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right (WithId slideId _) -> pure slideId

  let newDeck = Deck { deckSlides = [ slideId ] }

  runClientM (decksPut' deckId newDeck) clientEnv >>= \case
    Left err -> error $ "Expected updated deck, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksGet' b) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right decks ->
      if decks == [WithId deckId newDeck] then pure () else (error $ "Expected updated decks, got: " <> show decks)

  runClientM (decksGetDeckId' deckId) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right deck ->
      if deck == (WithId deckId newDeck) then pure () else (error $ "Expected get deck, got: " <> show deck)

  runClientM slidesGet' clientEnv >>= \case
    Left err -> error $ "Expected slides, got error: " <> show err
    Right slides ->
      if slides == [WithId slideId someSlide] then pure () else (error $ "Expected slides, got: " <> show slides)

  let updatedSlide = Slide "foo" "quux" HMS.empty

  runClientM (slidesPut' slideId updatedSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right {} -> pure ()

  runClientM slidesGet' clientEnv >>= \case
    Left err -> error $ "Expected updated slides, got error: " <> show err
    Right slides ->
      if slides == [WithId slideId updatedSlide] then pure () else (error $ "Expected updated slides, got: " <> show slides)

  runClientM (slidesGetSlideId' slideId) clientEnv >>= \case
    Left err -> error $ "Expected updated slide, got error: " <> show err
    Right slide ->
      if slide == (WithId slideId updatedSlide) then pure () else (error $ "Expected updated slide, got: " <> show slide)

  runClientM (slidesDelete' slideId) clientEnv >>= \case
    Left err -> error $ "Expected slide delete, got error: " <> show err
    Right {} -> pure ()

  runClientM slidesGet' clientEnv >>= \case
    Left err -> error $ "Expected no slides, got error: " <> show err
    Right slides ->
      if slides == [] then pure () else (error $ "Expected no slides, got: " <> show slides)

  runClientM (decksDelete' deckId) clientEnv >>= \case
    Left err -> error $ "Expected deck delete, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksGet' b) clientEnv >>= \case
    Left err -> error $ "Expected no decks, got error: " <> show err
    Right decks ->
      if decks == [] then pure () else (error $ "Expected no decks, got: " <> show decks)

-- 'client' allows you to produce operations to query an API from a client.
decksGet' :: T.Text -> ClientM [WithId DeckId Deck]
decksGetDeckId' :: DeckId -> ClientM (WithId DeckId Deck)
decksPost' :: Deck -> ClientM (WithId DeckId Deck)
decksPut' :: DeckId -> Deck -> ClientM (WithId DeckId Deck)
decksDelete' :: DeckId -> ClientM ()
slidesGet' :: ClientM [WithId SlideId Slide]
slidesGetSlideId' :: SlideId -> ClientM (WithId SlideId Slide)
slidesPost' :: Slide -> ClientM (WithId SlideId Slide)
slidesPut' :: SlideId -> Slide -> ClientM (WithId SlideId Slide)
slidesDelete' :: SlideId -> ClientM ()
((
  decksGet' :<|>
  decksGetDeckId' :<|>
  decksPost' :<|>
  decksPut' :<|>
  decksDelete'
  ) :<|>
  (
  slidesGet' :<|>
  slidesGetSlideId' :<|>
  slidesPost' :<|>
  slidesPut' :<|>
  slidesDelete'
  )
  ) = client api
