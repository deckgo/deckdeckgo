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

  runClientM usersGet' clientEnv >>= \case
    Left err -> error $ "Expected users, got error: " <> show err
    Right [] -> pure ()
    Right decks -> error $ "Expected 0 users, got: " <> show decks

  runClientM (decksGet' b Nothing) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right [] -> pure ()
    Right decks -> error $ "Expected 0 decks, got: " <> show decks

  let someUserId = UserId "foo"

  let someDeck = Deck { deckSlides = [] , deckDeckname = Deckname "foo", deckOwnerId = someUserId }

  deckId <- runClientM (decksPost' b someDeck) clientEnv >>= \case
    Left err -> error $ "Expected new deck, got error: " <> show err
    Right (Item deckId _) -> pure deckId

  let someSlide = Slide "foo" "bar" HMS.empty

  slideId <- runClientM (slidesPost' someSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right (Item slideId _) -> pure slideId

  let newDeck = Deck { deckSlides = [ slideId ], deckDeckname = Deckname "bar", deckOwnerId = someUserId }

  runClientM (decksPut' b deckId newDeck) clientEnv >>= \case
    Left err -> error $ "Expected updated deck, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksGet' b Nothing) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right decks ->
      if decks == [Item deckId newDeck] then pure () else (error $ "Expected updated decks, got: " <> show decks)

  runClientM (decksGetDeckId' b deckId) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right deck ->
      if deck == (Item deckId newDeck) then pure () else (error $ "Expected get deck, got: " <> show deck)

  runClientM slidesGet' clientEnv >>= \case
    Left err -> error $ "Expected slides, got error: " <> show err
    Right slides ->
      if slides == [Item slideId someSlide] then pure () else (error $ "Expected slides, got: " <> show slides)

  let updatedSlide = Slide "foo" "quux" HMS.empty

  runClientM (slidesPut' slideId updatedSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right {} -> pure ()

  runClientM slidesGet' clientEnv >>= \case
    Left err -> error $ "Expected updated slides, got error: " <> show err
    Right slides ->
      if slides == [Item slideId updatedSlide] then pure () else (error $ "Expected updated slides, got: " <> show slides)

  runClientM (slidesGetSlideId' slideId) clientEnv >>= \case
    Left err -> error $ "Expected updated slide, got error: " <> show err
    Right slide ->
      if slide == (Item slideId updatedSlide) then pure () else (error $ "Expected updated slide, got: " <> show slide)

  runClientM (slidesDelete' slideId) clientEnv >>= \case
    Left err -> error $ "Expected slide delete, got error: " <> show err
    Right {} -> pure ()

  runClientM slidesGet' clientEnv >>= \case
    Left err -> error $ "Expected no slides, got error: " <> show err
    Right slides ->
      if slides == [] then pure () else (error $ "Expected no slides, got: " <> show slides)

  runClientM (decksDelete' b deckId) clientEnv >>= \case
    Left err -> error $ "Expected deck delete, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksGet' b Nothing) clientEnv >>= \case
    Left err -> error $ "Expected no decks, got error: " <> show err
    Right decks ->
      if decks == [] then pure () else (error $ "Expected no decks, got: " <> show decks)


usersGet' :: ClientM [Item UserId User]
_usersGetUserId' :: UserId -> ClientM (Item UserId User)
_usersPost' :: T.Text -> User -> ClientM (Item UserId User)
_usersPut' :: T.Text -> UserId -> User -> ClientM (Item UserId User)
_usersDelete' :: T.Text -> UserId -> ClientM ()

decksGet' :: T.Text -> Maybe UserId -> ClientM [Item DeckId Deck]
decksGetDeckId' :: T.Text -> DeckId -> ClientM (Item DeckId Deck)
decksPost' :: T.Text -> Deck -> ClientM (Item DeckId Deck)
decksPut' :: T.Text -> DeckId -> Deck -> ClientM (Item DeckId Deck)
decksDelete' :: T.Text -> DeckId -> ClientM ()

slidesGet' :: ClientM [Item SlideId Slide]
slidesGetSlideId' :: SlideId -> ClientM (Item SlideId Slide)
slidesPost' :: Slide -> ClientM (Item SlideId Slide)
slidesPut' :: SlideId -> Slide -> ClientM (Item SlideId Slide)
slidesDelete' :: SlideId -> ClientM ()
((
  usersGet' :<|>
  _usersGetUserId' :<|>
  _usersPost' :<|>
  _usersPut' :<|>
  _usersDelete'
  ) :<|>
  (
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
