{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Control.Monad
import DeckGo.Handler
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types as HTTP
import Servant.API
import Servant.Client
import System.Environment
import System.Environment (getEnv)
import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Network.AWS as Aws
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClient
import qualified Network.Socket.Wait as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Auth.Firebase as Firebase
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

withServer :: (Warp.Port -> IO a) -> IO a
withServer act = do
    mgr <- HTTPClient.newManager HTTPClient.tlsManagerSettings
            { HTTPClient.managerModifyRequest =
                pure . rerouteDynamoDB
            }
    withPristineDB $ \conn -> do
      env <- Aws.newEnv Aws.Discover <&> Aws.envManager .~ mgr

      (port, socket) <- Warp.openFreePort
      let warpSettings = Warp.setPort port $ Warp.defaultSettings
      settings <- getFirebaseSettings
      race
        (Warp.runSettingsSocket warpSettings socket $ DeckGo.Handler.application settings env conn)
        (do
          Socket.wait "localhost" port
          act port
        ) >>= \case
          Left () -> error "Server returned"
          Right a -> pure a

withPristineDB :: (HC.Connection -> IO a) -> IO a
withPristineDB act = do
    conn <- getPostgresqlConnection
    putStrLn "DROP TABLE IF EXISTS username"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS username") conn
    putStrLn "DROP TABLE IF EXISTS account CASCADE"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS account CASCADE") conn
    putStrLn "DROP TABLE IF EXISTS db_meta"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS db_meta") conn
    act conn

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "1"
    Tasty.defaultMain $ Tasty.testGroup "tests"
      [ Tasty.testGroup "db"
          [ Tasty.testCase "users get" testUsersGet
          , Tasty.testCase "users create" testUsersCreate
          , Tasty.testCase "users get by id" testUsersGetByUserId
          , Tasty.testCase "users delete" testUsersDelete
          , Tasty.testCase "users update" testUsersUpdate
          ]
      , Tasty.testCase "foo" main'
      ]

testUsersGet :: IO ()
testUsersGet = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    dbGetAllUsers iface >>= \case
      [] -> pure ()
      users -> error $ "Expected no users, got: " <> show users

    let someFirebaseId = FirebaseId "foo"
        someUserId = UserId someFirebaseId
        someUser = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "patrick")
          }
    dbCreateUser iface someUserId someUser >>= \case
      Left () -> error "Encountered error"
      Right () -> pure ()

    dbGetAllUsers iface >>= \case
      [Item userId user] ->
        if userId == someUserId && user == someUser
        then pure ()
        else error "bad user"
      users -> error $ "Expected no users, got: " <> show users

testUsersGetByUserId :: IO ()
testUsersGetByUserId = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    let someFirebaseId = FirebaseId "foo"
        someUserId = UserId someFirebaseId
        someUser = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "patrick")
          }
    dbCreateUser iface someUserId someUser >>= \case
      Left () -> error "Encountered error"
      Right () -> pure ()

    dbGetUserById iface someUserId >>= \case
      Just (Item userId user) ->
        if userId == someUserId && user == someUser
        then pure ()
        else error "bad user"
      Nothing -> error "Got no users"

testUsersDelete :: IO ()
testUsersDelete = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    let someFirebaseId = FirebaseId "foo"
        someUserId = UserId someFirebaseId
        someUser = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "patrick")
          }
    dbCreateUser iface someUserId someUser >>= \case
      Left () -> error "Encountered error"
      Right () -> pure ()

    dbDeleteUser iface someUserId >>= \case
      Left () -> error "couldn't delete"
      Right () -> pure ()

testUsersCreate :: IO ()
testUsersCreate = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    let someFirebaseId = FirebaseId "foo"
        someUserId = UserId someFirebaseId
        someUser = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "patrick")
          }
    dbCreateUser iface someUserId someUser >>= \case
      Left () -> error "Encountered error"
      Right () -> pure ()

testUsersUpdate :: IO ()
testUsersUpdate = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    let someFirebaseId = FirebaseId "foo"
        someUserId = UserId someFirebaseId
        someUser = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "patrick")
          }

    dbCreateUser iface someUserId someUser >>= \case
      Left () -> error "Encountered error"
      Right () -> pure ()

    let someUser' = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "joseph")
          }

    dbUpdateUser iface someUserId someUser' >>= \case
      UserUpdateOk -> pure ()
      e -> error $ "encountered error:" <> show e

    dbGetUserById iface someUserId >>= \case
      Just (Item userId user) ->
        if userId == someUserId && user == someUser'
        then pure ()
        else error "bad user"
      Nothing -> error "Got no users"

getTokenPath :: IO FilePath
getTokenPath =
    lookupEnv "TEST_TOKEN_PATH" >>= \case
      Just tpath -> pure tpath
      Nothing -> pure "./token"

main' :: IO ()
main' = withServer $ \port -> do
  b <- T.readFile =<< getTokenPath

  manager' <- newManager defaultManagerSettings

  let clientEnv = mkClientEnv manager' (BaseUrl Http "localhost" port "")
  let someFirebaseId = FirebaseId "the-uid" -- from ./token
  let someUserId = UserId someFirebaseId
  let someDeck = Deck
        { deckSlides = []
        , deckDeckname = Deckname "foo"
        , deckDeckbackground = Nothing
        , deckOwnerId = someUserId
        , deckAttributes = HMS.empty
        }

  runClientM usersGet' clientEnv >>= \case
    Left err -> error $ "Expected users, got error: " <> show err
    Right [] -> pure ()
    Right users -> error $ "Expected 0 users, got: " <> show users

  runClientM (decksGet' b (Just someUserId)) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right [] -> pure ()
    Right decks -> error $ "Expected 0 decks, got: " <> show decks

  deckId <- runClientM (decksPost' b someDeck) clientEnv >>= \case
    Left err -> error $ "Expected new deck, got error: " <> show err
    Right (Item deckId _) -> pure deckId

  let someSlide = Slide (Just "foo") "bar" HMS.empty

  slideId <- runClientM (slidesPost' b deckId someSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right (Item slideId _) -> pure slideId

  let newDeck = Deck
        { deckSlides = [ slideId ]
        , deckDeckname = Deckname "bar"
        , deckDeckbackground = Just (Deckbackground "bar")
        , deckOwnerId = someUserId
        , deckAttributes = HMS.singleton "foo" "bar"
        }

  runClientM (decksPut' b deckId newDeck) clientEnv >>= \case
    Left err -> error $ "Expected updated deck, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksGet' b (Just someUserId)) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right decks ->
      if decks == [Item deckId newDeck] then pure () else (error $ "Expected updated decks, got: " <> show decks)

  runClientM (decksGetDeckId' b deckId) clientEnv >>= \case
    Left err -> error $ "Expected decks, got error: " <> show err
    Right deck ->
      if deck == (Item deckId newDeck) then pure () else (error $ "Expected get deck, got: " <> show deck)

  let updatedSlide = Slide Nothing "quux" HMS.empty

  runClientM (slidesPut' b deckId slideId updatedSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right {} -> pure ()

  runClientM (slidesPut' b deckId slideId updatedSlide) clientEnv >>= \case
    Left err -> error $ "Expected new slide, got error: " <> show err
    Right {} -> pure ()

  runClientM (slidesGetSlideId' b deckId slideId) clientEnv >>= \case
    Left err -> error $ "Expected updated slide, got error: " <> show err
    Right slide ->
      if slide == (Item slideId updatedSlide) then pure () else (error $ "Expected updated slide, got: " <> show slide)

  runClientM (slidesDelete' b deckId slideId) clientEnv >>= \case
    Left err -> error $ "Expected slide delete, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksDelete' b deckId) clientEnv >>= \case
    Left err -> error $ "Expected deck delete, got error: " <> show err
    Right {} -> pure ()

  runClientM (decksGet' b (Just someUserId)) clientEnv >>= \case
    Left err -> error $ "Expected no decks, got error: " <> show err
    Right decks ->
      if decks == [] then pure () else (error $ "Expected no decks, got: " <> show decks)

  let someUserInfo = UserInfo
        { userInfoFirebaseId = someFirebaseId
        , userInfoEmail = Just "patrick@foo.com" }
      Right someUser = userInfoToUser someUserInfo

  runClientM (usersPost' b someUserInfo) clientEnv >>= \case
    Left err -> error $ "Expected user, got error: " <> show err
    Right (Item userId user) ->
      if user == someUser && userId == someUserId then pure () else (error $ "Expected same user, got: " <> show user)

  runClientM (usersPost' b someUserInfo) clientEnv >>= \case
    -- TODO: test that user is returned here, even on 409
    Left (FailureResponse resp) ->
      if HTTP.statusCode (responseStatusCode resp) == 409 then pure () else
        error $ "Got unexpected response: " <> show resp
    Left err -> error $ "Expected 409, got error: " <> show err
    Right item -> error $ "Expected failure, got success: " <> show item

  -- TODO: test that creating user with token that has different user as sub
  -- fails

usersGet' :: ClientM [Item UserId User]
_usersGetUserId' :: UserId -> ClientM (Item UserId User)
usersPost' :: T.Text -> UserInfo -> ClientM (Item UserId User)
_usersPut' :: T.Text -> UserId -> UserInfo -> ClientM (Item UserId User)
_usersDelete' :: T.Text -> UserId -> ClientM ()

decksGet' :: T.Text -> Maybe UserId -> ClientM [Item DeckId Deck]
decksGetDeckId' :: T.Text -> DeckId -> ClientM (Item DeckId Deck)
decksPost' :: T.Text -> Deck -> ClientM (Item DeckId Deck)
decksPut' :: T.Text -> DeckId -> Deck -> ClientM (Item DeckId Deck)
decksDelete' :: T.Text -> DeckId -> ClientM ()

slidesGetSlideId' :: T.Text -> DeckId -> SlideId -> ClientM (Item SlideId Slide)
slidesPost' :: T.Text -> DeckId -> Slide -> ClientM (Item SlideId Slide)
slidesPut' :: T.Text -> DeckId -> SlideId -> Slide -> ClientM (Item SlideId Slide)
slidesDelete' :: T.Text -> DeckId -> SlideId -> ClientM ()
((
  usersGet' :<|>
  _usersGetUserId' :<|>
  usersPost' :<|>
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
  slidesGetSlideId' :<|>
  slidesPost' :<|>
  slidesPut' :<|>
  slidesDelete'
  )
  ) = client api

rerouteDynamoDB :: HTTPClient.Request -> HTTPClient.Request
rerouteDynamoDB req =
    case HTTPClient.host req of
      "dynamodb.us-east-1.amazonaws.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 8000 -- TODO: read from Env
          , HTTPClient.secure = False
          }
      _ -> req

getFirebaseSettings :: IO Firebase.FirebaseLoginSettings
getFirebaseSettings = do
    pkeys <- getEnv "GOOGLE_PUBLIC_KEYS"
    pid <- getEnv "FIREBASE_PROJECT_ID"
    keyMap <- Aeson.decodeFileStrict pkeys >>= \case
      Nothing -> error "Could not decode key file"
      Just keyMap -> pure keyMap
    pure Firebase.FirebaseLoginSettings
      { Firebase.firebaseLoginProjectId = Firebase.ProjectId (T.pack pid)
      , Firebase.firebaseLoginGetKeys = pure keyMap
      }

getPostgresqlConnection :: IO HC.Connection
getPostgresqlConnection = do
    user <- getEnv "PGUSER"
    password <- getEnv "PGPASSWORD"
    host <- getEnv "PGHOST"
    db <- getEnv "PGDATABASE"
    port <- getEnv "PGPORT"
    HC.acquire (
      HC.settings
        (BS8.pack host)
        (read port)
        (BS8.pack user)
        (BS8.pack password)
        (BS8.pack db)
      ) >>= \case
        Left e -> error (show e)
        Right c -> pure c
