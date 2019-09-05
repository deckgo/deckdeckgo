{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad
import Data.List (sortOn)
import Data.Monoid (First)
import DeckGo.Handler
import DeckGo.Presenter
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
import qualified Network.AWS.Extended as AWS
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.SQS as SQS
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClient
import qualified Network.Socket.Wait as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Auth.Firebase as Firebase
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

withEnv :: (AWS.Env -> IO a) -> IO a
withEnv act = do
    mgr <- HTTPClient.newManager HTTPClient.tlsManagerSettings
            { HTTPClient.managerModifyRequest =
                pure . rerouteSQS . rerouteS3
            }
    env <- AWS.newEnv <&> AWS.envManager .~ mgr
    act env

withServer :: (Warp.Port -> IO a) -> IO a
withServer act =
    withPresURL $ withEnv $ \env -> withS3 env $ withSQS env $
      withPristineDB $ \conn -> do
        putStrLn "Server environment loaded, finding port"
        (port, socket) <- Warp.openFreePort
        let warpSettings = Warp.setPort port $ Warp.defaultSettings
        settings <- getFirebaseSettings
        putStrLn "Starting server"
        race
          (Warp.runSettingsSocket warpSettings socket $
            DeckGo.Handler.application settings env conn)
          (do
            putStrLn "Waiting for server..."
            Socket.wait "0.0.0.0" port
            putStrLn "Server ready!"
            act port
          ) >>= \case
            Left () -> error "Server returned"
            Right a -> pure a
  where
    withPresURL =
      bracket_ (setEnv "DECKGO_PRESENTATIONS_URL" "foo.bar.baz") (unsetEnv "DECKGO_PRESENTATIONS_URL")

is'
  :: AWS.AsError a
  => Getting (First AWS.ServiceError) a AWS.ServiceError
  -> a
  -> Bool
is' prsm v = is _Just $ v ^? prsm

withSQS :: AWS.Env -> IO a -> IO a
withSQS env act = withQueueName $ do
    runAWS env (AWS.send $ SQS.getQueueURL ttestQueueName) >>= \case
      Right r -> runAWS env (AWS.send $
          SQS.deleteQueue ttestQueueName & SQS.dqQueueURL .~ (r ^. SQS.gqursQueueURL)
          ) >>= \case
        Left e -> error $ "Could not delete queue: " <> show e
        Right {} -> pure ()
      Left e
        | is' SQS._QueueDoesNotExist e -> pure ()
        | otherwise -> error $ "Could not get queue URL: " <> show e

    putStrLn $ "Creating " <> testQueueName

    runAWS env (AWS.send $ SQS.createQueue ttestQueueName) >>= \case
      Left e -> error $ "Could not create queue: " <> show e
      Right {} -> pure ()
    act
  where
    testQueueName = "the-queue"
    ttestQueueName = T.pack testQueueName
    withQueueName =
      bracket_ (setEnv "QUEUE_NAME" testQueueName) (unsetEnv "QUEUE_NAME")

withS3 :: AWS.Env -> IO a -> IO a
withS3 env act = do
    let bucket = S3.BucketName bucketName
    putStrLn "Emptying bucket, if exists"
    try (deleteObjects env bucket Nothing) >>= \case
      Right () -> pure ()
      Left (Err msg e)
        | is' S3._NoSuchBucket e -> pure ()
        | otherwise -> error $ T.unpack msg <> ": " <> show e

    putStrLn "Deleting bucket, if exists"
    runAWS env (AWS.send $ S3.deleteBucket bucket) >>= \case
      Right {} -> pure ()
      Left e
        | is' S3._NoSuchBucket e -> pure ()
        | otherwise -> error $ "Could not delete bucket: " <> show e

    putStrLn "Creating bucket"

    runAWS env (AWS.send $ S3.createBucket bucket) >>= \case
      Right {} -> pure ()
      Left e -> error $ "Could not create bucket: " <> show e
    withBucketName act
  where
    bucketName = "deckgo-bucket-foo"
    withBucketName =
      bracket_ (setEnv "BUCKET_NAME" (T.unpack bucketName)) (unsetEnv "BUCKET_NAME")

withPristineDB :: (HC.Connection -> IO a) -> IO a
withPristineDB act = do
    conn <- getPostgresqlConnection
    putStrLn "DROP TABLE IF EXISTS username"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS username") conn
    putStrLn "DROP TABLE IF EXISTS account CASCADE"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS account CASCADE") conn
    putStrLn "DROP TABLE IF EXISTS slide"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS slide") conn
    putStrLn "DROP TABLE IF EXISTS deck"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS deck") conn
    putStrLn "DROP TABLE IF EXISTS db_meta"
    void $ HS.run (HS.sql "DROP TABLE IF EXISTS db_meta") conn
    act conn

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "1"
    Tasty.defaultMain $ Tasty.testGroup "tests"
      [ Tasty.testGroup "db"
          [ Tasty.testGroup "users"
              [ Tasty.testCase "get" testUsersGet
              , Tasty.testCase "create" testUsersCreate
              , Tasty.testCase "get by id" testUsersGetByUserId
              , Tasty.testCase "delete" testUsersDelete
              , Tasty.testCase "update" testUsersUpdate
              ]
          , Tasty.testGroup "decks"
              [ Tasty.testCase "create" testDecksCreate
              , Tasty.testCase "update" testDecksUpdate
              , Tasty.testCase "get by id" testDecksGetById
              , Tasty.testCase "get all" testDecksGetAll
              ]
          , Tasty.testGroup "slides"
              [ Tasty.testCase "get" testSlidesGet
              , Tasty.testCase "create" testSlidesCreate
              , Tasty.testCase "update" testSlidesUpdate
              ]
          ]
      , Tasty.testCase "presentation" testPresDeploys
      , Tasty.testCase "server" testServer
      ]

testPresDeploys :: IO ()
testPresDeploys = withQueueName $ withEnv $ \env -> withSQS env $ withS3 env $ do
    let someFirebaseId = FirebaseId "the-uid" -- from ./token
    let someUserId = UserId someFirebaseId

    let someSlide = Slide (Just "foo") "bar" HMS.empty
        someSlideId = SlideId "foo-id"

    let newDeck = Deck
          { deckSlides = [ someSlideId ]
          , deckDeckname = Deckname "bar"
          , deckDeckbackground = Just (Deckbackground "bar")
          , deckOwnerId = someUserId
          , deckAttributes = HMS.singleton "foo" "bar"
          }

    deployPresentation env (Username "josph") newDeck [someSlide]
    -- XXX: tests the obj diffing by making sure we can upload a presentation
    -- twice without errors
    deployPresentation env (Username "josph") newDeck [someSlide]
  where
    testQueueName = "the-queue"
    withQueueName =
      bracket_ (setEnv "QUEUE_NAME" testQueueName) (unsetEnv "QUEUE_NAME")

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

withDeck
  :: DbInterface
  -> (DeckId -> Deck -> IO ())
  -> IO ()
withDeck iface act = do
    let someFirebaseId = FirebaseId "foo"
        someUserId = UserId someFirebaseId
        someUser = User
          { userFirebaseId = someFirebaseId
          , userUsername = Just (Username "patrick")
          }
    dbCreateUser iface someUserId someUser >>= \case
      Left () -> error "Encountered error"
      Right () -> pure ()

    let someDeckId = DeckId "bar"
        someDeck = Deck
          { deckSlides = []
          , deckDeckname = Deckname "Some deck!!"
          , deckDeckbackground  = Nothing
          , deckOwnerId  = someUserId
          , deckAttributes = HMS.singleton "foo" "bar"
          }
    dbCreateDeck iface someDeckId someDeck
    act someDeckId someDeck

testDecksGetAll :: IO ()
testDecksGetAll = withPristineDB $ \conn -> do
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

    someDecks <- forM [(0 :: Int) .. 10] $ \i -> do
      let someDeckId = DeckId $ "bar-" <> tshow i
          someDeck = Deck
            { deckSlides = []
            , deckDeckname = Deckname $ "Some deck!! - " <> tshow i
            , deckDeckbackground  = Nothing
            , deckOwnerId  = someUserId
            , deckAttributes = HMS.singleton "foo" "bar"
            }
      dbCreateDeck iface someDeckId someDeck
      pure someDeckId

    dbGetAllDecks iface >>= \case
      decks -> unless
        (sortOn unDeckId (itemId <$> decks) == sortOn unDeckId someDecks) $
        error "Bad decks"

testDecksCreate :: IO ()
testDecksCreate = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    withDeck iface $ \someDeckId someDeck -> do
      dbGetDeckById iface someDeckId >>= \case
        Nothing -> error "couldn't find deck"
        Just deck -> unless (deck == someDeck) $ error "Bad deck"

testDecksGetById :: IO ()
testDecksGetById = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    withDeck iface $ \someDeckId someDeck -> do
      slides <- forM [(0 :: Int)..10] $ \i -> do
        let someSlideId = SlideId $ "foo-" <> tshow i
            someSlide = Slide
              { slideContent = Nothing
              , slideTemplate = "The template"
              , slideAttributes = HMS.singleton "foo" "bar"
              }
        dbCreateSlide iface someSlideId someDeckId someSlide
        pure someSlideId
      let someDeck' = someDeck { deckSlides = slides }
      dbUpdateDeck iface someDeckId someDeck'
      dbGetDeckById iface someDeckId >>= \case
        Nothing -> error "couldn't find deck"
        Just deck -> unless (deck == someDeck') $ error $ unlines
          [ "Bad deck\n"
          , show someDeck'
          , show deck
          ]

testDecksUpdate :: IO ()
testDecksUpdate = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    withDeck iface $ \someDeckId someDeck -> do
      let someSlideId = SlideId "foo"
          someSlide = Slide
            { slideContent = Nothing
            , slideTemplate = "The template"
            , slideAttributes = HMS.singleton "foo" "bar"
            }
      dbCreateSlide iface someSlideId someDeckId someSlide

      let someDeck' = someDeck { deckSlides = [ someSlideId ] }

      dbUpdateDeck iface someDeckId someDeck'

testSlidesGet :: IO ()
testSlidesGet = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    withDeck iface $ \someDeckId _someDeck -> do
      let someSlideId = SlideId "foo"
          someSlide = Slide
            { slideContent = Nothing
            , slideTemplate = "The template"
            , slideAttributes = HMS.singleton "foo" "bar"
            }
      dbCreateSlide iface someSlideId someDeckId someSlide
      dbGetSlideById iface someSlideId >>= \case
        Nothing -> error "couldn't find slide"
        Just slide -> unless (slide == someSlide) $ error "Bad slide"

testSlidesCreate :: IO ()
testSlidesCreate = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    withDeck iface $ \someDeckId _someDeck -> do
      let someSlideId = SlideId "foo"
          someSlide = Slide
            { slideContent = Nothing
            , slideTemplate = "The template"
            , slideAttributes = HMS.singleton "foo" "bar"
            }
      dbCreateSlide iface someSlideId someDeckId someSlide

testSlidesUpdate :: IO ()
testSlidesUpdate = withPristineDB $ \conn -> do
    iface <- getDbInterface conn
    withDeck iface $ \someDeckId _someDeck -> do
      let someSlideId = SlideId "foo"
          someSlide = Slide
            { slideContent = Nothing
            , slideTemplate = "The template"
            , slideAttributes = HMS.singleton "foo" "bar"
            }
      dbCreateSlide iface someSlideId someDeckId someSlide

      let someOtherSlide = Slide
            { slideContent = Just "Some content"
            , slideTemplate = "The template"
            , slideAttributes = HMS.singleton "foo" "baz"
            }

      dbUpdateSlide iface someSlideId someOtherSlide
      dbGetSlideById iface someSlideId >>= \case
        Nothing -> error "couldn't find slide"
        Just slide -> unless (slide == someOtherSlide) $ error "Bad slide"

getTokenPath :: IO FilePath
getTokenPath =
    lookupEnv "TEST_TOKEN_PATH" >>= \case
      Just tpath -> pure tpath
      Nothing -> pure "./token"

testServer :: IO ()
testServer = withServer $ \port -> do
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
    Left e -> error $ "Expected users, got error: " <> show e
    Right [] -> pure ()
    Right users -> error $ "Expected 0 users, got: " <> show users

  runClientM (decksGet' b (Just someUserId)) clientEnv >>= \case
    -- TODO: shouldn't this be a 404?
    Left e -> error $ "Expected decks, got error: " <> show e
    Right [] -> pure ()
    Right decks -> error $ "Expected 0 decks, got: " <> show decks

  let someUserInfo = UserInfo
        { userInfoFirebaseId = someFirebaseId
        , userInfoEmail = Just "patrick@foo.com" }
      Right someUser = userInfoToUser someUserInfo

  runClientM (usersPost' b someUserInfo) clientEnv >>= \case
    Left e -> error $ "Expected user, got error: " <> show e
    Right (Item userId user) ->
      if user == someUser && userId == someUserId then pure () else (error $ "Expected same user, got: " <> show user)

  runClientM (usersPost' b someUserInfo) clientEnv >>= \case
    -- TODO: test that user is returned here, even on 409
    Left (FailureResponse resp) ->
      if HTTP.statusCode (responseStatusCode resp) == 409 then pure () else
        error $ "Got unexpected response: " <> show resp
    Left e -> error $ "Expected 409, got error: " <> show e
    Right item -> error $ "Expected failure, got success: " <> show item

  deckId <- runClientM (decksPost' b someDeck) clientEnv >>= \case
    Left e -> error $ "Expected new deck, got error: " <> show e
    Right (Item deckId _) -> pure deckId

  let someSlide = Slide (Just "foo") "bar" HMS.empty

  slideId <- runClientM (slidesPost' b deckId someSlide) clientEnv >>= \case
    Left e -> error $ "Expected new slide, got error: " <> show e
    Right (Item slideId _) -> pure slideId

  let newDeck = Deck
        { deckSlides = [ slideId ]
        , deckDeckname = Deckname "bar"
        , deckDeckbackground = Just (Deckbackground "bar")
        , deckOwnerId = someUserId
        , deckAttributes = HMS.singleton "foo" "bar"
        }

  runClientM (decksPut' b deckId newDeck) clientEnv >>= \case
    Left e -> error $ "Expected updated deck, got error: " <> show e
    Right {} -> pure ()

  runClientM (decksPostPublish' b deckId) clientEnv >>= \case
    Left e -> error $ "Expected publish, got error: " <> show e
    Right {} -> pure ()

  runClientM (decksGet' b (Just someUserId)) clientEnv >>= \case
    Left e -> error $ "Expected decks, got error: " <> show e
    Right decks ->
      if decks == [Item deckId newDeck] then pure () else (error $ "Expected updated decks, got: " <> show decks)

  runClientM (decksGetDeckId' b deckId) clientEnv >>= \case
    Left e -> error $ "Expected decks, got error: " <> show e
    Right (Item _deckId deck) ->
      if deck == newDeck then pure () else (error $ "Expected get deck, got: " <> show deck)

  let updatedSlide = Slide Nothing "quux" HMS.empty

  runClientM (slidesPut' b deckId slideId updatedSlide) clientEnv >>= \case
    Left e -> error $ "Expected new slide, got error: " <> show e
    Right {} -> pure ()

  runClientM (slidesPut' b deckId slideId updatedSlide) clientEnv >>= \case
    Left e -> error $ "Expected new slide, got error: " <> show e
    Right {} -> pure ()

  runClientM (slidesGetSlideId' b deckId slideId) clientEnv >>= \case
    Left e -> error $ "Expected updated slide, got error: " <> show e
    Right slide ->
      if slide == (Item slideId updatedSlide) then pure () else (error $ "Expected updated slide, got: " <> show slide)

  runClientM (slidesDelete' b deckId slideId) clientEnv >>= \case
    Left e -> error $ "Expected slide delete, got error: " <> show e
    Right {} -> pure ()

  runClientM (decksDelete' b deckId) clientEnv >>= \case
    Left e -> error $ "Expected deck delete, got error: " <> show e
    Right {} -> pure ()

  runClientM (decksGet' b (Just someUserId)) clientEnv >>= \case
    Left e -> error $ "Expected no decks, got error: " <> show e
    Right decks ->
      unless (decks == []) (error $ "Expected no decks, got: " <> show decks)

  -- TODO: test that creating user with token that has different user as sub
  -- fails

usersGet' :: ClientM [Item UserId User]
_usersGetUserId' :: UserId -> ClientM (Item UserId User)
usersPost' :: T.Text -> UserInfo -> ClientM (Item UserId User)
_usersPut' :: T.Text -> UserId -> User -> ClientM (Item UserId User)
_usersDelete' :: T.Text -> UserId -> ClientM ()

decksGet' :: T.Text -> Maybe UserId -> ClientM [Item DeckId Deck]
decksGetDeckId' :: T.Text -> DeckId -> ClientM (Item DeckId Deck)
decksPostPublish' :: T.Text -> DeckId -> ClientM PresResponse
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
  decksPostPublish' :<|>
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

rerouteSQS :: HTTPClient.Request -> HTTPClient.Request
rerouteSQS req =
    case HTTPClient.host req of
      "sqs.us-east-1.amazonaws.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 9324 -- TODO: read from Env
          , HTTPClient.secure = False
          }
      "queue.amazonaws.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 9324 -- TODO: read from Env
          , HTTPClient.secure = False
          }
      _ -> req

rerouteS3 :: HTTPClient.Request -> HTTPClient.Request
rerouteS3 req =
    case HTTPClient.host req of
      "s3.us-east-1.amazonaws.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 9000 -- TODO: read from Env
          , HTTPClient.secure = False
          }
      "s3.amazonaws.com" ->
        req
          { HTTPClient.host = "127.0.0.1"
          , HTTPClient.port = 9000 -- TODO: read from Env
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
