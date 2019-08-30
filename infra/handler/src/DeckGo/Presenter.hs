{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- XXX: !!!!IMPORTANT: CONCURRENCY 1 on this lambda!!!!
module DeckGo.Presenter where

import Control.Lens hiding ((.=))
import Control.Monad
import qualified Cases
import qualified Network.AWS.SQS as SQS
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import Data.Function
import Data.List (foldl')
import Data.Maybe
import Data.String
import DeckGo.Handler
import DeckGo.Prelude
import System.Environment
import System.FilePath
import UnliftIO
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Hasql.Connection as HC
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Body as Body
import qualified Network.AWS.S3 as S3
import qualified Network.Mime as Mime
import qualified System.Directory as Dir
import qualified System.IO.Temp as Temp
import qualified Text.HTML.TagSoup as TagSoup

data Err = Err T.Text SomeException
  deriving (Show, Exception)

err :: T.Text -> SomeException -> IO a
err msg e = throwIO $ Err msg e

-- | Diffs the bucket objects.
-- Returns:
--  * fst: the files to add
--  * snd: the files to delete
diffObjects
  :: [(FilePath, S3.ObjectKey, S3.ETag)]
  -> [(S3.ObjectKey, S3.ETag)]
  -> ([(FilePath, S3.ObjectKey, S3.ETag)], [S3.ObjectKey])
diffObjects news0 (HMS.fromList -> olds0) = second HMS.keys $
    foldl' (
      \(news, olds) obj@(_fp, okey, etag) -> do
          case HMS.lookup okey olds of
            Nothing -> (obj : news, olds)
            Just etag' ->
              if etag' == etag
              then (news, HMS.delete okey olds)
              else (obj : news, olds)
      ) ([], olds0) news0

listPresentationObjects
  :: AWS.Env
  -> S3.BucketName
  -> Username
  -> Deckname
  -> IO [S3.Object]
listPresentationObjects env bucket uname dname =
    listObjects env bucket (Just $ presentationPrefix uname dname)

withPresentationFiles
  :: Username
  -> Deck
  -> [Slide]
  -> ([(FilePath, S3.ObjectKey, S3.ETag)] -> IO a)
  -> IO a
withPresentationFiles uname deck slides act = do
    deckgoStarterDist <- getEnv "DECKGO_STARTER_DIST"
    Temp.withSystemTempDirectory "dist" $ \dir -> do
      Tar.extract dir deckgoStarterDist
      mapFile processIndex $ dir </> "index.html"
      mapFile interpol $ dir </> "manifest.json"
      putStrLn "Listing files..."
      files <- listDirectoryRecursive dir
      files' <- forM files $ \(fp, components) -> do
        etag <- fileETag fp
        let okey = mkObjectKey uname dname components
        pure (fp, okey, etag)
      act files'
  where
    dname = deckDeckname deck
    processIndex :: T.Text -> T.Text
    processIndex =
      TagSoup.renderTags . processTags deck slides . TagSoup.parseTags .
      interpol
    interpol =
      T.replace "{{DECKDECKGO_TITLE}}" (unDeckname dname) .
      T.replace "{{DECKDECKGO_AUTHOR}}" (unUsername uname) .
      T.replace "{{DECKDECKGO_USERNAME}}" (unUsername uname) .
      T.replace "{{DECKDECKGO_DECKNAME}}" (sanitizeDeckname dname) .
      -- TODO: description
      T.replace "{{DECKDECKGO_DESCRIPTION}}" "(no description given)" .
      T.replace "{{DECKDECKGO_BASE_HREF}}"
        ("/" <> presentationPrefix uname dname)

mapFile :: (T.Text -> T.Text) -> FilePath -> IO ()
mapFile f fp = do
    T.readFile fp >>= T.writeFile fp . f

type Tag = TagSoup.Tag T.Text

processTags :: Deck -> [Slide] -> [Tag] -> [Tag]
processTags deck slides = concatMap $ \case
  TagSoup.TagOpen str (HMS.fromList -> attrs)
    | str == "deckgo-deck" -> do
        [ TagSoup.TagOpen str (HMS.toList (deckAttributes deck <> attrs)) ] <>
          (concatMap slideTags slides) <>
          (maybe [] deckBackgroundTags (deckDeckbackground deck))
  t -> [t]

deckBackgroundTags :: Deckbackground -> [Tag]
deckBackgroundTags (unDeckbackground -> bg) =
    [ TagSoup.TagOpen "div" (HMS.toList $ HMS.singleton "slot" "background")
    ] <> TagSoup.parseTags bg <>
    [ TagSoup.TagClose "div"
    ]

slideTags :: Slide -> [Tag]
slideTags slide =
    [ TagSoup.TagOpen
        ("deckgo-slide-" <> slideTemplate slide)
        (first Cases.spinalize <$> HMS.toList (slideAttributes slide))
    ] <> maybe [] TagSoup.parseTags (slideContent slide) <>
    [ TagSoup.TagClose
        ("deckgo-slide-" <> slideTemplate slide)
    ]


listObjects :: AWS.Env -> S3.BucketName -> Maybe T.Text -> IO [S3.Object]
listObjects env bname mpref = xif ([],Nothing) $ \f (es, ct) ->
    runAWS env (AWS.send $ S3.listObjectsV2 bname &
      S3.lovContinuationToken .~ ct &
      S3.lovPrefix .~ mpref
      ) >>= \case
      Right r -> do
        putStrLn "Listed objects..."
        let objs = r ^. S3.lovrsContents
        case (r ^. S3.lovrsIsTruncated, r ^. S3.lovrsNextContinuationToken) of
          (Just True, Just ct') -> f (es <> objs, Just ct')
          _ -> pure (es <> objs)
      Left e -> err "Could not list objects" e

deleteObjects :: AWS.Env -> S3.BucketName -> Maybe T.Text -> IO ()
deleteObjects env bname mpref = do
    es <- listObjects env bname mpref
    putStrLn $ "Deleting " <> show (length es) <> " objects..."
    deleteObjects' env bname $ map (^. S3.oKey) es

deleteObjects' :: AWS.Env -> S3.BucketName -> [S3.ObjectKey] -> IO ()
deleteObjects' env bname okeys =
    forConcurrentlyN_ 10 okeys $ \okey -> runAWS env (
      AWS.send $ S3.deleteObject bname okey) >>= \case
        Right {} -> pure ()
        Left e -> error $ "Could not delete object: " <> show e

-- TODO: sanitize deck name
deployDeck :: AWS.Env -> HC.Connection -> DeckId -> IO ()
deployDeck env conn deckId = do
    iface <- liftIO $ getDbInterface conn
    dbGetDeckById iface deckId >>= \case
      Nothing -> pure () -- TODO
      Just deck -> do
        liftIO (fmap itemContent <$> dbGetUserById iface (deckOwnerId deck)) >>= \case
          Nothing -> pure () -- TODO
          Just user -> case userUsername user of
            Nothing -> pure () -- TODO
            Just uname -> do
              slides <- catMaybes <$> mapM (dbGetSlideById iface) (deckSlides deck)
              deployPresentation env uname deck slides

deployPresentation :: AWS.Env -> Username -> Deck -> [Slide] -> IO ()
deployPresentation env uname deck slides = do
    bucketName <- getEnv "BUCKET_NAME"
    let bucket = S3.BucketName (T.pack bucketName)
    let dname = deckDeckname deck
    putStrLn "Listing current objects"
    currentObjs <- listPresentationObjects env bucket uname dname
    putStrLn "Listing presentations files"



    withPresentationFiles uname deck slides $ \files -> do
      let
        currentObjs' =
          (\obj ->
            (obj ^. S3.oKey, fixupS3ETag $ obj ^. S3.oETag)
          ) <$> currentObjs
        (toPut, toDelete) = diffObjects files currentObjs'
      putStrLn $ "Deleting " <> show (length toDelete) <> " old files"
      deleteObjects' env bucket toDelete
      putStrLn $ "Uploading " <> show (length toPut) <> " new files"
      putObjects env bucket toPut

    queueName <- liftIO $ T.pack <$> getEnv "QUEUE_NAME"

    -- TODO: cleaner error handling down here

    liftIO $ putStrLn $ "Forwarding to queue: " <> T.unpack queueName
    queueUrl <- runAWS env (AWS.send $ SQS.getQueueURL queueName) >>= \case
      Right e -> pure $ e ^. SQS.gqursQueueURL
      Left e -> do
        liftIO $ print e
        error "Failed"

    liftIO $ print queueUrl

    res <- runAWS env $ AWS.send $ SQS.sendMessage queueUrl $
      T.decodeUtf8 $ BL.toStrict $ Aeson.encode (presentationPrefix uname dname)

    case res of
      Right r -> do
        liftIO $ print r
      Left e -> do
        liftIO $ print e
        error "Failed!!"

putObjects
  :: AWS.Env
  -> S3.BucketName
  -> [(FilePath, S3.ObjectKey, S3.ETag)]
  -> IO ()
putObjects env bucket objs = forConcurrentlyN_ 10 objs $ putObject env bucket

putObject
  :: AWS.Env
  -> S3.BucketName
  -> (FilePath, S3.ObjectKey, S3.ETag)
  -> IO ()
putObject env bucket (fp, okey, etag) = do
    body <- Body.toBody <$> BS.readFile fp
    runAWS env (
      AWS.send $ S3.putObject bucket okey body &
          -- XXX: partial, though technically should never fail
          S3.poContentType .~ inferContentType (T.pack fp)
      ) >>= \case
        Right r -> do
          putStrLn $ "Copied: " <> fp <> " to " <> show okey <> " with ETag " <> show etag
          case r ^. S3.porsETag of
            Just (fixupS3ETag -> s3ETag) ->
              when (etag /= s3ETag) $ do
                putStrLn $ "Warning: mismatched MD5: expected : actual: " <>
                  show etag <> " : " <> show s3ETag
            Nothing -> putStrLn "Warning: no ETag"
        Left e -> error $ "Error in put: " <> show e

-- | Some of the ETags we receive from S3 are surrounded with /"/ chars so we
-- strip them
fixupS3ETag :: S3.ETag -> S3.ETag
fixupS3ETag (S3.ETag etag) =
    S3.ETag $
      T.encodeUtf8 $
      T.dropWhileEnd (== '"') $
      T.dropWhile (== '"') $
      T.decodeUtf8 etag

mkObjectKey :: Username -> Deckname -> [T.Text] -> S3.ObjectKey
mkObjectKey uname dname components = S3.ObjectKey $
    presentationPrefix uname dname <> T.intercalate "/" components

fileETag :: FilePath -> IO S3.ETag
fileETag fp =
    -- XXX: The 'show' step is very import, it's what converts the Digest to
    -- the Hex representation
    (fromString . show . MD5.md5) <$> BL.readFile fp

inferContentType :: T.Text -> Maybe T.Text
inferContentType = Just . T.decodeUtf8 .
    Mime.mimeByExt Mime.defaultMimeMap Mime.defaultMimeType

listDirectoryRecursive :: FilePath -> IO [(FilePath, [T.Text])]
listDirectoryRecursive = fix $ \f dir -> do
    -- XXX: /so/ not tail recursive
    (dirs, fs) <- Dir.listDirectory dir >>=
      mapM (\component -> pure (dir </> component, [T.pack component])) >>=
      partitionM (Dir.doesDirectoryExist . fst)
    fs' <- concatMapM
      (\(dir', components) -> fmap (second (components <>)) <$> (f dir'))
      dirs
    pure (fs <> fs')

-- Data.List for Monad

-- | A version of 'partition' that works with a monadic predicate.
--
-- > partitionM (Just . even) [1,2,3] == Just ([2], [1,3])
-- > partitionM (const Nothing) [1,2,3] == Nothing
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM act = foldr f (return [])
    where f x xs = do x' <- act x; if null x' then xs else do xs' <- xs; return $ x' ++xs'

forConcurrentlyN_
  :: (MonadUnliftIO m) => Int -> [a] -> (a -> m b) -> m ()
forConcurrentlyN_ n xs act = forConcurrently_ (nChunks n xs) (mapM_ act)

nChunks :: Int -> [a] -> [[a]]
nChunks n xs = HMS.elems $ snd $ foldl'
    (\(i, m) v -> (i+1, HMS.insertWith (<>) (i `mod` n) [v] m) )
    (0, HMS.empty) xs
