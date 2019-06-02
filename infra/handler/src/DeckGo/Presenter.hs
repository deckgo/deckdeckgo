{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- XXX: !!!!IMPORTANT: CONCURRENCY 1 on this lambda!!!!
module DeckGo.Presenter where

import Control.Lens hiding ((.=))
import Data.Bifunctor
import Control.Monad
import Data.String
import Data.Function
import Data.List (foldl')
import DeckGo.Handler
import DeckGo.Prelude
import System.Environment
import System.FilePath
import UnliftIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.AWS as Aws
import qualified Network.AWS.Data as Data
import qualified Network.AWS.Data.Body as Body
import qualified Network.AWS.S3 as S3
import qualified Network.Mime as Mime
import qualified System.Directory as Dir

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
  :: Aws.Env
  -> S3.BucketName
  -> Username
  -> Deckname
  -> IO [S3.Object]
listPresentationObjects env bucket uname dname =
    listObjects env bucket (Just $ presentationPrefix uname dname)

presentationFiles
  :: Username
  -> Deckname
  -> IO [(FilePath, S3.ObjectKey, S3.ETag)]
presentationFiles uname dname = do
    deckgoStarterDist <- getEnv "DECKGO_STARTER_DIST"
    putStrLn "Listing files..."
    files <- listDirectoryRecursive deckgoStarterDist
    forM files $ \(fp, components) -> do
      etag <- fileETag fp
      let okey = mkObjectKey uname dname components
      pure (fp, okey, etag)

listObjects :: Aws.Env -> S3.BucketName -> Maybe T.Text -> IO [S3.Object]
listObjects (fixupEnv' -> env) bname mpref = xif ([],Nothing) $ \f (es, ct) ->
    runAWS env (Aws.send $ S3.listObjectsV2 bname &
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

deleteObjects :: Aws.Env -> S3.BucketName -> Maybe T.Text -> IO ()
deleteObjects (fixupEnv' -> env) bname mpref = do
    es <- listObjects env bname mpref
    putStrLn $ "Deleting " <> show (length es) <> " objects..."
    deleteObjects' env bname $ map (^. S3.oKey) es

deleteObjects' :: Aws.Env -> S3.BucketName -> [S3.ObjectKey] -> IO ()
deleteObjects' (fixupEnv' -> env) bname okeys =
    forConcurrentlyN_ 10 okeys $ \okey -> runAWS env (
      Aws.send $ S3.deleteObject bname okey) >>= \case
        Right {} -> pure ()
        Left e -> error $ "Could not delete object: " <> show e

deployPresentation :: Aws.Env -> Username -> Deckname -> IO ()
deployPresentation (fixupEnv' -> env) uname dname = do
    bucketName <- getEnv "BUCKET_NAME"
    let bucket = S3.BucketName (T.pack bucketName)
    putStrLn "Listing current objects"
    currentObjs <- listPresentationObjects env bucket uname dname
    putStrLn "Listing presentations files"
    files <- presentationFiles uname dname
    let
      currentObjs' = (\obj -> (obj ^. S3.oKey, obj ^. S3.oETag)) <$> currentObjs
      (toPut, toDelete) = diffObjects files currentObjs'
    putStrLn "Deleting old files"
    deleteObjects' env bucket toDelete
    putStrLn "Uploading new files"
    putObjects env bucket toPut

putObjects
  :: Aws.Env
  -> S3.BucketName
  -> [(FilePath, S3.ObjectKey, S3.ETag)]
  -> IO ()
putObjects env bucket objs = forConcurrentlyN_ 10 objs $ putObject env bucket

putObject
  :: Aws.Env
  -> S3.BucketName
  -> (FilePath, S3.ObjectKey, S3.ETag)
  -> IO ()
putObject (fixupEnv' -> env) bucket (fp, okey, etag) = do
    body <- Body.toBody <$> BS.readFile fp
    runAWS env (
      Aws.send $ S3.putObject bucket okey body &
          -- XXX: partial, though technically should never fail
          S3.poContentType .~ inferContentType (T.pack fp)
      ) >>= \case
        Right r -> do
          putStrLn $ "Copied: " <> fp
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

-- | Transforms the request to hit the region-specific S3, otherwise this
-- doesn't go through the VPC endpoint.
-- TODO: move this to 'fixupEnv'
fixupEnv' :: Aws.Env -> Aws.Env
fixupEnv' = Aws.configure $ S3.s3
  { Aws._svcEndpoint = \reg -> do
      let new = "s3." <> Data.toText reg <> ".amazonaws.com"
      (Aws._svcEndpoint S3.s3 reg) & Aws.endpointHost .~ T.encodeUtf8 new
  }

presentationPrefix :: Username -> Deckname -> T.Text
presentationPrefix uname dname = T.intercalate "/" $
    [unUsername uname] <> [unDeckname dname] -- TODO: deckname escaping

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
