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
    forConcurrentlyN_ 10 es $ \((^. S3.oKey) -> okey) -> runAWS env (
      Aws.send $ S3.deleteObject bname okey) >>= \case
        Right {} -> pure ()
        Left e -> error $ "Could not delete object: " <> show e

deployPresentation :: Aws.Env -> Username -> Deckname -> IO ()
deployPresentation (fixupEnv' -> env) uname dname = do
    bucketName <- getEnv "BUCKET_NAME"
    let bucket = S3.BucketName (T.pack bucketName)
    putStrLn "Deleting old objects..."
    deleteObjects env bucket (Just (unUsername uname <> "/"))
    deckgoStarterDist <- getEnv "DECKGO_STARTER_DIST"
    putStrLn "Listing new files..."
    starterFiles <- listDirectoryRecursive deckgoStarterDist
    putStrLn "Copying objects..."
    forConcurrentlyN_ 10 starterFiles $ \(fp, pathComponents) -> do
      putStrLn $ "Copying " <> fp
      bs <- BS.readFile fp -- TODO: streaming
      let expectedETag = show $ MD5.md5 $ BL.fromStrict bs
      putStrLn $ "Copying " <> fp <> " | md5: " <> expectedETag
      runAWS env (
        Aws.send $ S3.putObject
          bucket
          (mkObjectKey uname dname pathComponents)
          (Body.toBody bs) &
            -- XXX: partial, though technically should never fail
            S3.poContentType .~ inferContentType (last pathComponents)
        ) >>= \case
          Right r -> do
            putStrLn $ "Copied: " <> fp
            case r ^. S3.porsETag of
              Just (readETag -> actualETag) ->
                when (expectedETag /= actualETag) $ do
                  putStrLn $ "Warning: mismatched MD5: expected : actual: " <>
                    expectedETag <> " : " <> actualETag
              Nothing -> putStrLn "Warning: no ETag"
          Left e -> error $ "Error in put: " <> show e
    putStrLn "Done copying objects."

readETag :: S3.ETag -> String
readETag (S3.ETag etag) =
    T.unpack $
      T.dropWhileEnd (== '"') $
      T.dropWhile (== '"') $
      T.decodeUtf8 etag

fixupEnv' :: Aws.Env -> Aws.Env
fixupEnv' = Aws.configure $ S3.s3
  { Aws._svcEndpoint = \reg -> do
      let new = "s3." <> Data.toText reg <> ".amazonaws.com"
      (Aws._svcEndpoint S3.s3 reg) & Aws.endpointHost .~ T.encodeUtf8 new
  }

mkObjectKey :: Username -> Deckname -> [T.Text] -> S3.ObjectKey
mkObjectKey uname dname components = S3.ObjectKey $ T.intercalate "/" $
  [unUsername uname] <> [unDeckname dname] <> components -- TODO: present escaping

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
