{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- XXX: !!!!IMPORTANT: CONCURRENCY 1 on this lambda!!!!
module DeckGo.Presenter where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Bifunctor
import Data.Function
import DeckGo.Prelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import DeckGo.Handler
import System.Environment
import System.FilePath
import qualified Network.AWS as Aws
import qualified Network.Mime as Mime
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.Data.Body as Body
import qualified System.Directory as Dir

listObjects :: Aws.Env -> S3.BucketName -> Maybe T.Text -> IO [S3.Object]
listObjects env bname mpref = xif ([],Nothing) $ \f (es, ct) ->
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
      Left e -> error $ "Could not list objects: " <> show e

deleteObjects :: Aws.Env -> S3.BucketName -> Maybe T.Text -> IO ()
deleteObjects env bname mpref = do
    es <- listObjects env bname mpref
    putStrLn $ "Deleting " <> show (length es) <> " objects..."
    forM_ es $ \((^. S3.oKey) -> okey) -> runAWS env (
      Aws.send $ S3.deleteObject bname okey) >>= \case
        Right {} -> pure ()
        Left e -> error $ "Could not delete object: " <> show e

deployPresentation :: Aws.Env -> Username -> Deckname -> IO ()
deployPresentation env uname dname = do
    let bname = S3.BucketName "deckgo-bucket"
    putStrLn "Deleting old objects..."
    deleteObjects env bname (Just (unUsername uname <> "/"))
    deckgoStarterDist <- getEnv "DECKGO_STARTER_DIST"
    putStrLn "Listing new files..."
    starterFiles <- listDirectoryRecursive deckgoStarterDist
    putStrLn "Copying objects..."
    forM_ starterFiles $ \(fp, pathComponents) -> do
      putStrLn $ "Copying " <> fp
      bs <- BS.readFile fp -- TODO: streaming
      runAWS env (
        Aws.send $ S3.putObject
          bname
          (mkObjectKey uname dname pathComponents)
          (Body.toBody bs) &
            -- XXX: partial, though technically should never fail
            S3.poContentType .~ inferContentType (last pathComponents)
        ) >>= \case
          Right {} -> pure ()
          Left e -> error $ "Error in put: " <> show e

mkObjectKey :: Username -> Deckname -> [T.Text] -> S3.ObjectKey
mkObjectKey uname dname components = S3.ObjectKey $ T.intercalate "/" $
  [unUsername uname] <> [unDeckname dname] <> components

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
