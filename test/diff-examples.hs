{-# LANGUAGE TupleSections #-}

-- | Test examples from RFC 6902 sections A.1 to A.16.

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Diff
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List                  (isInfixOf, nub)
import           Data.Maybe
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob

roots :: [FilePath]
roots = ["test/data/diff-cases"]

globPattern :: FilePath
globPattern = "*.*"

derp :: String -> a
derp msg = throw (AssertionFailed $ " " <> msg)

readOriginal :: FilePath -> FilePath -> IO Value
readOriginal root name = do
    let file = root </> name <> "-a.json"
    doc <- eitherDecodeStrict <$> BS.readFile file

    return $ either (\e -> derp $ "Could not decode original: " <> e) id doc

readChanged :: FilePath -> FilePath -> IO Value
readChanged root name = do
    let file = root </> name <> "-b.json"
    doc <- eitherDecodeStrict <$> BS.readFile file

    return $ either (\e -> derp $ "Could not decode changed: " <> e) id doc

readPatch :: FilePath -> FilePath -> IO Patch
readPatch root name = do
    let file = root </> name <> "-patch.json"
    doc <- eitherDecodeStrict <$> BS.readFile file

    return $ either (\e -> derp $ "Could not decode patch: " <> e) id doc

readExample :: FilePath -> FilePath -> IO (Value, Value, Patch)
readExample root name =
    (,,) <$> readOriginal root name
         <*> readChanged root name
         <*> readPatch root name

-- | Check example and, if it fails, return an error message.
runExample :: (Value, Value, Patch) -> Maybe String
runExample (doc, changed, res) =
    case diff doc changed of
        dest | dest == res -> success "Result matches target"
             | otherwise   -> failure ("Result patch did not match: " <> BL.unpack (encode dest))
  where
    success n = Nothing
    failure n = Just ("Test Fails - " <> n)

testExample :: FilePath -> FilePath -> IO (Maybe String)
testExample root name = do
    r <- try (runExample <$> readExample root name)
    return $ either err id r
  where
    err :: AssertionFailed -> Maybe String
    err e = Just ("Error: " <> show e)

runSuite :: FilePath -> IO [(FilePath, Maybe String)]
runSuite root = do
    -- Gather directories in test/data
    let p = simplify (compile globPattern)
    examples <- nub . fmap (takeWhile (/= '-')) . filter (match p) <$> getDirectoryContents root
    -- Test each of them
    mapM (\nom -> (nom,) <$> testExample root nom) examples

main :: IO ()
main = do
    args <- getArgs
    results <- concat <$> mapM runSuite (if null args then roots else args)
    mapM_ display results
    -- Failure.
    when (any (isJust . snd) results)
        exitFailure
  where
    display :: (FilePath, Maybe String) -> IO ()
    display (name, Nothing) =
        putStrLn $ "SUCCESS: " <> name
    display (name, Just err) =
        putStrLn $ "FAILURE: " <> name <> ": " <> err
