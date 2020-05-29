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
roots = ["test/data/rfc6902", "test/data/patch-cases"]

globPattern :: FilePath
globPattern = "*.*"

derp :: String -> a
derp msg = throw (AssertionFailed $ " " <> msg)

readDocument :: FilePath -> FilePath -> IO Value
readDocument root name = do
    let file = root </> name <> "-original.json"
    doc <- eitherDecodeStrict <$> BS.readFile file

    return $ either (\e -> derp $ "Could not decode document: " <> e) id doc

readPatch :: FilePath -> FilePath -> IO (Either String Patch)
readPatch root name = do
    let file = root </> name <> "-patch.json"

    eitherDecodeStrict <$> BS.readFile file

readResult :: FilePath -> FilePath -> IO (Either String Value)
readResult root name = do
    let err_path = root </> name <> "-error.txt"
    let doc_path = root </> name <> "-result.json"

    err <- catch (Just . BC.unpack . BC.dropWhile isSpace . fst . BC.spanEnd isSpace
                 <$> BS.readFile err_path) handle

    doc <- catch (decodeStrict <$> BS.readFile doc_path) handle

    case (err, doc) of
      (Nothing, Just d)  -> return (Right d)
      (Just er, Nothing) -> return (Left er)
      (Just er, Just d)  -> derp "Expecting both error and success"
      (Nothing, Nothing) -> derp "No result defined; add `*-error.txt' or `*-result.json'"
  where
    handle :: IOException -> IO (Maybe a)
    handle e = return Nothing

readExample :: FilePath -> FilePath -> IO (Value, Either String Patch , Either String Value)
readExample root name =
    (,,) <$> readDocument root name
         <*> readPatch root name
         <*> readResult root name

-- | Check example and, if it fails, return an error message.
runExample :: (Value, Either String Patch, Either String Value) -> Maybe String
runExample (doc, diff, res) =
    case (diff, res) of
        (Left perr, Left err)
            | err `isInfixOf` perr -> success "Patch has expected error."
            | perr `isInfixOf` err -> success "Patch has expected error."
            | otherwise   -> failure ("Unexpected error `" <> perr <> "' was not '" <> err <> "'.")
        (Left err, Right _) ->
            failure ("Couldn't load patch: " <> err)
        (Right diff, Right res) ->
            case patch diff doc of
              Success dest
                  | dest == res -> success "Result matches target"
                  | otherwise -> failure ("Result document did not match: " <> BL.unpack (encode dest))
              Error dest -> failure ("Couldn't apply patch " <> dest)
        (Right diff, Left err) ->
            case patch diff doc of
              Success _ -> Just "Test Fails - Expected a failure but patch succeeded."
              Error msg
                  | msg /= err -> Just $ "Test Fails - Got: " <> msg <> "\nExpected: " <> err
                  | otherwise  -> Nothing
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
