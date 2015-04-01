{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Exception
import Data.Aeson
import Data.Aeson.Diff
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Options.Applicative hiding (Success)
import Options.Applicative.Types hiding (Success)
import System.IO

type File = Maybe FilePath

-- | Command-line options.
data Options = Options
    { optionJSON  :: Bool -- ^ Patch is JSON?
    , optionOut   :: File -- ^ JSON destination
    , optionPatch :: File -- ^ Patch input
    , optionFrom  :: File -- ^ JSON source
    }

data Configuration = Configuration
    { cfgJSON  :: Bool
    , cfgOut   :: Handle
    , cfgPatch :: Handle
    , cfgFrom  :: Handle
    }

optionParser :: Parser Options
optionParser = Options
    <$> switch
        (  long "json"
        <> short 'j'
        <> help "Patch is in JSON format."
        )
    <*> option fileP
        (  long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Destination for patched JSON."
        <> value Nothing
        )
    <*> argument fileP
        (  metavar "PATCH"
        <> help "Patch to apply."
        )
    <*> argument fileP
        (  metavar "FROM"
        <> help "JSON file to patch."
        )
  where
    fileP = do
        s <- readerAsk
        return $ case s of
            "-" -> Nothing
            _ -> Just s

jsonRead :: Handle -> IO Value
jsonRead fp = do
    s <- BS.hGetContents fp
    case decode (BSL.fromStrict s)of
        Nothing -> error "Could not parse as JSON"
        Just v -> return v

run :: Options -> IO ()
run opt = bracket (load opt) close process
  where
    openr :: Maybe FilePath -> IO Handle
    openr Nothing = return stdin
    openr (Just p) = openFile p ReadMode

    openw :: Maybe FilePath -> IO Handle
    openw Nothing = return stdout
    openw (Just p) = openFile p WriteMode

    load :: Options -> IO Configuration
    load Options{..} =
        Configuration
            <$> pure  optionJSON
            <*> openw optionOut
            <*> openr optionPatch
            <*> openr optionFrom

    close :: Configuration -> IO ()
    close Configuration{..} = do
        hClose cfgPatch
        hClose cfgFrom
        hClose cfgOut

process :: Configuration -> IO ()
process Configuration{..} = do
    json_patch <- if cfgJSON
        then jsonRead cfgPatch
        else error "Patch must be in JSON format. Sorry :-("
    json_from <- jsonRead cfgFrom
    case fromJSON json_patch of
        Error e -> error e
        Success p -> BS.hPutStrLn cfgOut . BSL.toStrict . encode
                $ patch p json_from

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> optionParser)
     (  fullDesc
     <> progDesc "Generate a patch between two JSON documents.")
