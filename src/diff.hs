{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Diff
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Monoid
import qualified Data.Text.IO              as T
import           Options.Applicative
import           Options.Applicative.Types
import           System.IO

type File = Maybe FilePath

-- | Command-line options.
data Options = Options
    { optionTst  :: Bool
    , optionOut  :: File
    , optionFrom :: File
    , optionTo   :: File
    }

data Configuration = Configuration
    { cfgTst  :: Bool
    , cfgOut  :: Handle
    , cfgFrom :: Handle
    , cfgTo   :: Handle
    }

optionParser :: Parser Options
optionParser = Options
    <$> switch
        (  long "test-before-remove"
        <> short 'T'
        <> help "Include a test before each remove."
        )
    <*> option fileP
        (  long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Write patch to file OUTPUT."
        <> value Nothing
        )
    <*> argument fileP
        (  metavar "FROM"
        )
    <*> argument fileP
        (  metavar "TO"
        )
  where
    fileP = do
        s <- readerAsk
        return $ case s of
            "-" -> Nothing
            _ -> Just s

jsonFile :: Handle -> IO Value
jsonFile fp = do
    s <- BS.hGetContents fp
    case decode (BSL.fromStrict s) of
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
            <$> pure  optionTst
            <*> openw optionOut
            <*> openr optionFrom
            <*> openr optionTo

    close :: Configuration -> IO ()
    close Configuration{..} = do
        hClose cfgOut
        hClose cfgFrom
        hClose cfgTo

process :: Configuration -> IO ()
process Configuration{..} = do
    json_from <- jsonFile cfgFrom
    json_to <- jsonFile cfgTo
    let c = Config cfgTst
    let p = diff' c json_from json_to
    BS.hPutStrLn cfgOut $ BSL.toStrict (encode p)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> optionParser)
     (  fullDesc
     <> progDesc "Generate a patch between two JSON documents.")
