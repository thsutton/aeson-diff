{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Description: JSON Pointers as described in RFC 6901.

module Data.Aeson.Pointer (
  Pointer(..),
  Key(..),
  pointerFailure,
  Path,
) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isNumber)
import           Data.Monoid
import           Data.Scientific
import           Data.Text                  (Text)
import qualified Data.Text                  as T

-- * Patch components

-- | Traverse a single layer of a JSON document.
data Key
    = OKey Text -- ^ Traverse a 'Value' with an 'Object' constructor.
    | AKey Int  -- ^ Traverse a 'Value' with an 'Array' constructor.
  deriving (Eq, Ord, Show)

instance ToJSON Key where
    toJSON (OKey t) = String t
    toJSON (AKey a) = Number . fromInteger . toInteger $ a

instance FromJSON Key where
    parseJSON (String t) = return $ OKey t
    parseJSON (Number n) =
        case toBoundedInteger n of
            Nothing -> fail "A numeric key must be a positive whole number."
            Just n' -> return $ AKey n'
    parseJSON _ = fail "A key element must be a number or a string."

-- * Pointers

type Path = [Key]

-- | Pointer to a location in a JSON document.
--
-- Defined in RFC 6901 <http://tools.ietf.org/html/rfc6901>
newtype Pointer = Pointer { pointerPath :: Path }
  deriving (Eq, Show, Monoid)

pointerFailure :: Path -> Value -> Result a
pointerFailure [] value = Error ("UNPOSSIBLE!" <> show value)
pointerFailure path@(key:rest) value =
    fail . BS.unpack $ "Cannot follow pointer " <> pt <> ". Expected " <> ty <> " but got " <> doc
  where
    doc = encode value
    pt = encode (Pointer path)
    ty = case key of
           (AKey _) -> "array"
           (OKey _) -> "object"

instance ToJSON Pointer where
    toJSON (Pointer path) = String ("/" <> T.intercalate "/" (fmap fmt path))
        where
          esc :: Char -> Text
          esc '~' = "~0"
          esc '/' = "~1"
          esc c = T.singleton c
          fmt :: Key -> Text
          fmt (OKey t) = T.concatMap esc t
          fmt (AKey i) = T.pack (show i)

instance FromJSON Pointer where
    parseJSON = modifyFailure ("Could not parse JSON pointer: " <>) . parse
      where
        parse (String t) = Pointer <$> mapM key (drop 1 $ T.splitOn "/" t)
        parse _ = fail "A JSON pointer must be a string."
        step t
              | "0" `T.isPrefixOf` t = T.cons '~' (T.tail t)
              | "1" `T.isPrefixOf` t = T.cons '/' (T.tail t)
              | otherwise = T.cons '~' t
        unesc :: Text -> Text
        unesc t =
            let l = T.split (== '~') t
            in T.concat $ take 1 l <> fmap step (tail l)
        key t
            | T.null t         = fail "JSON components must not be empty."
            | T.all isNumber t = return (AKey (read $ T.unpack t))
            | otherwise        = return $ OKey (unesc t)
