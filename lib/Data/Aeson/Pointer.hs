{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Description: JSON Pointers as described in RFC 6901.
module Data.Aeson.Pointer (
  Pointer(..),
  Key(..),
  pointerFailure,
  Path,
  formatPointer,
  get,
) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isNumber)
import qualified Data.HashMap.Strict        as HM
import           Data.Monoid
import           Data.Scientific
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V

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

formatKey :: Key -> Text
formatKey (AKey i) = T.pack (show i)
formatKey (OKey t) = T.concatMap esc t
  where
    esc :: Char -> Text
    esc '~' = "~0"
    esc '/' = "~1"
    esc c = T.singleton c

-- * Pointers

-- | A sequence of 'Key's forms a path through a JSON document.
type Path = [Key]

-- | Pointer to a location in a JSON document.
--
-- Defined in RFC 6901 <http://tools.ietf.org/html/rfc6901>
newtype Pointer = Pointer { pointerPath :: Path }
  deriving (Eq, Show, Monoid)

-- | Format a 'Pointer' as described in RFC 6901.
--
-- >>> formatPointer (Pointer [])
-- ""
-- >>> formatPointer (Pointer [OKey ""])
-- "/"
-- >>> formatPointer (Pointer [OKey " "])
-- "/ "
-- >>> formatPointer (Pointer [OKey "foo"])
-- "/foo"
-- >>> formatPointer (Pointer [OKey "foo", AKey 0])
-- "/foo/0"
-- >>> formatPointer (Pointer [OKey "a/b"])
-- "/a~1b"
-- >>> formatPointer (Pointer [OKey "c%d"])
-- "/c%d"
-- >>> formatPointer (Pointer [OKey "e^f"])
-- "/e^f"
-- >>> formatPointer (Pointer [OKey "g|h"])
-- "/g|h"
-- >>> formatPointer (Pointer [OKey "i\\j"])
-- "/i\\j"
-- >>> formatPointer (Pointer [OKey "k\"l"])
-- "/k\"l"
-- >>> formatPointer (Pointer [OKey "m~n"])
-- "/m~0n"
formatPointer :: Pointer -> Text
formatPointer (Pointer []) = ""
formatPointer (Pointer path) = "/" <> T.intercalate "/" (formatKey <$> path)

-- | Report an error following a pointer.
pointerFailure :: Path -> Value -> Result a
pointerFailure [] value = Error "Cannot follow empty pointer. This is impossible."
pointerFailure path@(key:rest) value =
    Error . BS.unpack $ "Cannot follow pointer " <> pt <> ". Expected " <> ty <> " but got " <> doc
  where
    doc = encode value
    pt = encode (Pointer path)
    ty = case key of
           (AKey _) -> "array"
           (OKey _) -> "object"

instance ToJSON Pointer where
    toJSON pointer =
        String (formatPointer pointer)

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

-- | Get the value at a 'Path'.
get :: Pointer -> Value -> Result Value
get (Pointer p) = get' p
  where
    get' [] v = return v
    get' (AKey i : path) (Array v) =
        maybe (fail "") return (v V.!? i) >>= get' path
    get' (OKey n : path) (Object v) =
        maybe (fail "") return (HM.lookup n v) >>= get' path
    get' path value = pointerFailure path value

-- $setup
-- >>> :set -XOverloadedStrings
