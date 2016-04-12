{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Description: Represent RFC 6902 patches.
module Data.Aeson.Patch (
  Patch(..),
  Operation(..),
) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid
import           Data.Vector                (Vector)
import qualified Data.Vector                as V

import Data.Aeson.Pointer

-- * Patches

-- | Describes the changes between two JSON documents.
newtype Patch = Patch
    { patchOperations :: [Operation] }
  deriving (Eq, Show, Monoid)

instance ToJSON Patch where
    toJSON (Patch ops) = toJSON ops

instance FromJSON Patch where
    parseJSON = modifyFailure ("Could not parse patch: " <> ) . parsePatch
      where
        parsePatch (Array v) = Patch <$> mapM parseJSON (V.toList v)
        parsePatch v = typeMismatch "Array" v

-- * Operations

-- | An 'Operation' describes the operations which can appear as part of a JSON
-- Patch.
--
-- See RFC 6902 Section 4 <http://tools.ietf.org/html/rfc6902#section-4>.
data Operation
    = Add { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.1
    | Rem { changePointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.2
    | Rep { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.3
    | Mov { changePointer :: Pointer, fromPointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.4
    | Cpy { changePointer :: Pointer, fromPointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.5
    | Tst { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.6
  deriving (Eq, Show)

instance ToJSON Operation where
    toJSON (Add p v) = object
        [ ("op", "add")
        , "path"  .= p
        , "value" .= v
        ]
    toJSON (Rem p) = object
        [ ("op", "remove")
        , "path" .= p
        ]
    toJSON (Rep p v) = object
        [ ("op", "replace")
        , "path"  .= p
        , "value" .= v
        ]
    toJSON (Mov p f) = object
        [ ("op", "move")
        , "path" .= p
        , "from" .= f
        ]
    toJSON (Cpy p f) = object
        [ ("op", "copy")
        , "path" .= p
        , "from" .= f
        ]
    toJSON (Tst p v) = object
        [ ("op", "test")
        , "path" .= p
        , "value" .= v
        ]

instance FromJSON Operation where
    parseJSON = parse
      where
        parse o@(Object v)
            =   (op v "add"     *> (Add <$> v .: "path" <*> v .: "value"))
            <|> (op v "replace" *> (Rep <$> v .: "path" <*> v .: "value"))
            <|> (op v "move"    *> (Mov <$> v .: "path" <*> v .: "from"))
            <|> (op v "copy"    *> (Cpy <$> v .: "path" <*> v .: "from"))
            <|> (op v "test"    *> (Tst <$> v .: "path" <*> v .: "value"))
            <|> (op v "remove"  *> (Rem <$> v .: "path"))
            <|> fail ("Expected a JSON patch operation, encountered: " <> BS.unpack (encode o))
        parse v = typeMismatch "Operation" v
        op v n = fixed v "op" (String n)
        fixed o n val = do
            v' <- o .: n
            if v' == val
              then return v'
              else mzero
        fixed' o n val = (o .: n) >>= \v -> guard (v == n)
