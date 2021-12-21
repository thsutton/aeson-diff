{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- | Description: Represent RFC 6902 patches.
module Data.Aeson.Patch (
  Patch(..),
  Operation(..),
  -- * Modification
  modifyPointer,
  modifyPointers,
  -- * Predicates
  isAdd,
  isRem,
  isRep,
  isMov,
  isCpy,
  isTst,
) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid
import           Data.Semigroup             (Semigroup)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)

import Data.Aeson.Pointer

-- * Patches

-- | Describes the changes between two JSON documents.
newtype Patch = Patch
    { patchOperations :: [Operation] }
  deriving (Eq, Show, Semigroup, Monoid, Generic)

instance ToJSON Patch where
    toJSON (Patch ops) = toJSON ops

instance FromJSON Patch where
    parseJSON = modifyFailure ("Could not parse patch: " <> ) . parsePatch
      where
        parsePatch (Array v) = Patch <$> mapM parseJSON (V.toList v)
        parsePatch v = typeMismatch "Array" v

-- | Modify the pointers in the 'Operation's of a 'Patch'.
--
-- See 'modifyPointer' for details.
modifyPointers :: (Pointer -> Pointer) -> Patch -> Patch
modifyPointers f (Patch ops) = Patch (map (modifyPointer f) ops)

-- * Operations

-- | An 'Operation' describes the operations which can appear as part of a JSON
-- Patch.
--
-- See RFC 6902 Section 4 <http://tools.ietf.org/html/rfc6902#section-4>.
data Operation
    = Add { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.1
    | Cpy { changePointer :: Pointer, fromPointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.5
    | Mov { changePointer :: Pointer, fromPointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.4
    | Rem { changePointer :: Pointer, deletedValue :: Maybe Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.2
    | Rep { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.3
    | Tst { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.6
  deriving (Eq, Show, Generic)

instance ToJSON Operation where
    toJSON (Add p v) = object
        [ ("op", "add")
        , "path"  .= p
        , "value" .= v
        ]
    toJSON (Cpy p f) = object
        [ ("op", "copy")
        , "path" .= p
        , "from" .= f
        ]
    toJSON (Mov p f) = object
        [ ("op", "move")
        , "path" .= p
        , "from" .= f
        ]
    toJSON (Rem p _) = object
        [ ("op", "remove")
        , "path" .= p
        ]
    toJSON (Rep p v) = object
        [ ("op", "replace")
        , "path"  .= p
        , "value" .= v
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
            <|> (op v "copy"    *> (Cpy <$> v .: "path" <*> v .: "from"))
            <|> (op v "move"    *> (Mov <$> v .: "path" <*> v .: "from"))
            <|> (op v "remove"  *> (Rem <$> v .: "path" <*> (pure Nothing)))
            <|> (op v "replace" *> (Rep <$> v .: "path" <*> v .: "value"))
            <|> (op v "test"    *> (Tst <$> v .: "path" <*> v .: "value"))
            <|> fail ("Expected a JSON patch operation, encountered: " <> BS.unpack (encode o))
        parse v = typeMismatch "Operation" v
        op v n = fixed v "op" (String n)
        fixed o n val = do
            v' <- o .: n
            if v' == val
              then return v'
              else mzero
        fixed' o n val = (o .: n) >>= \v -> guard (v == n)

-- | Modify the 'Pointer's in an 'Operation'.
--
-- If the operation contains multiple pointers (i.e. a 'Mov' or 'Cpy')
-- then both will be modified.
modifyPointer :: (Pointer -> Pointer) -> Operation -> Operation
modifyPointer f op =
  case op of
    Add{..} -> op{ changePointer = f changePointer }
    Cpy{..} -> op{ changePointer = f changePointer, fromPointer = f fromPointer }
    Mov{..} -> op{ changePointer = f changePointer, fromPointer = f fromPointer }
    Rem{..} -> op{ changePointer = f changePointer }
    Rep{..} -> op{ changePointer = f changePointer }
    Tst{..} -> op{ changePointer = f changePointer }

isAdd :: Operation -> Bool
isAdd Add{} = True
isAdd _ = False

isCpy :: Operation -> Bool
isCpy Cpy{} = True
isCpy _ = False

isMov :: Operation -> Bool
isMov Mov{} = True
isMov _ = False

isRem :: Operation -> Bool
isRem Rem{} = True
isRem _ = False

isRep :: Operation -> Bool
isRep Rep{} = True
isRep _ = False

isTst :: Operation -> Bool
isTst Tst{} = True
isTst _ = False
