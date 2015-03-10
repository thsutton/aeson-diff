
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Description: Diff and patch operations for JSON.
module Data.Aeson.Diff (
    Patch(..),
    patchOperations,
    Path,
    Key(..),
    Operation(..),
    -- * Functions
    diff,
    patch,
    formatPatch,
    parsePatch,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V


-- | Describes the changes between two JSON documents.
data Patch = Patch
  { patchOperations :: [Operation] }
  deriving (Show, Eq)

instance Monoid Patch where
    mempty = Patch []
    mappend (Patch p1) (Patch p2) = Patch $ p1 <> p2

instance ToJSON Patch where
    toJSON (Patch ops) = object [ "patch" .= ops ]

instance FromJSON Patch where
    parseJSON (Object v) = Patch <$> v .: "patch"
    parseJSON _ = fail "Patch must be a JSON object."

instance ToJSON Operation where
    toJSON (Ins p v) = object
        [ ("change", "insert")
        , "path"  .= p
        , "value" .= v
        ]
    toJSON (Del p v) = object
        [ ("change", "delete")
        , "path" .= p
        , "value" .= v
        ]

instance FromJSON Operation where
    parseJSON (Object v)
        =  (fixed v "change" (String "insert") *>
            (Ins <$> v .: "path" <*> v .: "value"))
        <> (fixed v "change" (String "delete") *>
            (Del <$> v .: "path" <*> v .: "value"))
      where
        fixed o n val = do
            v' <- o .: n
            unless (v' == val) . fail . T.unpack $ "Cannot find " <> n <> "."
            return v'
    parseJSON _ = fail "Operation must be a JSON object."

-- | Descripts an atomic change to a JSON document.
data Operation
    = Ins { changePath :: Path, changeValue :: Value }
    -- ^ Insert a value at a location.
    | Del { changePath :: Path, oldValue :: Value }
    -- ^ Delete the value at a location.
  deriving (Eq, Show)

-- | A path through a JSON document is a possibly empty sequence of 'Key's.
type Path = [Key]

-- | Traverse a single layer of a JSON document.
data Key
    = OKey Text
    | AKey Int
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

-- * Atomic patches

-- | Construct a patch with a single 'Ins' operation.
ins :: Path -> Value -> Patch
ins p v = Patch [Ins p v]

-- | Construct a patch with a single 'Del' operation.
del :: Path -> Value -> Patch
del p v = Patch [Del p v]

-- | Construct a patch which changes a single value.
ch :: Path -> Value -> Value -> Patch
ch p v1 v2 = del p v1 <> ins p v2

-- | Compare two JSON documents and generate a patch describing the differences.
diff
    :: Value
    -> Value
    -> Patch
diff = worker []
  where
    check :: Monoid m => Bool -> m -> m
    check b v = if b then mempty else v

    worker :: Path -> Value -> Value -> Patch
    worker p v1 v2 = case (v1, v2) of
        --        -- For atomic values of the same type, emit changes if they differ.
        (Null,      Null)      -> mempty
        (Bool b1,   Bool b2)   -> check (b1 == b2) $ ch p v1 v2
        (Number n1, Number n2) -> check (n1 == n2) $ ch p v1 v2
        (String s1, String s2) -> check (s1 == s2) $ ch p v1 v2

        -- For structured values of the same type, walk them.
        (Array a1,  Array a2)  -> check (a1 == a2) $ workArray p a1 a2
        (Object o1, Object o2) -> check (o1 == o2) $ workObject p o1 o2

        -- For values of different types, delete v1 and insert v2.
        _                      -> del p v1 <> ins p v2

    -- Walk the keys in two objects, producing a 'Patch'.
    workObject :: Path -> Object -> Object -> Patch
    workObject p o1 o2 =
        let k1 = HM.keys o1
            k2 = HM.keys o2
            dk = filter (not . (`elem` k2)) k1
            ik = filter (not . (`elem` k1)) k2
            ck = filter (`elem` k2) k1
            ds = Patch $ fmap (\k -> Del (p <> [OKey k]) . fromJust $ HM.lookup k o1) dk
            is = Patch $ fmap (\k -> Ins (p <> [OKey k]) . fromJust $ HM.lookup k o2) ik
            cs = mconcat $ fmap (\k -> worker (p <> [OKey k]) (fromJust $ HM.lookup k o1) (fromJust $ HM.lookup k o2)) ck
        in ds <> is <> cs

    -- Walk the indexes in two arrays, producing a 'Patch'.
    workArray :: Path -> Array -> Array -> Patch
    workArray _p _ _ = mempty

-- | Apply a patch to a JSON document.
patch
    :: Patch
    -> Value
    -> Value
patch (Patch []) val = val
patch (Patch ops) val= foldl work val ops
  where
    work :: Value -> Operation -> Value
    work v (Del _v _o) = v
    work _ (Ins [] v') = v'
    work v (Ins _p  _v') = v

-- | Format a 'Patch'.
formatPatch :: Patch -> Text
formatPatch (Patch ops) = T.unlines
    $ fmap formatOp ops
  where
    formatKey (OKey t) = "." <> t
    formatKey (AKey i) = "[" <> (T.pack . show $ i) <> "]"
    formatPath :: [Key] -> Text
    formatPath p = "@" <> (T.concat . fmap formatKey $ p)
    formatOp :: Operation -> Text
    formatValue :: Value -> Text
    formatValue v = case v of
        String t -> t
        Number s -> T.pack . show $ s
        Bool b -> T.pack . show $ b
        Null -> "Null"
        _ -> ":-("
    formatOp (Ins k v) = formatPath k <> "\n" <> "+" <> formatValue v
    formatOp (Del k v) = formatPath k <> "\n" <> "-" <> formatValue v

-- | Parse a 'Patch'.
parsePatch :: Text -> Either Text Patch
parsePatch _t = throwError "Cannot parse"
