{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Description: Extract and apply patches on JSON documents.
--
-- This module implements data types and operations to represent the
-- differences between JSON documents (i.e. a patch), to compare JSON documents
-- and extract such a patch, and to apply such a patch to a JSON document.

module Data.Aeson.Diff (
    -- * Patch
    Patch,
    patchOperations,
    Path,
    Key(..),
    Operation(..),
    -- * Functions
    diff,
    patch,
    formatPatch,
    parsePatch,
    applyOperation,
    -- * Utilitied
    vDelete,
    vInsert,
    vModify,
    hmModify,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Data.Aeson
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Distance

-- | Describes the changes between two JSON documents.
data Patch = Patch
  { patchOperations :: [Operation] }
  deriving (Eq)

instance Show Patch where
    show = T.unpack . formatPatch

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
    | Del { changePath :: Path, changeValue :: Value }
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

-- | Modify the 'Path' of an 'Operation'.
modifyPath :: (Path -> Path) -> Operation -> Operation
modifyPath path op = op { changePath = path (changePath op) }

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

-- * Operations

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
        -- For atomic values of the same type, emit changes iff they differ.
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
            -- Deletions
            del_keys = filter (not . (`elem` k2)) k1
            deletions = Patch $ fmap
                (\k -> Del (p <> [OKey k]) . fromJust $ HM.lookup k o1)
                del_keys
            -- Insertions
            ins_keys = filter (not . (`elem` k1)) k2
            insertions = Patch $ fmap
                (\k -> Ins (p <> [OKey k]) . fromJust $ HM.lookup k o2)
                ins_keys
            -- Changes
            chg_keys = filter (`elem` k2) k1
            changes = fmap
                (\k -> worker (p <> [OKey k])
                    (fromJust $ HM.lookup k o1)
                    (fromJust $ HM.lookup k o2))
                chg_keys
        in deletions <> insertions <> mconcat changes

    -- Use an adaption of the Wagner-Fischer algorithm to find the shortest
    -- sequence of changes between two JSON arrays.
    workArray :: Path -> Array -> Array -> Patch
    workArray path ss tt = Patch . snd . fmap concat $ leastChanges params
        (V.toList ss) (V.toList tt)
      where
        params = Params{..}
        equivalent = (==)
        delete i v = [Del (path ++ [AKey i]) v]
        insert i v = [Ins (path ++ [AKey i]) v]
        substitute i v v' =
            let p = path ++ [AKey i]
                Patch ops = diff v v'
            in fmap (modifyPath (p ++)) ops
        cost = sum . map (valueSize . changeValue)
        positionOffset = sum . map pos
        pos Del{} = 0
        pos Ins{} = 1

-- | Apply a patch to a JSON document.
patch
    :: Patch
    -> Value
    -> Value
patch (Patch []) val = val
patch (Patch ops) val = foldl (flip applyOperation) val ops

-- | Apply an 'Operation' to a 'Value'.
applyOperation
    :: Operation
    -> Value
    -> Value
applyOperation op j = case op of
    Ins path v' -> insert path v' j
    Del path v' -> delete path v' j
  where
    insert :: Path -> Value -> Value -> Value
    insert [] v' _ = v'
    -- Apply a local change.
    insert [AKey i] v' (Array  v) = Array  $ vInsert   i v' v
    insert [OKey n] v' (Object m) = Object $ HM.insert n v' m
    -- Traverse for deeper changes.
    insert (AKey i : path) v' (Array v) = Array $ vModify i
        (Just . insert path v' . fromMaybe (Array mempty)) v
    insert (OKey n : path) v' (Object m) = Object $ hmModify n
        (Just . insert path v' . fromMaybe (Object mempty)) m
    -- Type mismatch; let's throw away the thing we weren't expecting!
    --
    -- TODO: I have no idea what to do here.
    insert (AKey _ : path) v' v = Array $ V.singleton (insert path v' v)
    insert (OKey n : path) v' v = Object $ HM.singleton n (insert path v' v)

    delete :: Path -> Value -> Value -> Value
    -- Apply a local change.
    --
    -- TODO Should check whether the deleted item is something like @v'@.
    delete [AKey i] _v' (Array  v) = Array  $ vDelete   i v
    delete [OKey n] _v' (Object m) = Object $ HM.delete n m
    -- Traverse for deeper changes.
    delete (AKey i : rest) v' (Array v) = Array $ vModify i
        (fmap (delete rest v')) v
    delete (OKey n : rest) v' (Object m) = Object $ hmModify n
        (fmap (delete rest v')) m
    -- Type mismatch: clearly the thing we're deleting isn't here.
    --
    -- TODO: If v ~ v', return Null or something, just for fun.
    delete [] _v' _v = Null
    delete _  _v'  v = v

-- * Formatting patches

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

-- * Utilities
--
-- $ These are some utility functions used in the functions defined above. Mostly
-- they just fill gaps in the APIs of the "Data.Vector" and "Data.HashMap.Strict"
-- modules.

-- | Estimate the size of a JSON 'Value'.
--
-- This is used in the diff cost metric function.
valueSize :: Value -> Int
valueSize val = case val of
    Object o -> sum . map valueSize . HM.elems $ o
    Array  a -> V.sum $ V.map valueSize a
    _        -> 1

-- | Delete an element in a vector.
vDelete :: Int -> Vector a -> Vector a
vDelete i v =
    let l = V.length v
    in V.slice 0 i v <> V.slice (i + 1) (l - i - 1) v

-- | Insert an element into a vector.
vInsert :: Int -> a -> Vector a -> Vector a
vInsert i a v
    | i <= 0          = V.cons a v
    | V.length v <= i = V.snoc v a
    | otherwise       = V.slice 0 i v
                      <> V.singleton a
                      <> V.slice i (V.length v - i) v

-- | Modify the element at an index in a 'Vector'.
--
-- The function is passed the value at index @i@, or 'Nothing' if there is no
-- such element. The function should return 'Nothing' if it wants to have no
-- value corresponding to the index, or 'Just' if it wants a value.
--
-- Depending on the vector and the function, we will either:
--
-- - leave the vector unchanged;
-- - delete an existing element;
-- - insert a new element; or
-- - replace an existing element.
vModify :: Int -> (Maybe a -> Maybe a) -> Vector a -> Vector a
vModify i f v =
    let a = v V.!? i
        a' = f a
    in case (a, a') of
        (Nothing, Nothing) -> v
        (Just _,  Nothing) -> vDelete i v
        (Nothing, Just n ) -> vInsert i n v
        (Just _,  Just n ) -> V.update v (V.singleton (i, n))

-- | Modify the value associated with a key in a 'HashMap'.
--
-- The function is passed the value defined for @k@, or 'Nothing'. If the
-- function returns 'Nothing', the key and value are deleted from the map;
-- otherwise the value replaces the existing value in the returned map.
hmModify
    :: (Eq k, Hashable k)
    => k
    -> (Maybe v -> Maybe v)
    -> HashMap k v
    -> HashMap k v
hmModify k f m = case f (HM.lookup k m) of
    Nothing -> HM.delete k m
    Just v  -> HM.insert k v m
