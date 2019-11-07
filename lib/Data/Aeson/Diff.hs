{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Description: Extract and apply patches on JSON documents.
--
-- This module implements data types and operations to represent the
-- differences between JSON documents (i.e. a patch), to compare JSON documents
-- and extract such a patch, and to apply such a patch to a JSON document.
module Data.Aeson.Diff (
    -- * Patches
    Patch(..),
    Pointer,
    Key(..),
    Operation(..),
    Config(..),
    -- * Functions
    diff,
    diff',
    patch,
    applyOperation,
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Aeson
import           Data.Aeson.Types           (modifyFailure, typeMismatch)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Foldable              (foldlM)
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (groupBy, intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Data.Vector.Distance

import Data.Aeson.Patch
import Data.Aeson.Pointer

-- * Configuration

-- | Configuration for the diff algorithm.
newtype Config = Config
  { configTstBeforeRem :: Bool
  }

defaultConfig :: Config
defaultConfig = Config False

-- * Costs

-- | Calculate the cost of an operation.
operationCost :: Operation -> Int
operationCost op =
    case op of
      Add{} -> valueSize (changeValue op)
      Rem{} -> 1
      Rep{} -> valueSize (changeValue op)
      Mov{} -> 1
      Cpy{} -> 1
      Tst{} -> valueSize (changeValue op)

-- | Estimate the size of a JSON 'Value'.
valueSize :: Value -> Int
valueSize val = case val of
    Object o -> sum . fmap valueSize . HM.elems $ o
    Array  a -> V.sum $ V.map valueSize a
    _        -> 1

-- * Atomic patches

-- | Construct a patch with a single 'Add' operation.
ins :: Config -> Pointer -> Value -> [Operation]
ins cfg p v = [Add p v]

-- | Construct a patch with a single 'Rem' operation.
del :: Config -> Pointer -> Value -> [Operation]
del Config{..} p v =
  if configTstBeforeRem
  then [Tst p v, Rem p (Just v)]
  else [Rem p (Just v)]

-- | Construct a patch which changes 'Rep' operation.
rep :: Config -> Pointer -> Value -> [Operation]
rep Config{..} p v = [Rep p v]

-- * Diff

-- | Compare two JSON documents and generate a patch describing the differences.
--
-- Uses the 'defaultConfig'.
diff
  :: Value
  -> Value
  -> Patch
diff = diff' defaultConfig

-- | Compare two JSON documents and generate a patch describing the differences.
diff'
    :: Config
    -> Value
    -> Value
    -> Patch
diff' cfg@Config{..} v v' = Patch (worker mempty v v')
  where
    check :: Monoid m => Bool -> m -> m
    check b v = if b then mempty else v

    worker :: Pointer -> Value -> Value -> [Operation]
    worker p v1 v2 = case (v1, v2) of
        -- For atomic values of the same type, emit changes iff they differ.
        (Null,      Null)      -> mempty
        (Bool b1,   Bool b2)   -> check (b1 == b2) $ rep cfg p v2
        (Number n1, Number n2) -> check (n1 == n2) $ rep cfg p v2
        (String s1, String s2) -> check (s1 == s2) $ rep cfg p v2

        -- For structured values of the same type, walk them.
        (Array a1,  Array a2)  -> check (a1 == a2) $ workArray  p a1 a2
        (Object o1, Object o2) -> check (o1 == o2) $ workObject p o1 o2

        -- For values of different types, replace v1 with v2.
        _                      -> rep cfg p v2

    -- Walk the keys in two objects, producing a 'Patch'.
    workObject :: Pointer -> Object -> Object -> [Operation]
    workObject path o1 o2 =
        let k1 = HM.keys o1
            k2 = HM.keys o2
            -- Deletions
            del_keys :: [Text]
            del_keys = filter (not . (`elem` k2)) k1
            deletions :: [Operation]
            deletions = concatMap
                (\k -> del cfg (Pointer [OKey k]) (fromJust $ HM.lookup k o1))
                del_keys
            -- Insertions
            ins_keys = filter (not . (`elem` k1)) k2
            insertions :: [Operation]
            insertions = concatMap
                (\k -> ins cfg (Pointer [OKey k]) (fromJust $ HM.lookup k o2))
                ins_keys
            -- Changes
            chg_keys = filter (`elem` k2) k1
            changes :: [Operation]
            changes = concatMap
                (\k -> worker (Pointer [OKey k])
                    (fromJust $ HM.lookup k o1)
                    (fromJust $ HM.lookup k o2))
                chg_keys
        in modifyPointer (path <>) <$> (deletions <> insertions <> changes)

    -- Use an adaption of the Wagner-Fischer algorithm to find the shortest
    -- sequence of changes between two JSON arrays.
    workArray :: Pointer -> Array -> Array -> [Operation]
    workArray path ss tt = fmap (modifyPointer (path <>)) . snd . fmap concat $ leastChanges params ss tt
      where
        params :: Params Value [Operation] (Sum Int)
        params = Params{..}
        equivalent = (==)
        delete i = del cfg (Pointer [AKey i])
        insert i = ins cfg (Pointer [AKey i])
        substitute i = worker (Pointer [AKey i])
        cost = Sum . sum . fmap operationCost
        -- Position is advanced by grouping operations with same "head" index:
        -- + groups of many operations advance one
        -- + singletons with |pointer|>1 advance one
        -- + other singletons advance according to 'pos'
        positionOffset = sum . fmap adv . groupBy related
        related :: Operation -> Operation -> Bool
        related o1 o2 =
            let p1 = pointerPath (changePointer o1)
                p2 = pointerPath (changePointer o2)
            in case (p1, p2) of
                 ([i1], [i2]) -> False
                 (i1:_, i2:_) | i1 == i2  -> True
                              | otherwise -> False
        -- A group of operations has a peculiar (i.e. given by 'pos') advance
        -- when it's a single op and |changePointer| = 1; otherwise it's a
        -- bunch of changes inside the head key.
        adv :: [Operation] -> Int
        adv [op]
            | (length . pointerPath . changePointer $ op) == 1 = pos op
        adv _    = 1
        pos :: Operation -> Int
        pos Rem{changePointer=Pointer path}
            | length path == 1 = 0
            | otherwise        = 0
        pos Add{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Rep{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Cpy{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Mov{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Tst{changePointer=Pointer path} = 0

-- * Patching

-- | Apply a patch to a JSON document.
patch
    :: Patch
    -> Value
    -> Result Value
patch (Patch []) val  = return val
patch (Patch ops) val = foldlM (flip applyOperation) val ops

-- | Apply an 'Operation' to a 'Value'.
applyOperation
    :: Operation
    -> Value
    -> Result Value
applyOperation op json = case op of
    Add path v'   -> applyAdd path v' json
    Rem path _    -> applyRem path    json
    Rep path v'   -> applyRep path v' json
    Tst path v    -> applyTst path v  json
    Cpy path from -> applyCpy path from json
    Mov path from -> do
        v' <- get from json
        applyRem from json >>= applyAdd path v'

-- | Apply an 'Add' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.1
--
-- - An empty 'Path' replaces the document.
-- - A single 'OKey' inserts or replaces the corresponding member in an object.
-- - A single 'AKey' inserts at the corresponding location.
-- - Longer 'Paths' traverse if they can and fail otherwise.
applyAdd :: Pointer -> Value -> Value -> Result Value
applyAdd pointer = go pointer
  where
    go (Pointer []) val _ =
        return val
    go (Pointer [AKey i]) v' (Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn _ = return (Just v')
        in return (Array $ vInsert i v' v)
    go (Pointer (AKey i : path)) v' (Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "insert" "array" i pointer
            fn (Just d) = Just <$> go (Pointer path) v' d
        in Array <$> vModify i fn v
    go (Pointer [OKey n]) v' (Object m) =
        return . Object $ HM.insert n v' m
    go (Pointer (OKey n : path)) v' (Object o) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "insert" "object" n pointer
            fn (Just d) = Just <$> go (Pointer path) v' d
        in Object <$> hmModify n fn o
    go (Pointer (OKey n : path)) v' array@(Array v)
        | n == "-" = go (Pointer (AKey (V.length v) : path)) v' array
    go path _ v = pointerFailure path v

-- | Apply a 'Rem' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.2
--
-- - The target location MUST exist.
applyRem :: Pointer -> Value -> Result Value
applyRem from@(Pointer path) = go path
  where
    go [] _ = return Null
    go [AKey i] d@(Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "delete" "array" i from
            fn (Just v) = return Nothing
        in Array <$> vModify i fn v
    go (AKey i : path) (Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "traverse" "array" i from
            fn (Just o) = Just <$> go path o
        in Array <$> vModify i fn v
    go [OKey n] (Object m) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "delete" "object" n from
            fn (Just _) = return Nothing
        in Object <$> hmModify n fn m
    go (OKey n : path) (Object m) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "traverse" "object" n from
            fn (Just o) = Just <$> go path o
        in Object <$> hmModify n fn m
    -- Dodgy hack for "-" key which means "the end of the array".
    go (OKey n : path) array@(Array v)
        | n == "-" = go (AKey (V.length v) : path) array
    -- Type mismatch: clearly the thing we're deleting isn't here.
    go path value = pointerFailure from value

-- | Apply a 'Rep' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.3
--
-- - Functionally identical to a 'Rem' followed by an 'Add'.
applyRep :: Pointer -> Value -> Value -> Result Value
applyRep from v doc = applyRem from doc >>= applyAdd from v

-- | Apply a 'Mov' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.4
applyMov :: Pointer -> Pointer -> Value -> Result Value
applyMov path from doc = do
  v <- get from doc
  applyRem from doc >>= applyAdd path v

-- | Apply a 'Cpy' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.5
--
-- - The location must exist.
-- - Identical to an add with the appropriate value.
applyCpy :: Pointer -> Pointer -> Value -> Result Value
applyCpy path from doc = do
  v <- get from doc
  applyAdd path v doc

-- | Apply a 'Tst' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.6
--
-- - The location must exist.
-- - The value must be equal to the supplied value.
applyTst :: Pointer -> Value -> Value -> Result Value
applyTst path v doc = do
    v' <- get path doc
    unless (v == v') (Error . T.unpack $ "Element at \"" <> formatPointer path <> "\" fails test.")
    return doc

-- * Utilities

-- $ These are some utility functions used in the functions defined
-- above. Mostly they just fill gaps in the APIs of the "Data.Vector"
-- and "Data.HashMap.Strict" modules.

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
vModify
    :: Int
    -> (Maybe a -> Result (Maybe a))
    -> Vector a
    -> Result (Vector a)
vModify i f v =
    let a = v V.!? i
        a' = f a
    in case (a, a') of
        (Nothing, Success Nothing ) -> return v
        (Just _ , Success Nothing ) -> return (vDelete i v)
        (Nothing, Success (Just n)) -> return (vInsert i n v)
        (Just _ , Success (Just n)) -> return (V.update v (V.singleton (i, n)))
        (_      , Error   e       ) -> Error e

-- | Modify the value associated with a key in a 'HashMap'.
--
-- The function is passed the value defined for @k@, or 'Nothing'. If the
-- function returns 'Nothing', the key and value are deleted from the map;
-- otherwise the value replaces the existing value in the returned map.
hmModify
    :: (Eq k, Hashable k)
    => k
    -> (Maybe v -> Result (Maybe v))
    -> HashMap k v
    -> Result (HashMap k v)
hmModify k f m = case f (HM.lookup k m) of
    Error e          -> Error e
    Success Nothing  -> return $ HM.delete k m
    Success (Just v) -> return $ HM.insert k v m

-- | Report an error about being able to use a pointer key.
cannot
    :: (Show ix)
    => String -- ^ Use to be made "delete", "traverse", etc.
    -> String -- ^ Type "array" "object"
    -> ix
    -> Pointer
    -> Result a
cannot op ty ix p =
    Error ("Cannot " <> op <> " missing " <> ty <> " member at index "
          <> show ix <> " in pointer \"" <> T.unpack (formatPointer p) <> "\".")
