{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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

    -- * Functions
    diff,
    patch,
    patch',
    applyOperation,

    formatPatch,
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isNumber)
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

import Data.Vector.Distance

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
    toJSON (Patch ops) = toJSON ops

instance FromJSON Patch where
    parseJSON (Array v) = Patch <$> mapM parseJSON (V.toList v)
    parseJSON _ = fail "Patch must be a JSON object."

type Path = [Key]

-- | Pointer to a location in a JSON document.
--
-- Defined in RFC 6901 <http://tools.ietf.org/html/rfc6901>
newtype Pointer = Pointer { pointerPath :: Path }
  deriving (Eq, Show, Monoid)

pointerFailure :: Path -> Value -> Result a
pointerFailure [] value = fail $ "UNPOSSIBLE!" <> show value
pointerFailure path@(key:rest) value =
    fail . BS.unpack $ "Cannot follow pointer " <> pt <> ". Expected " <> ty <> " but got " <> doc
  where
    doc = encode value
    pt = encode (Pointer path)
    ty = case key of
           (AKey _) -> "array"
           (OKey _) -> "object"

-- | An 'Operation' describes an atomic change to a JSON document.
--
-- See RFC 6902 Section 4 <http://tools.ietf.org/html/rfc6902#section-4>.
data Operation
    = Add { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.1
    | Rem { changePointer :: Pointer, changeValue :: Value }
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
    toJSON (Rem p v) = object
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
    parseJSON o@(Object v)
        =   (op "add"     *> (Add <$> v .: "path" <*> v .: "value"))
        <|> (op "replace" *> (Rep <$> v .: "path" <*> v .: "value"))
        <|> (op "move"    *> (Mov <$> v .: "path" <*> v .: "from"))
        <|> (op "copy"    *> (Cpy <$> v .: "path" <*> v .: "from"))
        <|> (op "test"    *> (Tst <$> v .: "path" <*> v .: "value"))
        <|> (op "remove"  *> (Rem <$> v .: "path" <*> pure Null))
        <|> fail ("Expected a JSON patch operation, encountered: " <> BS.unpack (encode o))
      where
        op n = fixed v "op" (String n)
        fixed o n val = do
            v' <- o .: n
            if v' == val
              then return v'
              else mzero
    parseJSON _ = fail "Operation must be a JSON object."

operationCost :: Operation -> Int
operationCost op =
    case op of
      Add{} -> valueSize (changeValue op)
      Rem{} -> valueSize (changeValue op)
      Rep{} -> valueSize (changeValue op)
      Mov{} -> 1
      Cpy{} -> 1
      Tst{} -> valueSize (changeValue op)

-- | Modify the 'Pointer's of an 'Operation'.
--
-- This is typically used to add a prefix to the 'Pointer's in an
modifyPath :: ([Key] -> [Key]) -> Operation -> Operation
modifyPath f op = from (change op)
  where
    fn :: Pointer -> Pointer
    fn (Pointer p) = Pointer (f p)
    change op = op { changePointer = fn (changePointer op) }
    from op =
        case op of
          Mov{} -> op { fromPointer = fn (fromPointer op) }
          Cpy{} -> op { fromPointer = fn (fromPointer op) }
          _     -> op

-- | Traverse a single layer of a JSON document.
data Key
    = OKey Text -- ^ Traverse a 'Value' with an 'Object' constructor.
    | AKey Int  -- ^ Traverse a 'Value' with an 'Array' constructor.
  deriving (Eq, Ord, Show)

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
    parseJSON (String t) = Pointer <$> mapM key (drop 1 $ T.splitOn "/" t)
        where
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
    parseJSON _ = fail "A JSON pointer must be a string."

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

-- | Construct a patch with a single 'Add' operation.
ins :: Path -> Value -> Patch
ins p v = Patch [Add (Pointer p) v]

-- | Construct a patch with a single 'Rem' operation.
del :: Path -> Value -> Patch
del p v = Patch [Rem (Pointer p) v]

-- | Construct a patch which changes 'Rep' operation.
rep :: Path -> Value -> Patch
rep p v = Patch [Rep (Pointer p) v]

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
        (Bool b1,   Bool b2)   -> check (b1 == b2) $ rep p v2
        (Number n1, Number n2) -> check (n1 == n2) $ rep p v2
        (String s1, String s2) -> check (s1 == s2) $ rep p v2

        -- For structured values of the same type, walk them.
        (Array a1,  Array a2)  -> check (a1 == a2) $ workArray  p a1 a2
        (Object o1, Object o2) -> check (o1 == o2) $ workObject p o1 o2

        -- For values of different types, replace v1 with v2.
        _                      -> rep p v2

    -- Walk the keys in two objects, producing a 'Patch'.
    workObject :: Path -> Object -> Object -> Patch
    workObject path o1 o2 =
        let k1 = HM.keys o1
            k2 = HM.keys o2
            -- Deletions
            del_keys = filter (not . (`elem` k2)) k1
            deletions = Patch $ fmap
                (\k -> Rem (Pointer [OKey k]) . fromJust $ HM.lookup k o1)
                del_keys
            -- Insertions
            ins_keys = filter (not . (`elem` k1)) k2
            insertions = Patch $ fmap
                (\k -> Add (Pointer [OKey k]) . fromJust $ HM.lookup k o2)
                ins_keys
            -- Changes
            chg_keys = filter (`elem` k2) k1
            changes = fmap
                (\k -> worker [OKey k]
                    (fromJust $ HM.lookup k o1)
                    (fromJust $ HM.lookup k o2))
                chg_keys
        in Patch . fmap (modifyPath (path <>)) . patchOperations $ (deletions <> insertions <> mconcat changes)

    -- Use an adaption of the Wagner-Fischer algorithm to find the shortest
    -- sequence of changes between two JSON arrays.
    workArray :: Path -> Array -> Array -> Patch
    workArray path ss tt = Patch . fmap (modifyPath (path <>)) . snd . fmap concat $ leastChanges params ss tt
      where
        params :: Params Value [Operation] (Sum Int)
        params = Params{..}
        equivalent = (==)
        delete i v = [Rem (Pointer [AKey i]) v]
        insert i v = [Add (Pointer [AKey i]) v]
        substitute i v v' =
            let p = [AKey i]
                Patch ops = diff v v'
            in fmap (modifyPath (p <>)) ops
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

-- | Apply a patch to a JSON document.
--
-- If the patch cannot be cleanly applied an 'error' is thrown.
patch :: Patch -> Value -> Value
patch p d =
    case patch' p d of
      Error e -> error $ "Failed to apply patch: " <> e <> "\n" <> BS.unpack (encode p)
      Success v -> v

-- | Apply a patch to a JSON document.
patch'
    :: Patch
    -> Value
    -> Result Value
patch' (Patch []) val = return val
patch' (Patch ops) val = foldlM (flip applyOperation) val ops

-- | Apply an 'Operation' to a 'Value'.
applyOperation
    :: Operation
    -> Value
    -> Result Value
applyOperation op j = case op of
    Add (Pointer path) v' -> applyAdd path v' j
    Rem (Pointer path) _  -> applyRem path    j
    Rep (Pointer path) v' -> applyRep path v' j
    Mov (Pointer path) (Pointer from) -> do
        v' <- get from j
        applyRem from j >>= applyAdd path v'
    Cpy (Pointer path) (Pointer from) -> applyCpy path from j
    Tst (Pointer path) v  -> applyTst path v j

-- | Apply an 'Add' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.1
--
-- - An empty 'Path' replaces the document.
-- - A single 'OKey' inserts or replaces the corresponding member in an object.
-- - A single 'AKey' inserts at the corresponding location.
-- - Longer 'Paths' traverse if they can and fail otherwise.
applyAdd :: Path -> Value -> Value -> Result Value
applyAdd [] val _ =
    return val
applyAdd [AKey i] v' (Array v) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn _ = return (Just v')
    in return (Array $ vInsert i v' v)
applyAdd (AKey i : path) v' (Array v) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn Nothing = fail "Cannot insert beneath missing array index."
        fn (Just d) = Just <$> applyAdd path v' d
    in Array <$> vModify i fn v
applyAdd [OKey n] v' (Object m) =
    return . Object $ HM.insert n v' m
applyAdd (OKey n : path) v' (Object o) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn Nothing = fail "Cannot insert beneath missing object index."
        fn (Just d) = Just <$> applyAdd path v' d
    in Object <$> hmModify n fn o
applyAdd (OKey n : path) v' array@(Array v)
    | n == "-" = applyAdd (AKey (V.length v) : path) v' array
applyAdd path _ v = pointerFailure path v

-- | Apply a 'Rem' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.2
--
-- - The target location MUST exist.
applyRem :: Path -> Value -> Result Value
applyRem [] _ = return Null
applyRem [AKey i] d@(Array v) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn Nothing = fail $ "Cannot delete missing array member at " <> show i <> " " <> BS.unpack (encode d)
        fn (Just v) = return Nothing
    in Array <$> vModify i fn v
applyRem (AKey i : path) (Array v) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn Nothing = fail "Cannot traverse under missing array index."
        fn (Just o) = Just <$> applyRem path o
    in Array <$> vModify i fn v
applyRem [OKey n] (Object m) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn Nothing = fail "Cannot delete missing object member."
        fn (Just _) = return Nothing
    in Object <$> hmModify n fn m
applyRem (OKey n : path) (Object m) =
    let fn :: Maybe Value -> Result (Maybe Value)
        fn Nothing = fail "Cannot traverse under missing object index."
        fn (Just o) = Just <$> applyRem path o
    in Object <$> hmModify n fn m
-- Dodgy hack for "-" key which means "the end of the array".
applyRem (OKey n : path) array@(Array v)
    | n == "-" = applyRem (AKey (V.length v) : path) array
-- Type mismatch: clearly the thing we're deleting isn't here.
applyRem path value = pointerFailure path value

-- | Apply a 'Rep' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.3
--
-- - Functionally identical to a 'Rem' followed by an 'Add'.
applyRep :: Path -> Value -> Value -> Result Value
applyRep path v doc = applyRem path doc >>= applyAdd path v

-- | Apply a 'Mov' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.4
applyMov :: Path -> Path -> Value -> Result Value
applyMov path from doc = do
  v <- get from doc
  applyRem from doc >>= applyAdd path v

-- | Apply a 'Cpy' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.5
--
-- - The location must exist.
-- - Identical to an add with the appropriate value.
applyCpy :: Path -> Path -> Value -> Result Value
applyCpy path from doc = do
  v <- get from doc
  applyAdd path v doc

-- | Apply a 'Tst' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.6
--
-- - The location must exist.
-- - The value must be equal to the supplied value.
applyTst :: Path -> Value -> Value -> Result Value
applyTst path v doc = do
    v' <- get path doc
    unless (v == v') (fail "Tested elements do not match.")
    return doc

-- | Get the value at a 'Path'.
--
-- - The path must exist.
get :: Path -> Value -> Result Value
get [] v = return v
get (AKey i : path) (Array v) =
    maybe (fail "") return (v V.!? i) >>= get path
get (OKey n : path) (Object v) =
    maybe (fail "") return (HM.lookup n v) >>= get path
get path value = pointerFailure path value

-- * Formatting patches

-- | Format a 'Patch' for reading by humans.
--
-- For storing or exchanging 'Patch'es between systems using the JSON encoding
-- implemented by the 'FromJSON' and 'ToJSON' instances.
formatPatch
    :: Patch
    -> Text
formatPatch (Patch ops) = T.unlines $ fmap formatOp ops
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
    formatOp (Add (Pointer k) v) = formatPath k <> "\n" <> "+" <> formatValue v
    formatOp (Rem (Pointer k) _) = formatPath k <> "\n" <> "-"
    formatOp (Rep (Pointer k) v) = formatPath k <> "\n" <> "=" <> formatValue v
    formatOp (Mov (Pointer k) (Pointer f)) = formatPath k <> "\n" <> "<" <> formatPath f
    formatOp (Cpy (Pointer k) (Pointer f)) = formatPath k <> "\n" <> "~" <> formatPath f
    formatOp (Tst (Pointer k) v) = formatPath k <> "\n" <> "?" <> formatValue v

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
    Object o -> sum . fmap valueSize . HM.elems $ o
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
vModify :: Int -> (Maybe a -> Result (Maybe a)) -> Vector a -> Result (Vector a)
vModify i f v =
    let a = v V.!? i
        a' = f a
    in case (a, a') of
        (Nothing, Success Nothing ) -> return v
        (Just _ , Success Nothing ) -> return (vDelete i v)
        (Nothing, Success (Just n)) -> return (vInsert i n v)
        (Just _ , Success (Just n)) -> return (V.update v (V.singleton (i, n)))
        (_      , Error   e       ) -> fail e

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
    Error e -> fail e
    Success Nothing  -> return $ HM.delete k m
    Success (Just v) -> return $ HM.insert k v m
