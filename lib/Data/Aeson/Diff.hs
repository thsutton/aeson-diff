-- | Description: Diff and patch operations for JSON.
module Data.Aeson.Diff (
    Patch,
    patchOperations,
    Path,
    Key(..),
    -- * Functions
    diff,
    patch,
    -- * Utility functions
    explode,
    collapse,
) where


import Control.Applicative
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Vector as V

-- | Describes the changes between two JSON documents.
data Patch = Patch { patchOperations :: [Operation] }
  deriving (Eq)

instance Monoid Patch where
    mempty = Patch []
    mappend (Patch p1) (Patch p2) = Patch $ p1 <> p2

-- | Descripts an atomic change to a JSON document.
data Operation
    = Ins { changePath :: Path, changeValue :: Value } -- ^ Insert a value at a location.
    | Del { changePath :: Path, oldValue :: Value } -- ^ Delete the value at a location.
  deriving (Eq, Show)

-- | A path through a JSON document is a possibly empty sequence of 'Key's.
type Path = [Key]

-- | Traverse a single layer of a JSON document.
data Key
    = OKey Text
    | AKey Int
  deriving (Eq, Ord, Show)

-- * Atomic patches

-- | Construct a patch with a single 'Ins' operation.
ins p v = Patch [Ins p v]

-- | Construct a patch with a single 'Del' operation.
del p v = Patch [Del p v]

-- | Construct a patch which changes a single value.
ch p v1 v2 = Patch [Del p v1, Ins p v2]

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
            ds = Patch $ map (\k -> Del (p ++ [OKey k]) . fromJust $ HM.lookup k o1) dk
            is = Patch $ map (\k -> Ins (p ++ [OKey k]) . fromJust $ HM.lookup k o2) ik
            cs = mconcat $ map (\k -> worker (p ++ [OKey k]) (fromJust $ HM.lookup k o1) (fromJust $ HM.lookup k o2)) ck
        in ds <> is <> cs

    -- Walk the indexes in two arrays, producing a 'Patch'.
    workArray :: Path -> Array -> Array -> Patch
    workArray p _ _ = mempty

-- | Apply a patch to a JSON document.
patch
    :: Patch
    -> Value
    -> Value
patch (Patch []) v = v
patch (Patch ops) v = error "Data.Aeson.Diff.patch: not implemented"

-- | Decompose a JSON document into 'Path's and atomic values.
explode
    :: Value
    -> [(Path, Value)]
explode = worker []
  where
    worker prefix doc = case doc of
        Array arr  -> concatMap (\(n,v) -> worker (AKey n:prefix) v)
            $ zip [0..] $ V.toList arr
        Object obj -> concatMap (\(k,v) -> worker (OKey k:prefix) v) $ HM.toList obj
        atomic     -> [(prefix, atomic)]

-- | Collapse pairs of 'Path's and atomic values into a JSON document.
collapse
    :: [(Path, Value)]
    -> Value
collapse = foldl work Null
  where
    work doc ([],  val) = val
    work (Array arr) (AKey k:rest, val) = Array arr
    work (Object obj) (OKey k:rest, val) = Object $ modifyObj obj k (const)
    work _ _ = error "Data.Aeson.Diff.collapse: not implemented"
    modifyObj obj k f = error "modifyObj: not implemented"
