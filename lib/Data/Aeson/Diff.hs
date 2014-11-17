module Data.Aeson.Diff (
    Patch,
    Path,
    Key(..),
    -- * Functions
    diff,
    patch,
    -- * Utility functions
    explode,
    collapse,
) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Vector as V

-- | A path through a JSON document is a possibly empty sequence of 'Key's.
type Path = [Key]

-- | Traverse a single layer of a JSON document.
data Key
    = OKey Text
    | AKey Int
  deriving (Eq, Ord, Show)

-- | Describes the changes between two JSON documents.
data Patch = Patch
  deriving (Eq, Ord, Show)

-- | Compare two JSON documents and generate a patch describing the differences.
diff
    :: Value
    -> Value
    -> Patch
diff _ _ = error "Data.Aeson.Diff.diff: not implemented"

-- | Apply the a patch to a JSON document.
patch
    :: Patch
    -> Value
    -> Value
patch _ _ = error "Data.Aeson.Diff.patch: not implemented"

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

    modifyObj obj k f = HM.alter obj k f
