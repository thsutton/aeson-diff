module Data.Aeson.Diff (
    diff,
    patch,
    Patch,
) where

import Data.Aeson
import Data.Text (Text)

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
explode document = error "Data.Aeson.Diff.explode: not implemented"
