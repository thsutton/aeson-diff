{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Aeson
import Data.Aeson.Diff
import qualified Data.Vector as V

instance Arbitrary Value where
    arbitrary = elements
        [ Null
        , Bool True
        , Number 123
        , String "boo"
        , Array . V.fromList $ [String "123", Number 123]
        ]

-- | Check that extracting and applying a diff results in the targf
prop_DiffApply
    :: Value
    -> Value
    -> Bool
prop_DiffApply f t = t == patch (diff f t) f

-- | Check that (collapse . explode) == id
prop_ExplodeCollapse
    :: Value
    -> Bool
prop_ExplodeCollapse doc = doc == (collapse . explode) doc

--
-- Use Template Haskell to automatically run all of the properties above.
--

return []
main = $quickCheckAll
