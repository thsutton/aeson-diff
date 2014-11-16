{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Aeson
import Data.Aeson.Diff

instance Arbitrary Value

-- | Check that extracting and applying a diff results in the targf
prop_DiffApply
    :: Value
    -> Value
    -> Bool
prop_DiffApply f t = t == patch (diff f t) f

-- 
-- Use Template Haskell to automatically run all of the properties above.
--

return []
main = $quickCheckAll
