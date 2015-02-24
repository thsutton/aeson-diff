{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import Data.Aeson
import qualified Data.Vector as V
import System.Exit
import Test.QuickCheck

import Data.Aeson.Diff

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
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    result <- runTests
    unless result exitFailure
