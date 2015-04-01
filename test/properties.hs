{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Vector as V
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Aeson.Diff

instance Arbitrary Value where
    arbitrary = elements
        [ Null
        , Bool True
        , Number 123
        , String "boo"
        , Array . V.fromList $ [String "123", Number 123]
        ]

-- | Patch extracted from identical documents should be mempty.
prop_diff_id
    :: Value
    -> Bool
prop_diff_id v = diff v v == mempty

-- | Extracting and applying a patch is an identity.
prop_diff_apply
    :: Value
    -> Value
    -> Bool
prop_diff_apply f t = t == patch (diff f t) f

-- | Extract and apply a patch (specialised to JSON arrays).
prop_diff_arrays
    :: [Value]
    -> [Value]
    -> Bool
prop_diff_arrays v1 v2 = prop_diff_apply (Array . V.fromList $ v1) (Array . V.fromList $ v2)

-- | Extract and apply a patch (specialised to JSON objects).
prop_diff_objects
    :: HashMap Text Value
    -> HashMap Text Value
    -> Bool
prop_diff_objects m1 m2 = prop_diff_apply (Object m1) (Object m2)

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
