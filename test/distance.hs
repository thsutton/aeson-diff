{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import Data.Monoid
import qualified Data.Vector as V
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Distance

strParam :: ChangeParameters Char (Maybe Char)
strParam = ChangeParameters
    (error "Cannot diff")
    (error "Cannot cost")

-- | Patch extracted from identical documents should be mempty.
prop_diff_id
    :: String
    -> Bool
prop_diff_id s =
    let v = V.fromList s
    in leastChanges strParam v v == mempty

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
