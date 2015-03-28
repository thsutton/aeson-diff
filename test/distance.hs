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

str = strParams

-- | Patch extracted from identical documents should be mempty.
prop_distance_id :: String -> Bool
prop_distance_id s =
    leastChanges str s s == (0, [])

-- | Delete everything!
prop_distance_delete :: NonEmptyList Char -> Bool
prop_distance_delete (NonEmpty s) =
    leastChanges str s "" == (length s, [ D c | c <- s])

-- | Insert everything!
prop_distance_insert :: NonEmptyList Char -> Bool
prop_distance_insert (NonEmpty s) =
    leastChanges str "" s == (length s, [ I c | c <- s])

-- | The examples from wikipedia.
prop_distance_canned :: Bool
prop_distance_canned =
    let sitting = "sitting" :: String
        kitten = "kitten" :: String
        saturday = "Saturday" :: String
        sunday = "Sunday" :: String
    in leastChanges str sitting kitten == (3, [S 's' 'k',K,K,K,S 'i' 'e',K,D 'g'])
    && leastChanges str kitten sitting == (3, [S 'k' 's',K,K,K,S 'e' 'i',K,I 'g'])
    && leastChanges str saturday sunday == (3, [K,D 'a',D 't',K,S 'r' 'n',K,K,K])
    && leastChanges str sunday saturday == (3, [K,I 'a',I 't',K,S 'n' 'r',K,K,K])

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
