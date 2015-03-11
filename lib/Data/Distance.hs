{-# LANGUAGE TupleSections #-}

-- | Description: Find differences between sequences based on edit metrics.
--
-- This module implements a fairly generic approach to finding the smallest
-- difference between two sequences.
module Data.Distance where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

type ChangeMatrix a b = [((Int, Int), (a,a,Int,b))]

leastChanges
    :: (a -> a -> (Int, b))
    -> Vector a
    -> Vector a
    -> [b]
leastChanges f as bs =
    let mat = difference f (V.toList as) (V.toList bs)
        in findPath mat (0,0)

-- | Find the /smallest/ sequence of changes between to lists of items.
difference
    :: (a -> a -> (Int, b)) -- ^ Calculate a difference and its size.
    -> [a] -- ^ Input sequence.
    -> [a] -- ^ Output sequence.
    -> ChangeMatrix a b
difference diff as bs = cost
    [ ((x,y),(a,b,s,d))
    | (x,a) <- zip [0..] as
    , (y,b) <- zip [0..] bs
    , let (s, d) = diff a b
    ]

-- | Accumulate costs of the least path through a change matrix.
cost
    :: ChangeMatrix a b
    -> ChangeMatrix a b
cost [] = []
cost m  = work (0,0) m
  where
    work :: (Int, Int) -> ChangeMatrix a c -> ChangeMatrix a c
    work _ _ = []

-- | Find a lest-cost path through a 'ChangeMatrix', accumulating changes.
findPath
    :: ChangeMatrix a c -- ^ The matrix to traverse.
    -> (Int, Int) -- ^ Starting point.
    -> [c]
findPath m i =
    case lookup i m of
        Nothing -> error $ "No element defined for: " <> show i
        Just (_f,_t,_s,c) ->
            case cands m i of
                [] -> [c]
                _cs -> []

-- | Find candidates for the predecessor to some cell.
cands
    :: ChangeMatrix a c -- ^ Changes matrix
    -> (Int,Int) -- ^ Current cell
    -> [(Int,Int)]
cands m (x,y) = fst <$> mapMaybe (check m)
    [ (x-1,y  )
    , (x  ,y-1)
    , (x-1,y-1)
    ]
  where
    check :: ChangeMatrix a c -> (Int,Int) -> Maybe ((Int,Int) , (a,a,Int,c))
    check n i = (i,) <$> lookup i n
