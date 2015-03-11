{-# LANGUAGE TupleSections #-}

-- | Description: Find differences between sequences based on edit metrics.
--
-- This module implements a fairly generic approach to finding the smallest
-- difference between two sequences.
module Data.Distance where

import Control.Applicative
import Data.Maybe
import Data.Monoid

type ChangeMatrix a b = [((Int, Int), (a,a,Int,b))]

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

cost
    :: ChangeMatrix a b
    -> ChangeMatrix a b
cost [] = []
cost m = work (0,0) m
  where
    work (x,y) m = 

diff_str = difference comp

comp :: Char -> Char -> (Int, Maybe Char)
comp a b
    | a == b    = (0, Nothing)
    | otherwise = (1, Just b)

findPath
    :: ChangeMatrix a c
    -> (Int, Int)
    -> [c]
findPath m i =
    case lookup i m of
        Nothing -> error $ "No element defined for: " <> show i
        Just (f,t,s,c) ->
            case cands m i of
                [] -> [c]
                cs -> []

-- | Find candidates for the predecessor to some cell.
cands
    :: ChangeMatrix a c -- ^ Changes matrix
    -> (Int,Int) -- ^ Current cell
    -> [(Int,Int)]
cands m (x,y) = map fst $ mapMaybe (check m)
    [ (x-1,y  )
    , (x  ,y-1)
    , (x-1,y-1)
    ]
  where
    check :: ChangeMatrix a c -> (Int,Int) -> Maybe ((Int,Int) , (a,a,Int,c))
    check n i = (i,) <$> lookup i n
