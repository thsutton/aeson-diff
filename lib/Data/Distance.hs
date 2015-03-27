{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- | Description: Find differences between sequences based on edit metrics.
--
-- This module implements a fairly generic approach to finding the smallest
-- difference between two sequences.
--
-- Essentially this looks like so
module Data.Distance where

import Data.Function
import Data.List
import Data.Monoid
import Data.Vector (Vector)

data C a
    = I a
    | D a
    | S a a
    | K
  deriving (Show, Eq)

diff a b = S a

cost :: C a -> Int
cost K = 0
cost _ = 1

-- | Find the least-cost sequence of changes to transform one vector into
-- another.
leastChanges :: String -> String -> [ ((Int, Int), (Int, C Char)) ]
leastChanges ss tt = sortBy (compare `on` fst) f
  where
    f =     [ ((0  ,   0), (0,   K  )) ]
         ++ [ ((i+1,   0), (1+i, D s)) | (i,s) <- items ss]
         ++ [ ((0  , j+1), (1+j, I t)) | (j,t) <- items tt]
         ++ [ ((i+1, j+1), o)
            | (i,s) <- items ss
            , (j,t) <- items tt
            , let o = choose (i,s) (j,t) f
            ]
    items = zip [0..]
    get m i j = case lookup (i,j) m of
        Nothing -> error $ "Unable to get " <> show (i,j) <> " from:\n" <> show m
        Just r -> r
    choose (i,s) (j,t) m =
        if s == t
            then (fst $ get m i j, K)
            else head . sortBy (compare `on` fst) $
                [ (1 + fst (get m    i (1+j)), D s)
                , (1 + fst (get m (1+i)   j ), I t)
                , (1 + fst (get m    i    j ), S s t)
                ]

