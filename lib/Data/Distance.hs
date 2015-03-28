{-# LANGUAGE TupleSections #-}

-- | Description: Find differences between sequences based on edit metrics.
--
-- This module implements a fairly generic approach to finding the smallest
-- difference between two sequences using the Wagner-Fischer algorithm.
module Data.Distance where

import Control.Arrow ((***))
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

type ChangeMatrix e = [( (Int,Int) , (Int, [C e]) )]

data C a
    = K
    | I a
    | D a
    | S a a
  deriving (Show, Eq)

-- | Find the least-cost sequence of changes to transform one vector into
-- another.
leastChanges :: Eq e => [e] -> [e] -> (Int, [C e])
leastChanges ss tt = fmap reverse . snd . last $ changes ss tt

-- | Calculate the complete matrix of changes which transform one sequence of
-- values into another.
changes :: Eq e => [e] -> [e] -> ChangeMatrix e
changes ss tt = sortBy (compare `on` fst) f
  where
    f =  [ ((0  ,   0), (0,   [K  ])) ]
      <> [ ((i+1,   0), (1+i, [D s])) | (i,s) <- items ss]
      <> [ ((0  , j+1), (1+j, [I t])) | (j,t) <- items tt]
      <> [ ((i+1, j+1), o)
         | (i,s) <- items ss
         , (j,t) <- items tt
         , let o = choose (i,s) (j,t) f
         ]
    items = zip [0..]

-- | Choose an operation to perform at an /internal/ cell in a 'ChangeMatrix'.
--
-- 'choose' requires that the 'ChangeMatrix' defines values at @(i,j)@,
-- @(i+1,j)@, and @(i, j+1)@ (i.e. the cells to the top-left, top, and left of
-- the cell being determined).
choose
    :: (Eq e)
    => (Int, e) -- ^ \"From\" index and value.
    -> (Int, e) -- ^ \"To\" index and value.
    -> ChangeMatrix e -- ^ Previous changes.
    -> (Int, [C e]) -- ^ Cost and change selected.
choose (i,s) (j,t) m =
    let tl    = get m    i    j
        top   = get m    i (1+j)
        left  = get m (1+i)    j
    in if s == t
        then (fst tl, K : snd tl)
        else minimumBy (compare `on` fst)
            [ (1 +) *** (D s   :) $ top
            , (1 +) *** (I t   :) $ left
            , (1 +) *** (S s t :) $ tl
            ]
  where
    get mat x y = fromMaybe
        (error $ "Unable to get " <> show (x,y) <> " from change matrix")
        (lookup (x,y) mat)
