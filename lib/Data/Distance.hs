{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- | Description: Find differences between sequences based on edit metrics.
--
-- This module implements a fairly generic approach to finding the smallest
-- difference between two sequences using the Wagner-Fischer algorithm.
module Data.Distance where

import Control.Arrow ((***))
import Data.Function
import Data.List hiding (insert, delete)
import Data.Maybe
import Data.Monoid

data Params e c = Params
    { equivalent :: e -> e -> Bool
    , delete :: Int -> e -> c
    , insert :: Int -> e -> c
    , substitute :: Int -> e -> e -> c
    , cost :: c -> Int
    , nil :: c
    }

strParams :: Params Char (C Char)
strParams = Params{..}
  where
    equivalent = (==)
    delete _ = D
    insert _ = I
    substitute _ = S
    nil = K
    cost op = case op of
         D{} -> 1
         I{} -> 1
         S{} -> 1
         _   -> 0

data C a
    = K
    | I a
    | D a
    | S a a
  deriving (Show, Eq)

type ChangeMatrix c = [( (Int,Int) , (Int, [c]) )]

-- | Find the least-cost sequence of changes to transform one vector into
-- another.
leastChanges :: Params e c -> [e] -> [e] -> (Int, [c])
leastChanges p ss tt = case fmap reverse . snd . last $ changes p ss tt of
    -- No cost means no change.
    (0, _)   -> (0, [])
    -- Drop a leading K (which comes from the initial e~e)
    (n, h:r) -> (n, r)
    -- Otherwise leave it alone.
    o -> o

-- | Calculate the complete matrix of changes which transform one sequence of
-- values into another.
changes :: Params e c -> [e] -> [e] -> ChangeMatrix c
changes p@Params{..} ss tt = sortBy (compare `on` fst) f
  where
    f =  [ ((0  ,   0), (0,   [ nil ])) ]
      <> [ ((i+1,   0), (1+i, s))
         | (i,s) <- items . map (\i -> map (delete i) . reverse $ take i ss) $ [1..length ss]
         ]
      <> [ ((0  , j+1), (1+j, t))
         | (j,t) <- items . map (\i -> map (insert i) . reverse $ take i tt) $ [1..length tt]
         ]
      <> [ ((i+1, j+1), o)
         | (i,s) <- items ss
         , (j,t) <- items tt
         , let o = choose p (i,s) (j,t) f
         ]
    items = zip [0..]

-- | Choose an operation to perform at an /internal/ cell in a 'ChangeMatrix'.
--
-- 'choose' requires that the 'ChangeMatrix' defines values at @(i,j)@,
-- @(i+1,j)@, and @(i, j+1)@ (i.e. the cells to the top-left, top, and left of
-- the cell being determined).
choose
    :: Params e c
    -> (Int, e) -- ^ \"From\" index and value.
    -> (Int, e) -- ^ \"To\" index and value.
    -> ChangeMatrix c -- ^ Previous changes.
    -> (Int, [c]) -- ^ Cost and change selected.
choose Params{..} (i,s) (j,t) m =
    let tl    = get m    i    j
        top   = get m    i (1+j)
        left  = get m (1+i)    j
    in if s `equivalent` t
        then (fst tl, nil : snd tl)
        else minimumBy (compare `on` fst)
            [ let c = delete i s in (cost c +) *** (c:) $ top
            , let c = insert i t in (cost c +) *** (c:) $ left
            , let c = substitute i s t in (cost c +) *** (c:) $ tl
            ]
  where
    get mat x y = fromMaybe
        (error $ "Unable to get " <> show (x,y) <> " from change matrix")
        (lookup (x,y) mat)
