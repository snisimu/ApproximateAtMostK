{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Product (product) where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.List hiding (product)

import Base

{-
xvOn :: KN -> Int -> [Int]
xvOn (k, n) i = gen (map (\p -> [1..p]) ps) !! (i-1)
  where
    gen is's =
      let  is : iss = reverse is's
      in   foldr add (map return is) iss
    add is iss = flip concatMap is \i -> map ((:) i) iss
    ps =
      let psFor p's = if foldr1 (*) p's >= n
            then p's
            else psFor $ incr p's
      in  psFor $ replicate (k+1) 1
    incr p's = if length (nub p's) == 1
      then head p's + 1 : tail p's
      else
        let (former, latter) = break (/= head p's) p's
        in  former ++ (head latter + 1 : tail latter)
-}

xvOn :: KN -> Int -> [Int]
xvOn (k, _) = \case
    1 -> replicate (k+1) 1
    i -> 
        let (d, j) = (i-2) `divMod` (k+1)
            a = d + 2
        in  replicate j 1 ++ [a] ++ replicate (k-j) 1

product :: NumberConstraint
product = prod []
    where
    prod :: [ScopeID] -> NumberConstraint
    prod sIDs xs k = if length xs <= k+1
        then binomial xs k
        else
            let n = length xs
                xv = xvOn (k, n)
                x i = lifts xs !! (i-1)
                a d xvi =
                    let (xva, xvb) = splitAt d $ xvi
                        xv'd = init xva ++ xvb
                    in  (True, A sIDs d xv'd))
            in  [ [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] ]
                ++ concat
                    [ prod (show d : sIDs) (nub [ a d $ xv i | i <- [1..n] ]) k
                    | d <- [1 .. k+1]
                    ]
