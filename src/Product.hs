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

product :: Eq a => NumberConstraint a (String, Vproduct)
product = prod ""
    where
    prod :: Eq a => String -> [Literal a] -> Int -> CNF (Either a (String, Vproduct))
    prod strId xs k = if length xs <= k then [] else
        if length xs == k + 1
        then [map not $ lifts xs]
        else
            let n = length xs
                xv = xvOn (k, n)
                x i = lifts xs !! (i-1)
                a d xvi =
                    let (xva, xvb) = splitAt d $ xvi
                        xv'd = init xva ++ xvb
                    in  (True, Aux $ Right (strId, A d xv'd))
                arrange
                    :: String
                    -> CNF (Either (Either a (String, Vproduct)) (String, Vproduct))
                    -> CNF (Either a (String, Vproduct))
                arrange strId' = fmapCNF $ \case
                    Left (Left v) -> Left v
                    Left (Right (strId'', v)) -> Right (strId'', v)
                    Right (strId'', v) -> Right (strId'' ++ ":" ++ strId', v)
            in  [ [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] ]
                ++ concat
                    [ arrange (show d) $
                        prod (strId ++ "-" ++ show d) (nub [ a d $ xv i | i <- [1..n] ]) k
                    | d <- [1 .. k+1]
                    ]
