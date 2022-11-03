{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Product (product) where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.List hiding (product)

import Base

data Vproduct
    = A Int [Int]
    deriving (Eq, Show)

xvsOn :: KN -> [[Int]]
xvsOn (k, n) = gen $ map (\p -> [1..p]) ps
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

product :: Eq a => NumberConstraint a (String, Vproduct)
product xs k = 
    let n = length xs
        xv i = xvsOn (k, n) !! (i-1)
        x i = lifts xs !! (i-1)
        a d xvi =
            let (xva, xvb) = splitAt d $ xvi
                xv'd = init xva ++ xvb
            in  (True, Aux $ Right ("", A d xv'd))
        arrange
            :: String
            -> CNF (Either (Either a (String, Vproduct)) (String, Vproduct))
            -> CNF (Either a (String, Vproduct))
        arrange strId = fmapCNF $ \case
            Left (Left v) -> Left v
            Left (Right (strId', v)) -> Right (strId', v)
            Right (strId', v) -> Right (strId' ++ ":" ++ strId, v)
    in  [ [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] ]
        ++ concat
            [ arrange (show d) $ product (nub [ a d $ xv i | i <- [1..n] ]) k
            | d <- [1 .. k+1]
            ]
