{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Product (product) where

import Prelude hiding (not)

import System.Exit

import Control.Monad

import Base

data Vproduct
    = A Int [Int]
    deriving (Eq, Show)

data ProductArg =
    { paKN :: KN
    , paPs :: [Int]
    , paXv :: Int -> [Int]
    }
    deriving (Eq, Show)

makeProductArg :: KN -> [Int] -> (Int -> [Int]) -> IO ProductArg
makeProductArg (k, n) ps xv = do
    unless (foldr1 (*) ps >= n) $ die "ps < n"
    forM_ [1..n] \i -> do
        forM_ (xv i) \xvi -> do
            unless (1 <= xvi && xvi <= ps !! (i - 1)) $
                die $ "xv" ++ show i ++ " beyonds its range"
    unless (length (map xv [1..n]) == length (nub $ map xv [1..n])) $
        die "xs are not unique"
    return $ ProductArg (k, n) ps xv

product :: ProductArg -> NumberConstraint a (String, Vproduct)
product prodArg xs k = case paKN prodArg == (k, length literals) of True -> 
    let ps = paPs prodArg
        xv = paX prodArg
    in  prod "" ps vx xs k
    where
        prod strId ps vx xs k =
            let x i = lifts xs !! (i-1)
                n = length literal's
                a d xvi =
                    let (xva, xvb) = splitAt d $ xvi
                        xv'd = init xva ++ xvb
                    in  (True, Aux $ Right $ A d xv'd)
                l = [ [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] ]
                arrange ::
                arrange
                r = [  |  ]
            