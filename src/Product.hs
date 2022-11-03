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

xvOn :: Int -> Int -> [Int]
xvOn k = \case
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
        let n = length xs
            xv = xvOn k
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
        in  [ {- [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] -} ]
            ++ concat
                [ arrange (show d) $
                    prod (strId ++ "-" ++ show d) (nub [ a d $ xv i | i <- [1..n] ]) k
                | d <- [1 .. k+1]
                ]

productTr1 :: String -> [Literal (Either () (String, Vproduct))] -> Int -> IO ()
productTr1 strId xs k = unless (length xs <= k) $ do
    let n = length xs
        xv = xvOn k
        x i = lifts xs !! (i-1)
        a :: Int -> [Int] -> Literal (Either () (String, Vproduct))
        a d xvi =
            let (xva, xvb) = splitAt d $ xvi
                xv'd = init xva ++ xvb
            in  (True, Aux $ Right (strId, A d xv'd))
        arrange
            :: String
            -> CNF (Either (Either () (String, Vproduct)) (String, Vproduct))
            -> CNF (Either () (String, Vproduct))
        arrange strId = fmapCNF $ \case
            Left (Left v) -> Left v
            Left (Right (strId', v)) -> Right (strId', v)
            Right (strId', v) -> Right (strId' ++ ":" ++ strId, v)
    putStrLn $ "xs: " ++ show xs
    let -- tr1 :: (Eq a, Show a) => [Literal (Either a (String, Vproduct))]
        tr1 = [ a 1 $ xv i | i <- [1..n] ]
    putStrLn $ "tr1: " ++ show tr1
    let -- tr2 :: (Show a) => [Literal (Either a (String, Vproduct))]
        tr2 = nub tr1
    putStrLn $ "tr2: " ++ show tr2
    let -- addSymbol :: Show a => String -> [Literal (Either a (String, Vproduct))] -> [Literal (Either a (String, Vproduct))]
        addSymbol strId = map $ fmap $ fmap $ \case
            Left v -> Left v
            Right (strId', v) -> Right (strId' ++ strId, v)
    let -- tr3 :: Show a => [Literal (Either a (String, Vproduct))]
        tr3 = addSymbol "1" tr2
    putStrLn $ "tr3: " ++ show tr3
    productTr1 (strId ++ "1'") tr3 k

productTr2 :: [Literal (Either a (String, Vproduct))] -> Int -> IO ()
productTr2 = prod ""
    where
    prod :: Eq a => String -> [Literal a] -> Int -> IO ()
    prod strId xs k = if length xs <= k then return () else do
        let n = length xs
            xv = xvOn k
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
        {-}
        in  [ {- [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] -} ]
            ++ concat
                [ arrange (show d) $
                    prod (strId ++ "-" ++ show d) (nub [ a d $ xv i | i <- [1..n] ]) k
                | d <- [1 .. k+1]
                ]
