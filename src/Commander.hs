{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Control.Monad

import Data.Maybe

import Base
import Binomial

s :: Int
s = 10

commander :: NumberConstraint
commander x's k' = command [] Nothing x's k'
    where
    command :: [ScopeID] -> Maybe Int -> NumberConstraint
    command sIDs mbM xs k = if m' <= 1
        then binomial xs k
        else 
            let hss = divideInto m [1..n]
                g = length hss
                n = length xs
                c i j = (True, C sIDs i j)
                mNext =
                    let m' = length xs `div` s + 1
                    in  Just $ maybe m' $ min m'
                c1 = flip concatMap [1..g] \i -> 
                        let lits
                                =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                                ++ [ not $ c i j | j <- [1..k] ]
                        in  command ("c1AM" : sIDs) mNext lits k
                            ++ atLeastBy (command ("c1AL" : sIDs) mNext) lits k
                c2 =
                    [ [not $ c i j, c i (j+1)]
                    | i <- [1..g]
                    , j <- [1..k-1]
                    ]
                c3 =
                    let lits = [ c i j | i <- [1..g], j <- [1..k] ]
                    in  command ("c3" : sIDs) mNext lits k
            in  c1 ++ c2 ++ c3

commanderIO x's k' = commandIO [] (Just $ mFor x's) x's k'
    where
    commandIO sIDs mbM xs k = do
        let m = fromMaybe (mFor xs) mbM
        if m <= 1
        then putStrLn $ "binomial" ++ show (length xs, k)
        else do
            let hss = divideInto m [1..n]
                g = length hss
                n = length xs
                c i j = (True, C sIDs i j)
            putStrLn $ showSIDs sIDs ++ show (n,k)
            forM_ [1..g] \i -> do
                let lits
                        =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                        ++ [ not $ c i j | j <- [1..k] ]
                commandIO ("c1AM" : sIDs) Nothing lits k
                commandIO ("c1AL" : sIDs) Nothing lits k
            let c2 =
                    [ [not $ c i j, c i (j+1)]
                    | i <- [1..g]
                    , j <- [1..k-1]
                    ]
            commandIO ("c3" : sIDs) (Just $ m - 1) [ c i j | i <- [1..g], j <- [1..k] ] k
