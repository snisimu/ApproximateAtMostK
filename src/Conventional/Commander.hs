{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Conventional.Commander (commanderWith) where

import Prelude hiding (not)

import Control.Monad

import Data.Maybe

import Base
import Lib
import Conventional.Binomial

sFor :: KN -> Int
sFor (k, n) = k + n `div` 3

commanderWith :: NumberConstraint -> NumberConstraint
commanderWith atMost = command Nothing
    where
    command :: Maybe Int -> NumberConstraint
    command mbM vScope xs 0 = binomial vScope xs 0
    command mbM vScope xs k =
        let m :: Int
            m =
                let (a, b) = length xs `divMod` sFor (k, length xs)
                    m' = a + if 0 < b then 1 else 0
                in  maybe m' (min m') mbM
            vScopeNext sID = vScope . Scope ("comm:" ++ sID)
        in  if m <= 1
            then atMost (vScopeNext "final") xs k
            else
                let n = length xs
                    hss = divideInto m [1..n]
                    g = length hss
                    c i j = (True, vScope $ C i j)
                    mbMnext = Just $ m-1
                    c1 = flip concatMap [1..g] \i -> 
                            let lits
                                    =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                                    ++ [ not $ c i j | j <- [1..k] ]
                            in  command mbMnext (vScopeNext "c1AM") lits k
                                ++ atLeastBy (command mbMnext) (vScopeNext "c1AL") lits k
                    c2 =
                        [ [not $ c i j, c i (j+1)]
                        | i <- [1..g]
                        , j <- [1..k-1]
                        ]
                    c3 =
                        let lits = [ c i j | i <- [1..g], j <- [1..k] ]
                        in  command mbMnext (vScopeNext "c3") lits k
                in  c1 ++ c2 ++ c3

{-
commanderWith' :: NumberConstraint -> NumberConstraint
commanderWith' atMost vScope xs k =
    let m :: Int
        m =
            let (a, b) = length xs `divMod` sFor (k, length xs)
            in  a + if 0 < b then 1 else 0
        vScopeNext sID = vScope . Scope ("comm:" ++ sID)
        n = length xs
        hss = divideInto m [1..n]
        g = length hss
        c i j = (True, vScope $ C i j)
        mbMnext = Just $ m-1
        c1 = flip concatMap [1..g] \i -> 
                let lits
                        =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                        ++ [ not $ c i j | j <- [1..k] ]
                in  atMost (vScopeNext "c1AM") lits k
                    ++ atLeastBy atMost (vScopeNext "c1AL") lits k
        c2 =
            [ [not $ c i j, c i (j+1)]
            | i <- [1..g]
            , j <- [1..k-1]
            ]
        c3 =
            let lits = [ c i j | i <- [1..g], j <- [1..k] ]
            in  atMost (vScopeNext "c3") lits k
    in  c1 ++ c2 ++ c3
-}
