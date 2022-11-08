{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate where

import Prelude hiding (not)

import System.Exit

import Control.Monad

import Data.List

import Base
import Binomial

approximate :: NumberConstraint
approximate xs k = 
    let n = length xs
        x i = xs !! (i-1)
        p is j = (True, P is j)
    in  []

allIss :: Int -> Int -> [[Int]]
allIss m l = concat $ makeIsss (l-1) $ return $ map return [1..m]
  where
  makeIsss :: Int -> [[[Int]]] -> [[[Int]]]
  makeIsss l isss = if l == 0
    then isss
    else makeIsss (l-1) $ isss ++ [[ is ++ [i] | is <- last isss, i <- [1..m] ]]

approx :: (Int, Int, Int) -> CNF
approx (a, m, l) =
    let n = a * m^l
        p is j = (True, P is j)
        iss = allIss m l
        order = flip concatMap iss \is ->
          flip map [2..a] \j ->
            [ not $ p is j, p is $ j-1 ]
        atMost = flip concatMap iss \is ->
          let ps = p <$> filter ((==) is . init) iss <*> [1..a]
          in  flip concatMap [1..a] \j ->
                map ((:) $ p is j) $ binomial ps $ m*(j-1)
    in  order ++ atMost

{-
countObjBss n =
  let bssAll = countAllBss n
      l = maximum $ map length bssAll
  in  filter ((==) l . length) bssAll
vars'cnfsCount strId n ord =
  let vf = VarCountOf10 strId
      cnfOneHot bss = cnf ("CountOf10Body:OneHot:" ++ show bss) (bvssOne (map (vf False bss) [0..10]))
      cnfCnt m bs p = if m == 0
        then []
        else flip concatMap [0..10] \q ->
          let r = p - (q - p)
              commentHead = "CountOf10Body:" ++ show bs ++ show p ++ ":"
          in  if 0 <= r && r <= 10
              then cnf (commentHead ++ show q ++ "-" ++ show r) (bvssWhen (vf False bs p) $ bvssWhen (vf False (bs ++ [False]) q) $ bvssAtLeast 1 [vf False (bs ++ [True]) r])
                ++ cnfCnt (m - 1) (bs ++ [False]) q
                ++ cnfCnt (m - 1) (bs ++ [True]) r
              else cnf (commentHead ++ show q ++ ":N/A") $ bvssWhen (vf False bs p) $ bvssNo [vf False (bs ++ [False]) q] ++ bvssNo [vf False (bs ++ [True]) q]
      bvssOrd = case ord of
        LT -> bvssAtMost
        EQ -> bvssEq
        GT -> bvssAtLeast
      cnfObjs = flip concatMap ((,) <$> countObjBss n <*> [0..10]) \(bs, k) ->
        cnf ("CountOf10Body:" ++ show bs ++ show k) $ bvssWhen (vf False bs k) $ bvssOrd k $ map (vf True bs) [0..9]
      cnfs = concatMap cnfOneHot (countAllBss n) ++ concatMap (cnfCnt n []) [0..10] ++ cnfObjs
      vars = (vf False <$> countAllBss n <*> [0..10])
        ++ (vf True <$> countObjBss n <*> [0..9])
  in  (vars, cnfs)
  -- > mapM_ (putStrLn . showCnfLineHuman) $ cnfCount "tr" 1
-}