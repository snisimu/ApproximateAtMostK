{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate (approximate) where

import Prelude hiding (not)

import System.Exit

import Control.Monad

-- import Data.List

import Base
import Binomial

approximate :: NumberConstraint
approximate xs k = 
    let n = length xs
        x i = xs !! (i-1)
        p is j = (True, P is j)
    in  []

approx :: (Int, Int, Int) -> CNF
approx (a, m, l) =
    let n = a * m^l
        
    in  []
{-
countAllBss n = if n == 0 then [[]] else [] : makeBss (n - 1) [[False],[True]]
  where
  makeBss n bss = if n == 0
    then bss
    else makeBss (n - 1) $ bss ++ [ bs ++ [False] | bs <- bss ] ++ [ bs ++ [True] | bs <- bss ]

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