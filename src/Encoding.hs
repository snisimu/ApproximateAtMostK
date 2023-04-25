{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Encoding where

import Prelude hiding (not)

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

import Data.Maybe
import Data.List

import Base
import Lib
import Conventional.Binomial

approxOrderWith :: NumberConstraint -> VarScope -> (ParameterTree, Int) -> CNF
approxOrderWith atMost vScope ((hws, m), k) =
  let vScopeNext sID = vScope . Scope ("approxOrderWith:" ++ sID)
      p is j = (True, vScope $ P is j)
      (h, w) = head hws
      cnfTop = atMost (vScopeNext "top") [ p [i] j | i <- [1..w], j <- [1..h] ] k
      (cnfP, (hLeaf, isLeafs)) = approxOrderPwith atMost vScope hws
      cnfX =
        let h' = fst $ last hws
            wAll = product $ map snd hws
            xss = splitBy (h'*m) $ literalXs $ h'*m*wAll
        in  flip concatMap (zip isLeafs xss) \(is, xs) -> 
              flip concatMap [1..hLeaf] \j ->
                map ((:) $ p is j) $
                  atMost (vScopeNext $ "X:" ++ show is ++ show j) xs $
                    ((j-1)*h'*m) `div` hLeaf
  in  cnfTop ++ cnfP ++ cnfX
  -- > reportOf $ approxOrderWith binomial id ([(2,2)],2) 2)
  -- > generateDIMACSwithTrue (approxOrderWith counter id ([(2,2)],2) 2) [1,2,3,4]
  -- > wsl -- ./minisat the.cnf

approxOrderPwith :: NumberConstraint -> VarScope -> [HW] -> (CNF, (Height, [[Int]]))
approxOrderPwith atMost vScope hws =
  let p is j = (True, vScope $ P is j)
      is'hs = labeling hws
      cnfOrder = flip concatMap is'hs \(is, h) ->
        flip map [2..h] \j ->
          [ not $ p is j, p is $ j-1 ]
      cnfAtMost = flip concatMap is'hs \(is, h) ->
        let theIs'hs = filter ((==) is . init . fst) is'hs
            ps = concatMap (\(is, h) -> p is <$> [1..h]) theIs'hs
            h' = if null theIs'hs then 1 else snd $ head theIs'hs
            w' = length theIs'hs
        in  flip concatMap [1..h] \j ->
              let theScope = vScope . Scope ("approxOrderPwith:" ++ show is ++ show j)
              in  map ((:) $ p is j) $ 
                    --
                    atMost theScope ps $ (h'*w'*(j-1)) `div` h
                    --
      hLeaf = if null hws then 1 else fst $ last hws
      isLeafs =
        let iss = map fst is'hs
        in  filter ((==) (length hws) . length) iss
  in  (cnfOrder ++ cnfAtMost, (hLeaf, isLeafs))

-- 

totalExact :: VarScope -> ([[Int]], Height) -> Int -> CNF
totalExact vScope (iss, h) k = 
  let p is j = (True, vScope $ P is j)
      isJ'ss = map (zip iss) [ js | js <- allCombinationssOf [0..k] $ length iss, sum js == k ]
      isJss = distribution  isJ'ss
  in  [ [ p is j | (is, j) <- isJs ] | isJs <- isJss ]

approxDirectWith :: NumberConstraint -> VarScope -> ParameterTree -> Int -> CNF
approxDirectWith atMost vScope (hws, m) k =
  let p is j = (True, vScope $ P is j)
      (h, w) = head hws
      cnfTop = totalExact vScope ([ [i] | i <- [1..w] ], h) k
      (cnfP, (hLeaf, isLeafs)) = approxDirectPwith atMost vScope hws
      cnfX =
        let (h', w') = last hws
            wAll = product $ map snd hws
            xss = splitBy (h'*m) $ literalXs $ h'*m*wAll
        in  flip concatMap (zip isLeafs xss) \(is, xs) -> 
              flip concatMap [0..hLeaf] \j ->
                map ((:) $ not $ p is j) $
                  atMost (vScope . Scope ("approxDirectWith:" ++ show is ++ show j)) xs $ (h'*w'*j) `div` hLeaf
  in  cnfTop ++ cnfP ++ cnfX
  -- > generateDIMACSwithTrue (approxDirectWith counter id ([(2,2)],2) 2) [1,2,3,4]
  -- > wsl -- ./minisat the.cnf

approxDirectPwith :: NumberConstraint -> VarScope -> [HW] -> (CNF, (Height, [[Int]]))
approxDirectPwith atMost vScope hws = 
  let p is j = (True, vScope $ P is j)
      is'hs = labeling hws
      cnfDirect = flip concatMap is'hs \(is, h) ->
        let ps = [ p is j | j <- [0..h] ]
        in  binomial vScope ps 1 ++ atLeastBy binomial vScope ps 1
      cnfAtMost = flip concatMap is'hs \(is, h) ->
        let theScope = vScope . Scope ("approxDirectPwith:" ++ show is)
            theIs'hs = filter ((==) is . init . fst) is'hs
            theIss = map fst theIs'hs
            h' = if null theIs'hs then 0 else snd $ head theIs'hs
            w' = length theIss
            -- ps = concatMap (\(is, h) -> p is <$> [0..h]) theIs'hs
        in  flip concatMap [0..h] \j ->
              map ((:) $ not $ p is j) $ totalExact theScope (theIss, h) $ (h'*w'*j) `div` h
      hLeaf = fst $ last hws
      isLeafs =
        let iss = map fst is'hs
        in  filter ((==) (length hws) . length) iss
  in  (cnfDirect ++ cnfAtMost, (hLeaf, isLeafs))
