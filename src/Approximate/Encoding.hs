{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Encoding where

import Prelude hiding (not)

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

import Data.Maybe
import Data.List

import Base
import Binomial

import Approximate.Base
import Approximate.Lib

approxOrderWith :: NumberConstraint -> VarScope -> [HW] -> Int -> CNF
approxOrderWith atMost vScope hws k =
  let vScopeNext sID = vScope . Scope ("approxOrderWith:" ++ sID)
      p is j = (True, vScope $ P is j)
      (h, w) = head hws
      cnfTop = atMost (vScopeNext "top") [ p [i] j | i <- [1..w], j <- [1..h] ] k
      (cnfP, (hLeaf, isLeafs)) = approxDirectPwith atMost vScope hws
      cnfX =
        let (h', w') = last hws
            m = product $ map snd $ init hws
            xss = splitBy (h'*w') $ literalXs $ h'*w'*m
        in  case xss of
              [xs] -> atMost (vScopeNext "X") xs k
              _ ->
                flip concatMap (zip isLeafs xss) \(is, xs) -> 
                  flip concatMap [1..hLeaf] \j ->
                    map ((:) $ p is j) $
                      atMost (vScopeNext $ "X:" ++ show is ++ show j) xs $ ((j-1)*h'*w') `div` hLeaf
  in  cnfTop ++ cnfP ++ cnfX
  -- > generateDIMACSwithTrue (approxOrderWith counter id [(2,2),(2,2)] 2) [1,2,3,4]
  -- > wsl -- ./minisat the.cnf

approxOrderPwith :: NumberConstraint -> VarScope -> [HW] -> (CNF, (Height, [[Int]]))
approxOrderPwith atMost vScope hws = 
  let hw's = init hws
      p is j = (True, vScope $ P is j)
      is'hs = labeling hw's
      cnfOrder = flip concatMap is'hs \(is, h) ->
        flip map [2..h] \j ->
          [ not $ p is j, p is $ j-1 ]
      cnfAtMost = flip concatMap is'hs \(is, h) ->
        let theIs'hs = filter ((==) is . init . fst) is'hs
            ps = concatMap (\(is, h) -> p is <$> [1..h]) theIs'hs
            h' = if null theIs'hs then 0 else snd $ head theIs'hs
            w' = length theIs'hs
        in  flip concatMap [1..h] \j ->
              let theScope = vScope . Scope ("approxOrderPwith:" ++ show is ++ show j)
              in  map ((:) $ p is j) $ 
                    atMost theScope ps $ (h'*w'*(j-1)) `div` h
      hLeaf = if null hw's then 1 else fst $ last hw's
      isLeafs =
        let iss = map fst is'hs
        in  filter ((==) (length hw's) . length) iss
  in  (cnfOrder ++ cnfAtMost, (hLeaf, isLeafs))

-- 

approxDirectWith :: NumberConstraint -> VarScope -> [HW] -> Int -> CNF
approxDirectWith atMost vScope hws k =
  let vScopeNext sID = vScope . Scope ("approxDirectWith:" ++ sID)
      p is j = (True, vScope $ P is j)
      (h, w) = head hws
      cnfTop = atMost (vScopeNext "top") [ p [i] j | i <- [1..w], j <- [1..h] ] k
      (cnfP, (hLeaf, isLeafs)) = approxDirectPwith atMost vScope hws
      cnfX =
        let (h', w') = last hws
            m = product $ map snd $ init hws
            xss = splitBy (h'*w') $ literalXs $ h'*w'*m
        in  case xss of
              [xs] -> atMost (vScopeNext "X") xs k
              _ ->
                flip concatMap (zip isLeafs xss) \(is, xs) -> 
                  flip concatMap [0..hLeaf] \j ->
                    map ((:) $ not $ p is j) $ atMost (vScopeNext $ "X:" ++ show is ++ show j) xs $ (h'*w'*j) `div` hLeaf
  in  cnfTop ++ cnfP ++ cnfX
  -- > generateDIMACSwithTrue (approxDirectWith counter id [(2,2),(2,2)] 2) [1,2,3,4]
  -- > wsl -- ./minisat the.cnf

approxDirectPwith :: NumberConstraint -> VarScope -> [HW] -> (CNF, (Height, [[Int]]))
approxDirectPwith atMost vScope hws = 
  let hw's = init hws
      p is j = (True, vScope $ P is j)
      is'hs = labeling hw's
      cnfDirect = flip concatMap is'hs \(is, h) ->
        let ps = [ p is j | j <- [0..h] ]
        in  binomial vScope ps 1 ++ atLeastBy binomial vScope ps 1
      cnfAtMost = flip concatMap is'hs \(is, h) ->
        let theIs'hs = filter ((==) is . init . fst) is'hs
            ps = concatMap (\(is, h) -> p is <$> [1..h]) theIs'hs
            h' = if null theIs'hs then 0 else snd $ head theIs'hs
            w' = length theIs'hs
        in  flip concatMap [0..h] \j ->
              let theScope = vScope . Scope ("approxDirectPwith:" ++ show is ++ show j)
                  lrs = [ (l,r) | l <- [0..h'], r <- [0..h'], l + r == (h'*w'*j) `div` h ]
              in  flip map lrs \(l,r) ->
                    [not $ p is l, p is r]
      hLeaf = if null hw's then 1 else fst $ last hw's
      isLeafs =
        let iss = map fst is'hs
        in  filter ((==) (length hw's) . length) iss
  in  (cnfDirect ++ cnfAtMost, (hLeaf, isLeafs))
