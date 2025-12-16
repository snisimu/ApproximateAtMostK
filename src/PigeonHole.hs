{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module PigeonHole where

import System.FilePath
import System.Process

import Control.Monad

import Data.List.Extra

import Text.Printf

import Base
import Lib
import Conventional.Binomial
import Conventional.Counter
import Encoding
import Evaluation

-- add target literals
approxPracticalWith :: NumberConstraint -> VarScope -> ParameterCNF -> [Literal] -> CNF
approxPracticalWith atMost vScope paramCNF xIns =
  let xIns' = xIns ++ replicate nFalse (False, vScope $ V "false") ++ replicate nTrue (True, vScope $ V "true")
      (((hws, m), k), (nFalse, nTrue)) = paramCNF
      vScopeNext sID = vScope . Scope ("approxOrderWith:" ++ sID)
      p is j = (True, vScope $ P is j)
      (h, w) = head hws
      cnfTop = atMost (vScopeNext "top") [ p [i] j | i <- [1..w], j <- [1..h] ] k
      (cnfP, (hLeaf, isLeafs)) = approxOrderPwith atMost vScope hws
      cnfX =
        let h' = fst $ last hws
            wAll = product $ map snd hws
            nX = h'*m*wAll
        in  if length xIns + nFalse + nTrue /= nX then error $ "approxPracticalWith: length xs /= nX :" ++ show (length xIns) ++ " /= " ++ show nX else
              let xss = splitBy (h'*m) xIns'
              in  flip concatMap (zip isLeafs xss) \(is, xs) -> 
                    flip concatMap [1..hLeaf] \j ->
                      map ((:) $ p is j) $
                        atMost (vScopeNext $ "X:" ++ show is ++ show j) xs $
                          ((j-1)*h'*m) `div` hLeaf
      paramT = (hws, m)
      (kT, nT) = knOfTree (paramT, k)
      cnfAppr = approxOrderWith binomial id (paramT, k)
      cnfFix = map (\i -> [xIns' !! (i-1)]) [nT-nTrue-nFalse+1..nT-nTrue]
          ++ map (\i -> [xIns' !! (i-1)]) [nT-nTrue+1..nT]
  in  cnfTop ++ cnfP ++ cnfX ++ cnfFix

pigeonHole :: (Int, Int) -> IO CNF
pigeonHole (l, m) = do
  let n = l * m
  (_, paramCNF) <- theBestEfficiency False True (l, l*m)
  let approx vScope xs = approxPracticalWith binomial vScope paramCNF xs
      lV i j = literal $ V $ "p" ++ show i ++ "h" ++ show j
  let holeAM = concatMap (\j -> approx (Scope $ "holeAM" ++ show j) $ [ lV i j | i <- [1..n] ]) [1..m]
      pigeonAM = concatMap (\i ->  (counter (Scope $ "pigeonAM" ++ show i) [ lV i j | j <- [1..m] ] 1)) [1..n]
      pigeonAL = map (\i -> [ lV i j | j <- [1..m] ]) [1..n]
  pure $ holeAM ++ pigeonAM ++ pigeonAL
  -- > pigeonHole (3, 3) >>= printDIMACS
  -- > pigeonHole (3, 3) >>= \cnf -> generateDIMACSwithTrue cnf []
