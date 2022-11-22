{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Lib where

import System.Exit
import Control.Monad

import Base
import Approximate.Base

checkParameter :: ParameterTree -> Bool
checkParameter = (checkParam <$> fst . head <*> tail) . fst
  where
  checkParam :: Height -> [HW] -> Bool
  checkParam h = \case
    [] -> True
    (h', w') : hws -> if (h'*w') `mod` h /= 0
      then False
      else checkParam h' hws

ftss k n = filter ((==) k . length . filter id) $ allCombinationssOf [False, True] n

labeling :: [HW] -> [([Int], Height)]
labeling = tail . concat . foldl makeH'Isss [[([], 0)]]
  where
  makeH'Isss ishss (h, w) =
    ishss ++ [[ (is ++ [i], h) | (is, _) <- last ishss, i <- [1..w] ]]

trueIndicesToBools :: Int -> [Int] -> [Bool]
trueIndicesToBools n = foldr makeTrueAt (replicate n False)
  where
  makeTrueAt a bs =
    let (b1s, _ : b2s) = splitAt a bs
    in  b1s ++ [True] ++ b2s

accuracy :: ParameterTree -> Float
accuracy (hws, m) =
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  fromInteger (toInteger $ h*w+1) / fromInteger (toInteger $ n+1)
