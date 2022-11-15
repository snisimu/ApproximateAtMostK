{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Lib where

import System.Exit
import Control.Monad

import Base
import Approximate.Base

checkParameter :: Parameter -> IO ()
checkParameter = (checkParam <$> fst . head <*> tail) . fst
  where
  checkParam :: Height -> [HW] -> IO ()
  checkParam h = \case
    [] -> return ()
    (h', w') : hws -> do
      unless ((h'*w') `mod` h == 0) $ die $ show (h'*w') ++ " mod " ++ show h ++ " /= 0"
      checkParam h' hws

ftss k n = filter ((==) k . length . filter id) $ allFTssOf n

combinationNum r n = (product [1..n]) `div` (product [1..r] * product [1..n-r])
combinationLTnum r n = sum $ map (flip combinationNum n) [0..r]

labeling :: [HW] -> [([Int], Height)]
labeling = tail . concat . foldl makeH'Isss [[([], 0)]]
  where
  makeH'Isss ishss (h, w) =
    ishss ++ [[ (is ++ [i], h) | (is, _) <- last ishss, i <- [1..w] ]]
