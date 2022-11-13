{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Lib where

import System.Exit
import Control.Monad

import Base
import Approximate.Base

multipleCheck :: [HW] -> IO ()
multipleCheck = multCheck <$> fst . head <*> tail
  where
  multCheck :: Height -> [HW] -> IO ()
  multCheck h = \case
    [] -> return ()
    (h', w') : hws -> do
      unless ((h'*w') `mod` h == 0) $ die $ show (h'*w') ++ " mod " ++ show h ++ " /= 0"
      multCheck h' hws

ftss k n = filter ((==) k . length . filter id) $ allFTssOf n

combinationNum r n = (product [1..n]) `div` (product [1..r] * product [1..n-r])
combinationLTnum r n = sum $ map (flip combinationNum n) [0..r]
