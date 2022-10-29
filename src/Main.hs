{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import System.Exit

import Control.Monad

import Data.List

import Base
import Pairwise

report :: KN -> IO ()
report kn = do
  putStrLn $ "numNewVars: " ++ show (numNewVars kn)
  putStrLn $ "numLiterals: " ++ show (numLiterals kn)
  putStrLn $ "numClauses: " ++ show (numClauses kn)
  where
    numNewVars kn = length $ newVars $ atMost kn
    numLiterals kn = sum $ map length $ cnf $ atMost kn
    numClauses kn = length $ cnf $ atMost kn

generateDIMACStoCheck :: KN -> IO ()
generateDIMACStoCheck (k, n) = do
  let Result newVar's cnf' = atMost (k, n)
  vNumAtMostss <- forM cnf' \bvs -> do
    forM bvs \(bl, var) -> do
      vNum <- case var of
        X m -> return m
        New v -> case elemIndex v newVar's of
          Nothing -> die $ "cannot determine a number for newVar: " ++ show v
          Just index -> return $ n + index + 1
      return $ (if bl then 1 else -1) * vNum
  let vNumFixss bl = map return [1..(k + (if bl then 0 else 1))]
      vNumssToStr vNumss = unlines $ flip map vNumss \vNums ->
        concat $ intersperse " " $ map show $ vNums ++ [0]
  writeFile "ShouldBeSAT.cnf" $ vNumssToStr (vNumAtMostss ++ vNumFixss True)
  writeFile "ShouldBeUNSAT.cnf" $ vNumssToStr (vNumAtMostss ++ vNumFixss False)
  -- > wsl -- ./minisat ShouldBeSAT.cnf

main :: IO ()
main = do
  print $ cnf $ atMost (2, 4)
