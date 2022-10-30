{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main, report, generateDIMACStoCheck) where

import System.Exit

import Control.Monad

import Data.List (elemIndex, intercalate)

import Base
-- import Binomial
-- import Binary
import Counter

report :: KN -> IO ()
report kn = do
  putStrLn $ "aux vars: " ++ show numAuxVars
  putStrLn $ "clauses : " ++ show numClauses
  putStrLn $ "literals: " ++ show numLiterals
  where
    numAuxVars = length $ auxVarsOf $ atMost kn
    numClauses = length $ atMost kn
    numLiterals = sum $ map length $ atMost kn

generateDIMACStoCheck :: KN -> IO ()
generateDIMACStoCheck (k, n) = do
  let cnf = atMost (k, n)
      auxVars = auxVarsOf cnf 
  vNumAtMostss <- forM cnf \bvs -> do
    forM bvs \(bl, var) -> do
      vNum <- case var of
        X m -> return m
        VarAux nv -> case elemIndex nv auxVars of
          Nothing -> die $ "cannot determine a number for auxVar: " ++ show nv
          Just index -> return $ n + index + 1
      return $ (if bl then 1 else -1) * vNum
  let vNumFixss bl = map return [1..(k + (if bl then 0 else 1))]
      vNumssToStr vNumss = unlines $ flip map vNumss \vNums ->
        intercalate " " $ map show $ vNums ++ [0]
  writeFile "ShouldBeSAT.cnf" $ vNumssToStr (vNumAtMostss ++ vNumFixss True)
  writeFile "ShouldBeUNSAT.cnf" $ vNumssToStr (vNumAtMostss ++ vNumFixss False)
  -- > wsl -- ./minisat ShouldBeSAT.cnf

main :: IO ()
main = do
  printCNF $ atMost (1, 3)
