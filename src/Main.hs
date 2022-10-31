{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main, report, generateDIMACStoCheck) where

import System.Exit

import Control.Monad

import Data.List (elemIndex, intercalate)

import Base
import Binomial
import Binary
import Counter
-- import Commander

report :: KN -> IO ()
report (k, n) = do
  putStrLn "Binomial"; reportOf $ binomial (literalXs n) k
  putStrLn "Binary"; reportOf $ binary (literalXs n) k
  putStrLn "Counter"; reportOf $ counter (literalXs n) k
  where
    reportOf cnf = do
      putStrLn $ " aux vars: " ++ show (length $ auxsOf cnf)
      putStrLn $ " clauses : " ++ show (length cnf)
      putStrLn $ " literals: " ++ show (sum $ map length cnf)

generateDIMACStoCheck :: (Eq a, Show a) => NumberConstraint VarX a -> KN -> IO ()
generateDIMACStoCheck atMost (k, n) = do
  let cnf = atMost (literalXs n) k
      auxs = auxsOf cnf 
  vNumAtMostss <- forM cnf \literals -> do
    forM literals \(bl, var) -> do
      vNum <- case var of
        Left (X m) -> return m
        Right v -> case elemIndex v auxs of
          Nothing -> die $ "cannot determine a number for auxVar: " ++ show v
          Just index -> return $ n + index + 1
      return $ (if bl then 1 else -1) * vNum
  let vNumFixss bl = map return [1..(k + (if bl then 0 else 1))]
      vNumssToStr vNumss = unlines $ flip map vNumss \vNums ->
        intercalate " " $ map show $ vNums ++ [0]
  writeFile "ShouldBeSAT.cnf" $ vNumssToStr (vNumAtMostss ++ vNumFixss True)
  writeFile "ShouldBeUNSAT.cnf" $ vNumssToStr (vNumAtMostss ++ vNumFixss False)
  -- ghci> generateDIMACStoCheck Counter.atMost (5,10)
  -- PowerShell> wsl -- ./minisat ShouldBeSAT.cnf

main :: IO ()
main = do
  printCNF $ binary (literalXs 3) 1
