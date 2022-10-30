{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main, report, generateDIMACStoCheck) where

import System.Exit

import Control.Monad

import Data.List (elemIndex, intercalate)

import Base
import qualified Binomial
import qualified Binary
import qualified Counter
import qualified Commander

report :: KN -> IO ()
report kn = do
  putStrLn "Binomial"; reportOf $ Binomial.atMost kn
  putStrLn "Binary"; reportOf $ Binary.atMost kn
  putStrLn "Counter"; reportOf $ Counter.atMost kn
  where
    reportOf cnf = do
      putStrLn $ " aux vars: " ++ show (length $ auxVarsOf cnf)
      putStrLn $ " clauses : " ++ show (length cnf)
      putStrLn $ " literals: " ++ show (sum $ map length cnf)

generateDIMACStoCheck :: (Eq a, Show a) => (KN -> CNFwith a) -> KN -> IO ()
generateDIMACStoCheck atMst (k, n) = do
  let cnf = atMst (k, n)
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
  -- ghci> generateDIMACStoCheck Counter.atMost (5,10)
  -- PowerShell> wsl -- ./minisat ShouldBeSAT.cnf

main :: IO ()
main = do
  printCNF $ Commander.atMostOn 2 (1, 4)
