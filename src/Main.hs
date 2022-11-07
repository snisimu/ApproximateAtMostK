{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main, report, generateDIMACStoCheck) where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.List (elemIndex, intercalate)

import Base
import Binomial
import Binary
import Counter
import Commander
-- import Product

report :: KN -> IO ()
report (k, n) = do
  putStrLn "binomial"; reportOf $ binomial (literalXs n) k
  putStrLn "binary"; reportOf $ binary (literalXs n) k
  putStrLn "counter"; reportOf $ counter (literalXs n) k
  -- putStrLn "commander(+counter)"; reportOf $ commander counter s (literalXs n) k
  -- putStrLn "product"; reportOf $ product (literalXs n) k
  where
    reportOf cnf = do
      putStrLn $ " aux vars: " ++ show (length $ auxsOf cnf)
      putStrLn $ " clauses : " ++ show (length cnf)
      putStrLn $ " literals: " ++ show (sum $ map length cnf)

generateDIMACStoCheck :: NumberConstraint -> KN -> IO ()
generateDIMACStoCheck atMost (k, n) = do
  let cnf = atMost (literalXs n) k
      auxs = auxsOf cnf
  vNumAtMostss <- forM cnf \literals -> do
    forM literals \(bl, var) -> do
      vNum <- case var of
        X i -> return i
        v -> case elemIndex v auxs of
          Nothing -> die $ "cannot determine a number for aux var: " ++ show v
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
main = return ()

-- > printCNF $ commander (literalXs 3) 1
-- > printCNF $ commander (literalXs 12) 3
