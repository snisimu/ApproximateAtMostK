{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.List (elemIndex, intercalate)

import Base
import Binomial
import Binary
import Counter
import Commander
import Product
import Approximate

report :: KN -> IO ()
report (k, n) = do
  putStrLn "binomial"; reportOf $ binomial (literalXs n) k
  putStrLn "binary"; reportOf $ binary (literalXs n) k
  putStrLn "counter"; reportOf $ counter (literalXs n) k
  putStrLn "commander"; reportOf $ commander (literalXs n) k
  putStrLn "product"; reportOf $ product (literalXs n) k
  where
    reportOf cnf = do
      putStrLn $ " aux vars: " ++ show (length $ auxsOf cnf)
      putStrLn $ " clauses : " ++ show (length cnf)
      putStrLn $ " literals: " ++ show (sum $ map length cnf)

vNumssToStr vNumss = unlines $ flip map vNumss \vNums ->
  intercalate " " $ map show $ vNums ++ [0]

genDIMACS :: CNF -> IO String
genDIMACS cnf = do
  let n = length $ xsOf cnf
      auxs = auxsOf cnf
  vNumssToStr <$> forM cnf \literals -> do
    forM literals \(bl, var) -> do
      vNum <- case var of
        X i -> return i
        v -> case elemIndex v auxs of
          Nothing -> die $ "cannot determine a number for aux var: " ++ show v
          Just index -> return $ n + index + 1
      return $ (if bl then 1 else -1) * vNum

generateDIMACS :: CNF -> IO ()
generateDIMACS cnf = writeFile "the.cnf" =<< genDIMACS cnf

generateDIMACStoCheck :: NumberConstraint -> KN -> IO ()
generateDIMACStoCheck atMost (k, n) = do
  strAtMost <- genDIMACS $ atMost (literalXs n) k
  let vNumFixss bl = map return [1..(k + (if bl then 0 else 1))]
  writeFile "ShouldBeSAT.cnf" $ strAtMost ++ vNumssToStr (vNumFixss True)
  writeFile "ShouldBeUNSAT.cnf" $ strAtMost ++ vNumssToStr (vNumFixss False)
  -- ghci> generateDIMACStoCheck Counter.atMost (5,10)
  -- PowerShell> wsl -- ./minisat ShouldBeSAT.cnf

check :: NumberConstraint -> KN -> IO ()
check atMost (k, n) = printCNF $ atMost (literalXs n) k
-- > check commander (5,10)

main :: IO ()
main = return ()
