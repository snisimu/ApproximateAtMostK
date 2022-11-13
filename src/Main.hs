{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

-- stack ghci --ghc-options -w

module Main where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.List

import Base
import Binomial
import Binary
import Counter
import Commander
import Product
import Approximate.Encoding
import Approximate.Evaluation

report :: KN -> IO ()
report (k, n) = do
  putStrLn "binomial"; reportOf $ binomial id (literalXs n) k
  putStrLn "binary"; reportOf $ binary id (literalXs n) k
  putStrLn "counter"; reportOf $ counter id (literalXs n) k
  putStrLn "commander(+binomial)"; reportOf $ commanderWith binomial id (literalXs n) k
  putStrLn "product(+binomial)"; reportOf $ productWith binomial id (literalXs n) k

strDIMACSwithTrue :: CNF -> [Int] -> IO String
strDIMACSwithTrue cnf ts = do
  let n = length $ xsOf cnf
      auxs = auxsOf cnf
  when (n == 0 && ts /= []) $ die "has no Xs"
  vNumAtMostss <- forM cnf \literals -> do
    forM literals \(bl, var) -> do
      vNum <- case var of
        X i -> return i
        v -> case elemIndex v auxs of
          Nothing -> die $ "cannot determine a number for aux var: " ++ show v
          Just index -> return $ n + index + 1
      return $ (if bl then 1 else -1) * vNum
  let vNumTruess = map return ts
  return $ vNumssToStr $ vNumAtMostss ++ vNumTruess
  where
    vNumssToStr vNumss = unlines $ flip map vNumss \vNums ->
      intercalate " " $ map show $ vNums ++ [0]

generateDIMACSwithTrue :: CNF -> [Int] -> IO ()
generateDIMACSwithTrue cnf ts = writeFile "the.cnf" =<< strDIMACSwithTrue cnf ts
  -- > wsl -- ./minisat the.cnf

generateDIMACStoCheck :: NumberConstraint -> KN -> IO ()
generateDIMACStoCheck atMost (k, n) = do
  let strDIMACS bl = strDIMACSwithTrue (atMost id (literalXs n) k) [1 .. k + (if bl then 0 else 1)]
  writeFile "ShouldBeSAT.cnf" =<< strDIMACS True
  writeFile "ShouldBeUNSAT.cnf" =<< strDIMACS False
  -- > wsl -- ./minisat ShouldBeSAT.cnf

check :: NumberConstraint -> KN -> IO ()
check atMost (k, n) = printCNF $ atMost id (literalXs n) k
-- > check commander (5,10)

main :: IO ()
main = return ()
