{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.Maybe
import Data.Tuple
import Data.List

import Base
import Lib
import Conventional.Binomial
import Conventional.Binary
import Conventional.Counter
import Conventional.Commander
import Conventional.Product
import Encoding
import Evaluation
import Problem
import Statistics

reportConventionals :: KN -> IO ()
reportConventionals (k, n) = do
  -- putStrLn "binomial"; reportOf $ binomial id (literalXs n) k
  putStrLn "binary"; reportOf $ binary id (literalXs n) k
  putStrLn "counter"; reportOf $ counter id (literalXs n) k
  putStrLn "commander(+counter)"; reportOf $ commanderWith counter id (literalXs n) k
  putStrLn "product(+counter)"; reportOf $ productWith counter id (literalXs n) k

reportLiterals :: KN -> IO ()
reportLiterals (k, n) = do
  -- putStrLn $ "binomial: " ++ (show $ sum $ map length $ binomial id (literalXs n) k)
  putStrLn $ "binary: " ++ (show $ sum $ map length $ binary id (literalXs n) k)
  putStrLn $ "counter: " ++ (show $ sum $ map length $ counter id (literalXs n) k)
  -- putStrLn $ "commander(+counter): " ++ (show $ sum $ map length $ commanderWith counter id (literalXs n) k)
  -- putStrLn $ "product(+counter): " ++ (show $ sum $ map length $ productWith counter id (literalXs n) k)

main :: IO ()
main = do
  writeProblem ((([(2,2),(2,3)],2),2),(2,2)) Nothing
  -- writeProblems
  -- theBestEfficiencies
