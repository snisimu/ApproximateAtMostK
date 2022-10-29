{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import System.Exit

import Control.Monad

import Data.Maybe
import Data.List

import Base
-- import Binomial
import Binary

report :: KN -> IO ()
report kn = do
  putStrLn $ "numNewVars: " ++ show (numNewVars kn)
  putStrLn $ "numClauses: " ++ show (numClauses kn)
  putStrLn $ "numLiterals: " ++ show (numLiterals kn)
  where
    numNewVars kn = length $ newVarsOf $ atMost kn
    numClauses kn = length $ atMost kn
    numLiterals kn = sum $ map length $ atMost kn

newVarsOf :: Eq a => CNF a -> [a]
newVarsOf cnf = nub $ flip concatMap cnf \bvs ->
  catMaybes $ flip map bvs \case
    (_, VarNew v) -> Just v
    _ -> Nothing

generateDIMACStoCheck :: KN -> IO ()
generateDIMACStoCheck (k, n) = do
  let cnf = atMost (k, n)
      newVars = newVarsOf cnf 
  vNumAtMostss <- forM cnf \bvs -> do
    forM bvs \(bl, var) -> do
      vNum <- case var of
        X m -> return m
        VarNew nv -> case elemIndex nv newVars of
          Nothing -> die $ "cannot determine a number for newVar: " ++ show nv
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
  mapM_ print $ atMost (1, 3)
