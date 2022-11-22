{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

-- stack ghci --ghc-options -w

module Main where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.Tuple
import Data.List

import Text.Printf

import Base
import Binomial
import Binary
import Counter
import Commander
import Product
import Approximate.Base
import Approximate.Encoding
import Approximate.Evaluation

reportConventionals :: KN -> IO ()
reportConventionals (k, n) = do
  putStrLn "binomial"; reportOf $ binomial id (literalXs n) k
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

{-
reportWith :: NumberConstraint -> Parameter -> Int -> IO ()
reportWith atMost param k' = do
  putStrLn ""
  let (k, n) = knOf param k'
  putStrLn $ "(k=" ++ show k ++ ",n=" ++ show n ++ ")"
  let lApprox = sum $ map length $ approxOrderWith atMost id param k'
  putStrLn $ "- approx(order): "   ++ (show lApprox)
  -- putStrLn $ "direct literals: "
  --   ++ (show $ sum $ map length $ approxDirectWith atMost id param k')
  let lCounter = sum $ map length $ counter id (literalXs n) k
  putStrLn $ "- counter: "   ++ (show lCounter)
  -- putStrLn $ "approx/counter: " ++ (take 5 $ show $ fromInteger (toInteger lApprox) / fromInteger (toInteger lCounter))
  let literalRate = fromInteger (toInteger lApprox) / fromInteger (toInteger lCounter) :: Float
  putStrLn $ "- approx/counter: " ++ (printf "%.8f" literalRate)
  forM_ [False, True] $ reportPR n literalRate
  where
  reportPR n literalRate just = do
    putStrLn $ if just then "- just" else "- overall"
    pRate <- if n <= 20
      then possibilityRate just param k'
      else randomRate just param k'
    let e = pRate / literalRate
    putStrLn $ " efficiency: " ++ (printf "%.8f" e)
    {-
    let a = accuracy param
    putStrLn $ "accuracy: " ++ (printf "%.8f" a)
    putStrLn $ "point(e*a): " ++ (printf "%.8f" $ e*a)
    -}
-}

theBestEfficienciesHalf :: Bool -> Int -> Int -> IO ()
theBestEfficienciesHalf just nMin nMax = forM_ [nMin..nMax] \n -> do
  let (k, n) = (n `div` 2, n)
      lCounter = sum $ map length $ counter id (literalXs n) k
  bestE <- theBestE just lCounter (k, n)
  putStrLn $ show (k, n) ++ ": " ++ show bestE

theBestE :: Bool -> Int -> KN -> IO (Float, ParamPlus)
theBestE just l kn = do
  let paramPluss = parametersFor kn
  effs <- forM paramPluss $ getEfficiency just l
  let effParamPluss = sort $ zip effs paramPluss
  return $ last effParamPluss

getEfficiency :: Bool -> Int -> ParamPlus -> IO Float
getEfficiency just l ((param, k'), (nFalse, nTrue)) = do
  let (k, n) = knOf param k'
      lApprox = sum (map length $ approxOrderWith binomial id param k') + nFalse + nTrue
      literalRate = fromInteger (toInteger lApprox) / fromInteger (toInteger l) :: Float
  pRate <- if n <= 20
    then possibilityRate just param k'
    else randomRate just param k'
  let e = pRate / literalRate
  putStrLn $ " " ++ show (param, k') ++ " -> " ++ (printf "%.8f" e)
  return e
