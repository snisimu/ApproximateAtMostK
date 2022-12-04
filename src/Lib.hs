{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Lib where

import Prelude hiding (not)
import qualified Prelude (not)

import System.Exit

import Control.Monad

import Data.List

import Base

atLeastBy :: NumberConstraint -> NumberConstraint
atLeastBy atMost vScope literals k
  = atMost vScope (map not literals) (length literals - k)

reportOf :: CNF -> IO ()
reportOf cnf = do
  putStrLn $ "aux vars: " ++ show (length $ auxsOf cnf)
  putStrLn $ "clauses : " ++ show (length cnf)
  putStrLn $ "literals: " ++ show (sum $ map length cnf)

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

-- for approximate

checkParameter :: ParameterTree -> Bool
checkParameter = (checkParam <$> fst . head <*> tail) . fst
  where
  checkParam :: Height -> [HW] -> Bool
  checkParam h = \case
    [] -> True
    (h', w') : hws -> if (h'*w') `mod` h /= 0
      then False
      else checkParam h' hws

ftss k n = filter ((==) k . length . filter id) $ allCombinationssOf [False, True] n

labeling :: [HW] -> [([Int], Height)]
labeling = tail . concat . foldl makeH'Isss [[([], 0)]]
  where
  makeH'Isss ishss (h, w) =
    ishss ++ [[ (is ++ [i], h) | (is, _) <- last ishss, i <- [1..w] ]]

trueIndicesToBools :: Int -> [Int] -> [Bool]
trueIndicesToBools n = foldr makeTrueAt (replicate n False)
  where
  makeTrueAt a bs =
    let (b1s, _ : b2s) = splitAt a bs
    in  b1s ++ [True] ++ b2s

accuracy :: ParameterTree -> Float
accuracy (hws, m) =
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  fromInteger (toInteger $ h*w+1) / fromInteger (toInteger $ n+1)

knOfTree :: (ParameterTree, Int) -> KN
knOfTree ((hws, m), k') = 
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  ((k'*n) `div` (h*w), n)

knOfSpace :: ParameterCNF -> KN
knOfSpace (paramTk', (nFalse, nTrue)) = 
  let (k0, n0) = knOfTree paramTk'
  in  (k0 - nTrue, n0 - nFalse - nTrue)
