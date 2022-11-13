{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Evaluation where

import Prelude hiding (not)

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

import Data.Maybe
import Data.List

import Base
import Binomial

import Approximate.Base
import Approximate.Lib
import Approximate.Encoding

isPossible :: [HW] -> Int -> [Int] -> IO Bool
isPossible hws k js = do
  let (h', w') = head hws
  unless (k < h'*w') $ die "k: too large"
  let (h, w) = last hws
      m = product $ map snd $ init hws
      n = h * w * m
  unless (null $ filter (> n) js) $ die "js: out of range"
  let bss = splitBy h $ foldr makeTrueAt (replicate n False) js
        where
        makeTrueAt m bs =
          let (hd, tl) = splitAt m bs
          in  init hd ++ [True] ++ tl
      integrate :: [Int] -> [HW] -> IO [Int]
      integrate ls = \case
        hw : [] -> return ls
        (h', w') : (h, w) : hws -> do
          let m = product $ map snd $ (h, w) : hws
              l'ss = divideInto m ls
              lsNext = flip map l'ss \l's -> 
                let (a, b) = (sum l's * h) `divMod` (h' * w')
                in  a + if b == 0 then 0 else 1
          integrate lsNext $ (h, w) : hws
  z <- sum <$> integrate (map (length . filter id) bss) (reverse hws)
  -- print z -- [debug]
  return $ z <= k

knOf :: [HW] -> Int -> KN
knOf hws k' = 
  let (h, w) = last hws
      m = product $ map snd $ init hws
  in  (k' * m, h * w * m)

reportApproxWith :: NumberConstraint -> [HW] -> Int -> IO ()
reportApproxWith atMost hws k' = do
  let (k, n) = knOf hws k'
  putStrLn $ "(k=" ++ show k ++ ",n=" ++ show n ++ ")"
  reportOf $ approx atMost id hws k'

possibilityRate :: [HW] -> Int -> IO ()
possibilityRate hws k' = do
  let (k, n) = knOf hws k'
      jsKs = combinations [1..n] k
      -- jsLtKs = [] : concatMap (combinations [1..n]) [1..k-1]
      check jss = forM jss \js -> do
        bl <- isPossible hws k' js
        return (js, bl)
  jsK'bls <- check jsKs
  -- jsLtK'bls <- check jsLtKs
  let jsK'Trues = filter snd jsK'bls
      -- jsLtK'Trues = filter snd jsLtK'bls
      lK = length jsK'bls
      lKTrue = length jsK'Trues
  putStrLn $ show lKTrue ++ "/" ++ show lK ++ showPercentage lKTrue lK
  -- print js'rPoss; print js'rs -- [debug]
  -- > reportOf $ counter (literalXs 20) 10

{-
byIsPossible :: [HW] -> Int -> IO [[Bool]]
byIsPossible hws k' = do
  let (k, n) = knOf hws k'
      jsKs = combinations [1..n] k
      check jss = forM jss \js -> do
        bl <- isPossible hws k' js
        return (js, bl)
  jsK'bls <- check jsKs
  let jsK'Trues = filter snd jsK'bls
      jss = map fst jsK'Trues
      makeTrueAt m bs =
        let (hd, tl) = splitAt m bs
        in  init hd ++ [True] ++ tl
  return $ map (foldr makeTrueAt (replicate n False)) jss
-}

possible22bssJust :: Int -> [[Bool]]
possible22bssJust d = pble22 d 1 2
  where
  (+++) a b = (++) <$> a <*> b
  pble22 d d' = \case
    0 -> pb22 0 +++ pb22 0
    1 -> pb22 1 +++ pb22 0
          ++ pb22 0 +++ pb22 1
    2 -> pb22 2 +++ pb22 0
          ++ pb22 1 +++ pb22 1
          ++ pb22 0 +++ pb22 2
    3 -> pb22 2 +++ pb22 1
          ++ pb22 1 +++ pb22 2
    4 -> pb22 2 +++ pb22 2
   where
    pb22 k = if d == d'
      then ftss k 2
      else pble22 d (d'+1) (k*2)

-- writeFile "work/4-8.hs" $ unlines $ map (show . findIndices id) $ possible22bssJust 2

fileFor :: (Int, Int) -> FilePath
fileFor (k, n) = "work" </> show k ++ "-" ++ show n <.> "hs"

lengthOf :: (Int, Int) -> IO ()
lengthOf (k, n) = do
  let fileIn = fileFor (k, n)
  bl <- doesFileExist fileIn
  unless bl $ die "not exist"
  bss <- (read :: String -> [[Bool]]) <$> readFile fileIn
  print $ length bss

{-
writeNext :: (Int, Int) -> IO ()
writeNext (k, n) = do
  let fileIn = fileFor (k, n)
  bl <- doesFileExist fileIn
  unless bl $ die $ "not exist: " ++ fileIn
  str <- readFile fileIn
  let isInits = (read :: String -> [[Int]]) $
        concat $ "[" ++ intercalate "," (lines str) ++ "]"
  let nInit = length isInits
      dropOne :: [[Int]] -> (Int, [Int]) -> IO [[Int]]
      dropOne iss (j, is) = do
        putStrLn $ show (k, n) ++ " -> " ++ show (k-1, n) ++ ": "
          ++ show j ++ "/" ++ show nInit
        let droppeds = flip map [0 .. length is] \h ->
              let (bHs, _ : bTs) = splitAt h bs
              in  bHs ++ bTs
            toAdds = catMaybes $ flip map droppeds \drs ->
              if elem drs bss then Nothing else Just drs
        return $ bss ++ toAdds
  bs's <- foldM dropOne [] (zip [1..] isInits)
  writeFile (fileFor (k-1, n)) $ show bs's

startCalc :: Int -> IO ()
startCalc d = do
  let n = 4 * 2^(d-1)
      k = n `div` 2
  writeFile (fileFor (k, n)) $ show $ possible22bssJust d
  forM_ (reverse [1..k]) \k' -> writeNext (k', n)
-}
