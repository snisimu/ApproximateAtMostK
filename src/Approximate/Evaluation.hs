{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Evaluation where

import Prelude hiding (not)
import qualified Prelude (not)

import System.Exit
import System.Directory
import System.FilePath
import System.Random

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
  unless (null $ filter (>= n) js) $ die "js: out of range"
  let bss = splitBy h $ foldr makeTrueAt (replicate n False) js
        where
        makeTrueAt m bs =
          let (b1s, _ : b2s) = splitAt m bs
          in  b1s ++ [True] ++ b2s
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
      jss = [] : concatMap (combinations [1..n]) [1..k]
      check jss = forM jss \js -> do
        bl <- isPossible hws k' js
        return (js, bl)
  js'bls <- check jss
  let js'Trues = filter snd js'bls
      l = length js'bls
      lTrue = length js'Trues
  putStrLn $ show lTrue ++ "/" ++ show l ++ showPercentage lTrue l

-- for [(2,2)..]

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

fileFor :: (Int, Int) -> Maybe String -> FilePath
fileFor (k, n) mbStr = "work" </> show k ++ "-" ++ show n ++ fromMaybe "" mbStr <.> "txt"

lengthOf :: (Int, Int) -> IO Int
lengthOf (k, n) = do
  let fileIn = fileFor (k, n) Nothing
  bl <- doesFileExist fileIn
  unless bl $ die "not exist"
  return . length . lines =<< readFile fileIn

-- procedure

makeInit d = do
  let n = 4 * 2^(d-1)
      k = n `div` 2
  writeFile (fileFor (k, n) Nothing) $ unlines $
    map (show . findIndices id) $ possible22bssJust d

dropOne :: KN -> IO ()
dropOne kn = do
  nFrom <- (length . lines) <$> readFile (fileFor kn Nothing)
  drOne 1 nFrom kn
  where
    drOne j nFrom (k, n) = when (j < nFrom) $ do
      putStrLn $ show j ++ "/" ++ show nFrom
      let fileFrom = fileFor (k, n) Nothing
          k' = k-1
          fileTo = fileFor (k', n) $ Just "drop"
      l <- (flip (!!) (j-1) . lines) <$> readFile fileFrom
      let is = (read :: String -> [Int]) l
          iss = flip map [0 .. length is - 1] \h ->
              let (i1s, _ : i2s) = splitAt h is
              in  i1s ++ i2s
      appendFile fileTo $ show iss ++ "\n"
      drOne (j+1) nFrom (k, n)

concatenation :: KN -> IO ()
concatenation kn = do
  let fileFrom = fileFor kn $ Just "drop"
      fileTo = fileFor kn $ Just "concat"
  ls <- lines <$> readFile fileFrom
  let nFrom = length ls
  forM_ [1..nFrom] \j -> do
    putStrLn $ show j ++ "/" ++ show nFrom
    let l = ls !! (j-1)
        iss = (read :: String -> [[Int]]) l
    forM_ iss \is -> appendFile fileTo $ show is ++ "\n"

-- Linux> sort -u K-Nconcat.txt > K-N.txt

total :: KN -> IO ()
total (k, n) = do
  ls <- forM [0..k] \i -> lengthOf (i, n)
  print $ sum ls

-- in random

randomCheck :: Int -> [HW] -> Int -> IO ()
randomCheck m hws k' = sequence_ $ replicate m $ do
  let (k, n) = knOf hws k'
      file = "work" </> "randomCheck" ++ show hws ++ show k' <.> "txt"
      findLtK = do
        j <- random0toLT $ 2 ^ n
        let zeroOnes = reverse $ map (\h -> (j `div` 2^(h-1)) `mod` 2) [1..n] :: [Int]
            is = findIndices ((==) 1) zeroOnes
        if length is <= k
          then return is
          else findLtK
  is <- findLtK
  bl <- isPossible hws k' is
  print (is, bl) -- [debug]
  -- appendFile file $ show $ (is, bl)
  where
    random0toLT n = newStdGen >>= \gen -> return $ fst (random gen) `mod` n
