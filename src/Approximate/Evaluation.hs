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

knOf :: Parameter -> Int -> KN
knOf (hws, m) k' = 
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  ((k'*n) `div` (h*w), n)

reportApproxWith :: NumberConstraint -> Parameter -> Int -> IO ()
reportApproxWith atMost param k' = do
  let (k, n) = knOf param k'
  putStrLn $ "(k=" ++ show k ++ ",n=" ++ show n ++ ")"
  putStrLn $ "order literals: "
    ++ (show $ sum $ map length $ approxOrderWith atMost id param k')
  -- putStrLn $ "direct literals: "
  --   ++ (show $ sum $ map length $ approxDirectWith atMost id param k')

isPossible :: Parameter -> Int -> [Int] -> IO Bool
isPossible (hws, m) k js = do
  let (h', w') = head hws
  unless (k < h'*w') $ die "k: too large"
  let h = fst $ last hws
      wAll = product $ map snd hws
      n = h*m * wAll
  unless (null $ filter (>= n) js) $ die $ "js: out of range: " ++ show js
  let bss = splitBy (h*m) $ foldr makeTrueAt (replicate n False) js
        where
        makeTrueAt a bs =
          let (b1s, _ : b2s) = splitAt a bs
          in  b1s ++ [True] ++ b2s
  -- print bss -- [debug]
  let divAlongUp x y =
        let (a, b) = x `divMod` y
        in  a + if b == 0 then 0 else 1
      integrate :: [Int] -> [HW] -> IO [Int]
      integrate ls = \case
        (_, _) : [] -> return $ map (flip divAlongUp m) ls
        (h', w') : (h, w) : hws -> do
          let wAll = product $ map snd $ (h, w) : hws
              l'ss = divideInto wAll ls
              lsNext = flip map l'ss \l's -> 
                divAlongUp (sum l's * h) (h' * w')
          integrate lsNext $ (h, w) : hws
  z <- sum <$> integrate (map (length . filter id) bss) (reverse hws)
  -- print z -- [debug]
  return $ z <= k

possibilityRate :: Parameter -> Int -> IO ()
possibilityRate param k' = do
  let (k, n) = knOf param k'
      jss = [] : concatMap (combinations [0..n-1]) [1..k]
      check jss = forM jss \js -> do
        bl <- isPossible param k' js
        return (js, bl)
  js'bls <- check jss
  let js'Trues = filter snd js'bls
      l = length js'bls
      lTrue = length js'Trues
  putStrLn $ "overall: " ++ show lTrue ++ "/" ++ show l ++ showPercentage lTrue l
  --
  let jss = combinations [0..n-1] k
      check jss = forM jss \js -> do
        bl <- isPossible param k' js
        return (js, bl)
  js'bls <- check jss
  let js'Trues = filter snd js'bls
      l = length js'bls
      lTrue = length js'Trues
  putStrLn $ "just: " ++ show lTrue ++ "/" ++ show l ++ showPercentage lTrue l

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

fileKNfor :: (Int, Int) -> Maybe String -> FilePath
fileKNfor (k, n) mbStr = "work" </> show k ++ "-" ++ show n ++ fromMaybe "" mbStr <.> "txt"

lengthOf :: (Int, Int) -> IO Int
lengthOf (k, n) = do
  let fileIn = fileKNfor (k, n) Nothing
  bl <- doesFileExist fileIn
  unless bl $ die "not exist"
  return . length . lines =<< readFile fileIn

-- procedure

makeInit d = do
  let n = 4 * 2^(d-1)
      k = n `div` 2
  writeFile (fileKNfor (k, n) Nothing) $ unlines $
    map (show . findIndices id) $ possible22bssJust d

dropOne :: KN -> IO ()
dropOne kn = do
  nFrom <- (length . lines) <$> readFile (fileKNfor kn Nothing)
  drOne 1 nFrom kn
  where
    drOne j nFrom (k, n) = when (j < nFrom) $ do
      putStrLn $ show j ++ "/" ++ show nFrom
      let fileFrom = fileKNfor (k, n) Nothing
          k' = k-1
          fileTo = fileKNfor (k', n) $ Just "drop"
      l <- (flip (!!) (j-1) . lines) <$> readFile fileFrom
      let is = (read :: String -> [Int]) l
          iss = flip map [0 .. length is - 1] \h ->
              let (i1s, _ : i2s) = splitAt h is
              in  i1s ++ i2s
      appendFile fileTo $ show iss ++ "\n"
      drOne (j+1) nFrom (k, n)

concatenation :: KN -> IO ()
concatenation kn = do
  let fileFrom = fileKNfor kn $ Just "drop"
      fileTo = fileKNfor kn $ Just "concat"
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

fileRCfor just param k' =
  let justOr = if just then "Just" else "Overall"
  in "work" </> "randomCheck" ++ justOr ++ show param ++ show k' <.> "txt"

randomCheck :: Bool -> Int -> Parameter -> Int -> IO ()
randomCheck just n param k' = sequence_ $ replicate n $ do
  let (k, n) = knOf param k'
      findLtK = do
        zeroOnes <- sequence $ replicate n $ random0toLT 2 :: IO [Int]
        let is = findIndices ((==) 1) zeroOnes
        if (just && length is == k) || (Prelude.not just && length is <= k)
          then return is
          else findLtK
  is <- findLtK
  bl <- isPossible param k' is
  -- print (is, bl) -- [debug]
  appendFile (fileRCfor just param k') $ show (is, bl) ++ "\n"
  where
    random0toLT n = newStdGen >>= \gen -> return $ fst (random gen) `mod` n

randomRate :: Bool -> Parameter -> Int -> IO ()
randomRate just param k' = do
  is'bs <- (nub . map (read :: String -> ([Int], Bool)) . lines) <$>
    readFile (fileRCfor just param k')
  let is'Trues = filter snd is'bs
      l = length is'bs
      lTrue = length is'Trues
  putStrLn $ show lTrue ++ "/" ++ show l ++ showPercentage lTrue l
