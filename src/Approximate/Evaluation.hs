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

possibilityRate :: Parameter -> Int -> IO Float
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
  return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)

accuracy :: Parameter -> Float
accuracy (hws, m) =
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  fromInteger (toInteger $ h*w+1) / fromInteger (toInteger $ n+1)

-- in random

fileRCfor just param k' =
  let justOr = if just then "Just" else "Overall"
  in "work" </> "randomCheck" ++ justOr ++ show param ++ show k' <.> "txt"

randomRate :: Parameter -> Int -> IO Float
randomRate param k' = do
  rdRate True param k'
  rdRate False param k'
  where
    nIteration = 10000
    rdRate just param k' = do
      let file = fileRCfor just param k'
      bl <- doesFileExist file
      unless bl $ randomCheck just nIteration param k'
      is'bs <- (nub . map (read :: String -> ([Int], Bool)) . lines) <$> readFile file
      let is'Trues = filter snd is'bs
          l = length is'bs
          lTrue = length is'Trues
      putStrLn $ (if just then "just" else "overall") ++ "(randam): " ++
        show lTrue ++ "/" ++ show l ++ showPercentage lTrue l
      return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)
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

--

{-
parametersFor :: Int -> [Parameter]
parametersFor n =
  let fs0s = factorss n
      fs1s = filter ((<=) 3 . length) fs0s

      fs3s = nub $ concatMap permutations fs2s
      makeParams hws = \case
        m : [] -> (hws, m)
        a : b : cs -> makeParams (hws ++ [(a, b)]) cs
      param0s = map (makeParams []) fs3s
      -- check
  in  param0s
-}
{-
knOf (hws, m) k' = 
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
-}
