{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Evaluation where

import System.Exit
import System.Directory
import System.FilePath
import System.Random

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List
import Data.Functor.Identity

import Base hiding (not)
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
  let bss = splitBy (h*m) $ trueIndicesToBools n js
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

possibilityRate :: Bool -> Parameter -> Int -> IO Float
possibilityRate just param k' = do
  let (k, n) = knOf param k'
      jss = if just
        then combinations [0..n-1] k
        else [] : concatMap (combinations [0..n-1]) [1..k]
      check jss = forM jss \js -> do
        bl <- isPossible param k' js
        return (js, bl)
  js'bls <- check jss
  let js'Trues = filter snd js'bls
      l = length js'bls
      lTrue = length js'Trues
  -- putStrLn $ " possibility rate: " ++ show lTrue ++ "/" ++ show l ++ showPercentage lTrue l
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

randomRate :: Bool -> Parameter -> Int -> IO Float
randomRate just param k' = do
  let nIteration = 1000 -- or 10000
      file = fileRCfor just param k'
  bl <- doesFileExist file
  unless bl $ randomCheck just nIteration param k'
  is'bs <- (nub . map (read :: String -> ([Int], Bool)) . lines) <$> readFile file
  let is'Trues = filter snd is'bs
      l = length is'bs
      lTrue = length is'Trues
  -- putStrLn $ " possibility rate(randam): " ++
  --   show lTrue ++ "/" ++ show l ++ showPercentage lTrue l
  return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)
  where
    randomCheck just nIteration param k' = forM_ [1..nIteration] \j -> do
      -- putStr $ show j ++ if j == nIteration then "\n" else " "
      let (k, n) = knOf param k'
          findLtK = do
            zeroOnes <- sequence $ replicate n $ random0toLT 2 :: IO [Int]
            let is = findIndices ((==) 1) zeroOnes
            if length is <= k
              then return is
              else findLtK
      is <- if k == 1 
              then return <$> random0toLT n
              else if just
                then randomChoice $ combinations [0..n-1] k
                else findLtK
      bl <- isPossible param k' is
      -- print (is, bl) -- [debug]
      appendFile (fileRCfor just param k') $ show (is, bl) ++ "\n"
      where
        random0toLT n = newStdGen >>= \gen -> return $ fst (random gen) `mod` n

--

parametersAt :: Int -> [Parameter]
parametersAt n = 
  let fs0s = factorss n
      fs1s = filter ((<=) 3 . length) fs0s
      fs2ss = nub $ concatMap permutations fs1s
      params = concatMap makeParams fs2ss
  -- forM_ params $ print . checkParameter
  in  params
  where
    makeParams :: [Int] -> [Parameter]
    makeParams (m : hn : wn : ws) = mkParams [[(hn, wn)]] (hn*wn) ws
      where
      mkParams hwss hw = \case
        [] -> map (\hws -> (hws, m)) hwss
        w : ws ->
          let hs = [ a | a <- [2..hw-1], hw `mod` a == 0 ]
          in  concatMap (\h -> mkParams (map ((:) (h, w)) hwss) (h*w) ws) hs

type ParamPlus = ((Parameter, Int), (Int, Int))

parametersFor :: KN -> [ParamPlus]
parametersFor (k, n) = 
  flip concatMap [0..n-1] \d ->
    catMaybes $ flip map (parametersAt $ n+d) \param ->
      let inTheRange i = 
            let k' = fst (knOf param i)
            in  k <= k' && k' <= k+d
      in  case dropWhile (not . inTheRange) [1..k] of
            [] -> Nothing
            k0 : _ ->
              let (k', n') = knOf param k0
                  nTrue = k' - k
                  nFalse = n' - n - nTrue
              in  Just ((param, k0), (nFalse, nTrue))
  where
  -- > mapM_ print $ parametersFor (4,12)
