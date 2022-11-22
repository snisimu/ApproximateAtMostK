{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Evaluation where

import System.Exit
import System.Directory
import System.FilePath
import System.Random
import qualified System.IO.Strict

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List
import Data.Functor.Identity

import Text.Printf

import Base hiding (not)
import Binomial
import Counter

import Approximate.Base
import Approximate.Lib
import Approximate.Encoding

knOf :: ParameterTree -> Int -> KN
knOf (hws, m) k' = 
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  ((k'*n) `div` (h*w), n)

isInTheSolutionSpace :: ParameterCNF -> [Int] -> IO Bool
isInTheSolutionSpace (((hws, m), k'), (nFalse, nTrue)) js = do
  let (h', w') = head hws
  unless (k' < h'*w') $ die "k': too large"
  let h = fst $ last hws
      wAll = product $ map snd hws
      n0 = h*m * wAll
      n = n0 - nFalse - nTrue
  unless (null $ filter (>= n) js) $ die $ "js: out of range: " ++ show js
  let bss = splitBy (h*m) $ trueIndicesToBools n js
        ++ replicate nFalse False ++ replicate nTrue True
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
  return $ z <= k'

{-
solutionSpaceRatio :: Bool -> ParameterCNF -> IO Float
solutionSpaceRatio just paramCNF = do
  let threshold = 1000000
      ((paramT, k'), (nFalse, nTrue)) = paramCNF
      (k0, n0) = knOf paramT k'
  
  if threshold < combinationNum just (k, n)
    then solutionSpaceRatioInRandom just paramCNF
    else do
      let jss = if just
            then combinations [0..n-1] k
            else [] : concatMap (combinations [0..n-1]) [1..k]
          check jss = forM jss \js -> do
            bl <- isInTheSolutionSpace paramCNF k' js
            return (js, bl)
      js'bls <- check jss
      let js'Trues = filter snd js'bls
          l = length js'bls
          lTrue = length js'Trues
      return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)

solutionSpaceRatioInRandom :: Bool -> ParameterCNF -> IO Float
solutionSpaceRatioInRandom just paramCNF = do
  let nIteration = 1000 -- or 10000
  bl <- doesFileExist file
  unless bl $ randomCheck just nIteration paramCNF k'
  is'bs <- (nub . map (read :: String -> ([Int], Bool)) . lines) <$> readFile file
  let is'Trues = filter snd is'bs
      l = length is'bs
      lTrue = length is'Trues
  -- putStrLn $ " possibility rate(randam): " ++
  --   show lTrue ++ "/" ++ show l ++ showPercentage lTrue l
  return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)
  where
    file =
      let justOr = if just then "Just" else "Overall"
      in "work" </> "randomCheck" ++ justOr ++ show paramCNF ++ show k' <.> "txt"
    randomCheck just nIteration paramCNF = forM_ [1..nIteration] \j -> do
      -- putStr $ show j ++ if j == nIteration then "\n" else " "
      let (k, n) = knOf paramCNF k'
          findLtK = do
            zeroOnes <- sequence $ replicate n $ random0toLT 2 :: IO [Int]
            let is = findIndices ((==) 1) zeroOnes
            if length is <= k
              then return is
              else findLtK
      is <- if k == 1 
              then do
                i1s <- return <$> random0toLT n
                return $ [] : i1s
              else if just
                then randomChoice $ combinations [0..n-1] k
                else findLtK
      bl <- isInTheSolutionSpace paramCNF k' is
      -- print (is, bl) -- [debug]
      appendFile file $ show (is, bl) ++ "\n"
      where
        random0toLT n = newStdGen >>= \gen -> return $ fst (random gen) `mod` n

--

parameterTreesAt :: Int -> [ParameterTree]
parameterTreesAt n = 
  let fs0s = factorss n
      fs1s = filter ((<=) 3 . length) fs0s
      fs2ss = nub $ concatMap permutations fs1s
      params = concatMap makeParams fs2ss
  -- forM_ params $ print . checkParameter
  in  params
  where
    makeParams :: [Int] -> [ParameterTree]
    makeParams (m : hn : wn : ws) = mkParams [[(hn, wn)]] (hn*wn) ws
      where
      mkParams hwss hw = \case
        [] -> map (\hws -> (hws, m)) hwss
        w : ws ->
          let hs = [ a | a <- [2..hw-1], hw `mod` a == 0 ]
          in  concatMap (\h -> mkParams (map ((:) (h, w)) hwss) (h*w) ws) hs

parameterCNFsFor :: KN -> [ParameterCNF]
parameterCNFsFor (k, n) = 
  flip concatMap [0..n-1] \d ->
    catMaybes $ flip map (parameterTreesAt $ n+d) \param ->
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
  -- > mapM_ print $ parameterCNFsFor (4,12)

efficiency :: Bool -> Int -> ParameterCNF -> IO Float
efficiency just l ((param, k'), (nFalse, nTrue)) = do
  let (k, n) = knOf param k'
      lApprox = sum (map length $ approxOrderWith binomial id param k') + nFalse + nTrue
      literalRate = fromInteger (toInteger lApprox) / fromInteger (toInteger l) :: Float
  putStr $ " " ++ show (param, k') ++ " -> "
  pRate <- solutionSpaceRatio just param k'
  let e = pRate / literalRate
  putStrLn $ printf "%.8f" e
  return e

theBestEfficiency :: Bool -> KN -> IO (Float, ParameterCNF)
theBestEfficiency just (k, n) = do
  let lCounter = sum $ map length $ counter id (literalXs n) k
      paramCNFs = parameterCNFsFor (k, n)
  effs <- forM paramCNFs $ efficiency just lCounter
  let effParamPluss = sort $ zip effs paramCNFs
  return $ last effParamPluss

theBestEfficiencies :: IO ()
theBestEfficiencies = do
  let file = "TheBestEfficiencies.txt"
  knMbs <- (map (read :: String -> ((Int, Int), Maybe ((Float, ParameterCNF), (Float, ParameterCNF)))) . lines) <$>
    System.IO.Strict.readFile file
  let (knMbHds, ((k, n), _) : knMbTls) = break (isNothing . snd) knMbs
  eParamPlusOverall <- theBestEfficiency False (k, n)
  eParamPlusJust <- theBestEfficiency True (k, n)
  let the = (eParamPlusOverall, eParamPlusJust)
  putStrLn $ "\n" ++ show the ++ "\n"
  writeFile file $ unlines $ map show $
    knMbHds ++ [((k, n), Just the)] ++ knMbTls
  theBestEfficiencies
-}