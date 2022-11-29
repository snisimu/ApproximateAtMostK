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

knOfTree :: ParameterTree -> Int -> KN
knOfTree (hws, m) k' = 
  let (h, w) = head hws
      (h', w') = last hws
      wAll = product $ map snd hws
      n = h' * m * wAll
  in  ((k'*n) `div` (h*w), n)

knOfSpace :: ParameterCNF -> KN
knOfSpace ((paramT, k'), (nFalse, nTrue)) = 
  let (k0, n0) = knOfTree paramT k'
  in  (k0 - nTrue, n0 - nFalse - nTrue)

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

solutionSpaceRatio :: Bool -> ParameterCNF -> IO Float
solutionSpaceRatio just paramCNF = do
  let iterationThreshold = 1000000
      ((paramT, k'), (nFalse, nTrue)) = paramCNF
      (k, n) = knOfSpace paramCNF
      nSpace = combinationNum just (toInteger k, toInteger n) :: Integer
  -- putStrLn $ " space size " ++ show (k, n) ++ ": " ++ show nSpace ++ " " -- [debug]
  if iterationThreshold < nSpace
    then solutionSpaceRatioInRandom just paramCNF
    else do
      let jss = if just
            then combinations [0..n-1] k
            else [] : concatMap (combinations [0..n-1]) [1..k]
          check jss = forM jss \js -> do
            bl <- isInTheSolutionSpace paramCNF js
            -- print (js, bl) -- [debug]
            return (js, bl)
      js'bls <- check jss
      let js'Trues = filter snd js'bls
          l = length js'bls
          lTrue = length js'Trues
      -- print "solutionSpaceRatio: done" -- [debug]
      return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)

solutionSpaceRatioInRandom :: Bool -> ParameterCNF -> IO Float
solutionSpaceRatioInRandom just paramCNF = do
  -- print "in randam" -- [debug]
  let nIteration = 1000 -- or 10000
  randomCheck just nIteration paramCNF
  js'bs <- (map (read :: String -> ([Int], Bool)) . lines) <$> readFile file
  let js'Trues = filter snd js'bs
      l = length js'bs
      lTrue = length js'Trues
  return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)
  where
    file = "SolutionSpaceRatioInRandom" </> show just ++ show paramCNF <.> "txt"
    randomCheck just nIteration paramCNF = do
      existFile <- doesFileExist file
      continue <- do
        if existFile
          then do
            l <- (length . lines) <$> readFile file
            return $ l < nIteration
          else return True
      when continue $ do
        let (k, n) = knOfSpace paramCNF
        jss <- if existFile
          then (map (fst . (read :: String -> ([Int], Bool))) . lines) <$> readFile file
          else return []
        let findJs just = do
              zeroOnes <- sequence $ replicate n $ random0toLT 2 :: IO [Int]
              let js = findIndices ((==) 1) zeroOnes
              if ((not just && length js <= k) || (just && length js == k)) && (not $ elem js jss)
                then return js
                else findJs just
        js <- findJs just
        bl <- isInTheSolutionSpace paramCNF js
        -- print (js, bl) -- [debug]
        appendFile file $ show (js, bl) ++ "\n"
        randomCheck just nIteration paramCNF
      where
        random0toLT n = newStdGen >>= \gen -> return $ fst (random gen) `mod` n

--

parameterTreesAt :: Int -> [ParameterTree]
parameterTreesAt n = 
  let atMostLimit = 20
      fs0s = factorss n
      fs1s = filter ((<=) 3 . length) fs0s
      fs2ss = nub $ concatMap permutations fs1s
      param0s = concatMap makeParams fs2ss
      param1s = flip filter param0s \(hws, m) -> 
        let nums = map (\(h,w) -> h*w) hws
            num = fst (last hws) * m
        in  and $ map ((>=) atMostLimit) $ num : nums
  -- forM_ params $ print . checkParameter
  in  param1s
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
            let k' = fst (knOfTree param i)
            in  k <= k' && k' <= k+d
      in  case dropWhile (not . inTheRange) [1..k] of
            [] -> Nothing
            k0 : _ ->
              let (k', n') = knOfTree param k0
                  nTrue = k' - k
                  nFalse = n' - n - nTrue
              in  Just ((param, k0), (nFalse, nTrue))
  where
  -- > mapM_ print $ parameterCNFsFor (4,12)

efficiency :: Bool -> Int -> Int -> (Int, ParameterCNF) -> IO Float
efficiency just nLiteralOther nParamCNFs (no, paramCNF) = do
  let ((paramT, k'), (nFalse, nTrue)) = paramCNF
  let (k, n) = knOfTree paramT k'
      lApprox = sum (map length $ approxOrderWith binomial id paramT k') + nFalse + nTrue
      literalRate = fromInteger (toInteger lApprox) / fromInteger (toInteger nLiteralOther) :: Float
  pRate <- solutionSpaceRatio just paramCNF
  -- print pRate -- [debug]
  -- print lApprox -- [debug]
  let e = pRate / literalRate
      strItem = show no ++ "/" ++ show nParamCNFs ++ " " ++ show just ++ " " ++ show paramCNF ++ " -> "
  putStrLn $ strItem ++ printf "%.8f" e
  -- appendFile "efficiency.log" $ strItem ++ show e ++ "\n"
  return e

theBestEfficiency :: Bool -> KN -> IO (Float, ParameterCNF)
theBestEfficiency just (k, n) = do
  let lCounter = sum $ map length $ counter id (literalXs n) k
      paramCNFs = parameterCNFsFor (k, n)
      nParamCNFs = length paramCNFs
  effs <- forM (zip [1..] paramCNFs) \iParamCNF -> do
    -- print iParamCNF -- [debug]
    efficiency just lCounter nParamCNFs iParamCNF
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
  putStrLn $ "\n" ++ show (k, n) ++ "\n" ++ show the ++ "\n"
  writeFile file $ unlines $ map show $
    knMbHds ++ [((k, n), Just the)] ++ knMbTls
  theBestEfficiencies

makeTheBestEfficienciesInit = do
  let f n = forM_ [2..n-2] \k ->
        putStrLn $ show $ (((k,n),Nothing) :: ((Int, Int), Maybe ((Float, ParameterCNF), (Float, ParameterCNF))))
  forM_ [10..20] f
  f 30
  f 50
  f 100
