{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Evaluation where

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
import Lib
import Conventional.Binomial
import Conventional.Counter
import Encoding

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

solutionSpaceRate :: Bool -> Either Bool Int -> ParameterCNF -> IO Float
solutionSpaceRate debug eth paramCNF = do
  let iterationThreshold = 1000000
      (k0, n) = knOfSpace paramCNF
      (k, just) = case eth of
        Left bl -> (k0, bl)
        Right k -> (k, True)
      nSpace = combinationNum just (toInteger k, toInteger n) :: Integer
  when debug $
    putStrLn $ " space size " ++ show (k, n) ++ ": " ++ show nSpace ++ " "
  if iterationThreshold < nSpace
    then solutionSpaceRateInRandom debug just paramCNF
    else do
      let jss = if just
            then combinations [0..n-1] k
            else [] : concatMap (combinations [0..n-1]) [1..k]
          check jss = forM jss \js -> do
            bl <- isInTheSolutionSpace paramCNF js
            -- when debug $ print (js, bl) -- [debug]
            return (js, bl)
      js'bls <- check jss
      let js'Trues = filter snd js'bls
          l = length js'bls
          lTrue = length js'Trues
      when debug $ print "solutionSpaceRate: done"
      return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)
  -- > solutionSpaceRate False (Left False) ((([(2,2),(2,3)],2),2),(2,2))

solutionSpaceRateInRandom :: Bool -> Bool -> ParameterCNF -> IO Float
solutionSpaceRateInRandom debug just paramCNF = do
  let nIteration = 1000 -- or 10000
      limitRate = 1 / 1000 -- 1000000 -- genuine random or..
      file = "SolutionSpaceRateInRandom" </> show just ++ show paramCNF <.> "txt"
      (k, n) = knOfSpace paramCNF
      k' = toInteger k :: Integer
      n' = toInteger n :: Integer
      rateJustFull = fromInteger (combinationNum just (k', n')) / fromInteger (combinationNum False (n', n')) :: Float
  when debug $ putStrLn $ "rateJustFull: " ++ show rateJustFull
  -- 
  if limitRate < rateJustFull
    then do
      when debug $ putStrLn "in genuine random"
      checkInGenuineRandom debug just nIteration paramCNF file
    else do
      checkInPseudoRandom debug just nIteration paramCNF file
  -- 
  js'bs <- (map (read :: String -> ([Int], Bool)) . lines) <$> readFile file
  let js'Trues = filter snd js'bs
      l = length js'bs
      lTrue = length js'Trues
  return $ fromInteger (toInteger lTrue) / fromInteger (toInteger l)
  -- > solutionSpaceRateInRandom True False (((replicate 2 (2,2),2),2),(0,0))

checkInGenuineRandom :: Bool -> Bool -> Int -> ParameterCNF -> FilePath -> IO ()
checkInGenuineRandom debug just nIteration paramCNF file = do
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
    -- 
    js <- findJs just (k, n) jss
    -- 
    bl <- isInTheSolutionSpace paramCNF js
    -- when debug $ print (js, bl) -- [debug]
    appendFile file $ show (js, bl) ++ "\n"
    checkInGenuineRandom debug just nIteration paramCNF file
  where
    findJs just (k, n) jss = do
      zeroOnes <- sequence $ replicate n $ random0toLT 2 :: IO [Int]
      let js = findIndices ((==) 1) zeroOnes
      if ((not just && length js <= k) || (just && length js == k)) && (notElem js jss)
        then return js
        else findJs just (k, n) jss

checkInPseudoRandom :: Bool -> Bool -> Int -> ParameterCNF -> FilePath -> IO ()
checkInPseudoRandom debug just nIteration paramCNF file = do
  when debug $ putStr "in pseudo random: "
  existFile <- doesFileExist file
  unless existFile $ do
    let (k, n) = knOfSpace paramCNF
    nIs <- if just
          then return $ flip map [0..k] \k' -> if k' == k then nIteration else 0
          else do
            let n' = toInteger n
                r0s :: [Float]
                r0s = flip map [0..k] \k' -> fromInteger (combinationNum True (toInteger k', n')) / fromInteger (combinationNum False (n', n'))
                r1s :: [Float]
                r1s = flip map r0s \r -> r / sum r0s
            return $ flip map r1s \r -> roundUpOn5 $ r * fromInteger (toInteger nIteration)
    when debug $ print nIs
    forM_ [1..k] \k' -> do
      when debug $ putStrLn $ "k'=" ++ show k' ++ ":"
      findJss (k', n) [] $ nIs !! k'
  where
  findJss :: KN -> [[Int]] -> Int -> IO ()
  findJss (k, n) jss = \case
    0 -> return ()
    m -> do
      when debug $ putStrLn $ "m: " ++ show m
      js <- findJs (k, n) jss
      findJss (k, n) (js : jss) $ m-1
  findJs (k, n) jss = do
    when debug $ putStrLn $ "(k, n): " ++ show (k, n)
    js <- randamJs (k, n) []
    if elem js jss
      then findJs (k, n) jss
      else do
        bl <- isInTheSolutionSpace paramCNF js
        when debug $ print (js, bl)
        appendFile file $ show (js, bl) ++ "\n"
        return js
  randamJs (k, n) js = do
    if length js == k
      then return js
      else do
        j <- randomChoice $ [0..n-1] \\ js
        randamJs (k, n) $ j : js

-- efficiency

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
    catMaybes $ flip map (parameterTreesAt $ n+d) \paramT ->
      let inTheRange i = 
            let k' = fst (knOfTree (paramT, i))
            in  k <= k' && k' <= k+d
      in  case dropWhile (not . inTheRange) [1..k] of
            [] -> Nothing
            k0 : _ ->
              let (k', n') = knOfTree (paramT, k0)
                  nTrue = k' - k
                  nFalse = n' - n - nTrue
              in  Just ((paramT, k0), (nFalse, nTrue))
  where
  -- > mapM_ print $ parameterCNFsFor (4,12)

efficiency :: Bool -> Bool -> Int -> ParameterCNF -> Maybe (Int, Int) -> IO Float
efficiency debug just nLiteralOther paramCNF mbNoTotal = do
  let ((paramT, k'), (nFalse, nTrue)) = paramCNF
  let (k, n) = knOfTree (paramT, k')
      lApprox = sum (map length $ approxOrderWith binomial id (paramT, k')) + nFalse + nTrue
      literalRate = fromInteger (toInteger lApprox) / fromInteger (toInteger nLiteralOther) :: Float
  pRate <- solutionSpaceRate debug (Left just) paramCNF
  when debug $ do
    putStrLn $ "pRate: " ++ show pRate
    putStrLn $ "lApprox: " ++ show lApprox
  let e = pRate / literalRate
  case mbNoTotal of
    Nothing -> return ()
    Just (no, nTotal) -> do
      let strItem = show no ++ "/" ++ show nTotal ++ " " ++ show just ++ " " ++ show paramCNF ++ " -> "
      putStrLn $ strItem ++ printf "%.8f" e
  return e

theBestEfficiency :: Bool -> Bool -> KN -> IO (Float, ParameterCNF)
theBestEfficiency debug just (k, n) = do
  let lCounter = sum $ map length $ counter id (literalXs n) k
      paramCNFs = parameterCNFsFor (k, n)
      nParamCNFs = length paramCNFs
  effs <- forM (zip [1..] paramCNFs) \(i, paramCNF) -> do
    when debug $ print (i, paramCNF)
    efficiency debug just lCounter paramCNF $ Just (i, nParamCNFs)
  let effParamCNFs = sort $ zip effs paramCNFs
  return $ last effParamCNFs

fileTheBestEfficiencies = "TheBestEfficiencies.txt"

theBestEfficiencies :: IO ()
theBestEfficiencies = do
  let file = fileTheBestEfficiencies
  knMbs <- (map (read :: String -> ((Int, Int), Maybe ((Float, ParameterCNF), (Float, ParameterCNF)))) . lines) <$>
    System.IO.Strict.readFile file
  let (knMbHds, ((k, n), _) : knMbTls) = break (isNothing . snd) knMbs
  eParamPlusOverall <- theBestEfficiency False False (k, n)
  eParamPlusJust <- theBestEfficiency False True (k, n)
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

readTheBestEfficiencies :: IO ()
readTheBestEfficiencies = do
  let file = fileTheBestEfficiencies
  knMbs <- (map (read :: String -> ((Int, Int), Maybe ((Float, ParameterCNF), (Float, ParameterCNF)))) . lines) <$>
    readFile file
  let (knMbJusts, knMbNothings) = break (isNothing . snd) knMbs
  forM_ knMbJusts \((k, n), Just ((eF, pramCNFf),(eT, paramCNFt))) -> do
    putStrLn $ intercalate "," $ [show k, show n, printf "%.8f" eF, printf "%.8f" eT]

--

compareToCounter :: Int -> IO ()
compareToCounter m = forM_ [1..m] \l -> do
  let paramTk' = ((replicate l (2,2), 2), 2)
      (k, n) = knOfTree paramTk'
  print (k, n)
  let nApprox = sum $ map length $ approxOrderWith binomial id paramTk'
      nCounter = sum $ map length $ counter id (literalXs n) k
  putStrLn $ "approx: " ++ show nApprox
  putStrLn $ "counter: " ++ show nCounter
  putStrLn $ showPercentage nApprox nCounter
  e <- efficiency False False nCounter (paramTk', (0,0)) Nothing
  putStrLn $ "efficiency: " ++ show e
