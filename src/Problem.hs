{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Problem where

import System.FilePath
import System.Process

import Control.Monad

import Data.List.Extra

import Text.Printf

import Base
import Lib
import Conventional.Binomial
import Conventional.Counter
import Encoding
import Evaluation

solutionNumsForProblem :: KN -> [Int]
solutionNumsForProblem (k,n) =
  let pN = 10/100
      pK = 1/100
      limitSolutionNum :: Int
      limitSolutionNum = 1000
      n' = fromInteger $ toInteger n
      k' = fromInteger $ toInteger k
      p i =
        let i' = fromInteger $ toInteger i
            p' = (pN-pK) / (n'-k') * i' - (pN-pK) / (n'-k') * n' + pN
        in  if p' < 0
            then 0
            else p'
      -- nn = combinationNum False (n, n)
      n's = flip map [0..n] \i -> floor $ p i * fromInteger (toInteger $ combinationNum True (i, n))
      nSum = sum n's
      r = if limitSolutionNum < nSum
          then fromInteger (toInteger limitSolutionNum) / fromInteger (toInteger nSum)
          else 1
  in  map (floor . (*) r . fromInteger . toInteger) n's

generateProblem :: KN -> IO CNF
generateProblem (k, n) = do
  let limitChunk = 300 -- 100000
      nums = solutionNumsForProblem (k, n)
  -- print nums -- [debug]
  litss <- concat <$> forM [0..n] \k -> do
    let num = nums !! k
        newSolution :: [[Int]] -> IO [Int]
        newSolution js's = do
            -- putStrLn $ show k ++ "/" ++ show n ++ " - " ++ show (length js's) ++ "/" ++ show num -- [debug]
            i <- random0toLT $ combinationNum True (k, n)
            let js = (combinations [0..n-1] k) !! i
            if notElem js js's
                then return js
                else newSolution js's
        defineSolution :: [[Int]] -> IO [[Int]]
        defineSolution js's = do
            if num <= length js's
                then return js's
                else do
                    js <- newSolution js's
                    -- print js -- [debug]
                    defineSolution $ js : js's
    jss <- defineSolution []
    let blss = map (trueIndicesToBools n) jss
    return $ flip map blss \bls ->
        flip map (zip [1..n] bls) \(i, bl) -> (bl, X i)
  -- mapM_ print $ take 10 litss -- [debug]
  let l = length litss
  -- print l -- [debug]
  let nChunk = last $ takeWhile (\i -> n^i <= limitChunk) [1..]
  putStrLn $ "nChunk: " ++ show nChunk -- [debug]
  let litsss = splitBy nChunk litss
      nLitsss = length litsss
  let cnfRepr = return $ map (\i -> (True, Repr i)) [0..nLitsss-1]
  cnfXpre <- forM [0..nLitsss-1] \i -> do
      -- putStrLn $ "distribution " ++ show i ++ "/" ++ show nLitsss -- [debug]
      return $ map ((:) (False, Repr i)) $ distribution $ litsss !! i
  let cnfX = concat cnfXpre
  return $ cnfRepr ++ cnfX

writeProblem :: ParameterCNF -> Maybe Int -> IO ()
writeProblem paramCNF mbNo = do
  let ((paramT, k'), (nFalse, nTrue)) = paramCNF
      (kT, nT) = knOfTree (paramT, k')
      (kS, nS) = knOfSpace paramCNF
  cnfSolution <- generateProblem (kS, nS)
  let cnfAppr = approxOrderWith binomial id (paramT, k')
      cnfFix = map (\i -> [(False, X i)]) [nT-nTrue-nFalse+1..nT-nTrue]
          ++ map (\i -> [(True, X i)]) [nT-nTrue+1..nT]
  let cnfApprox = cnfSolution ++ cnfAppr ++ cnfFix
      cnfCounter = cnfSolution ++ counter id (literalXs nS) kS
      (fileApprox, fileCounter) = case mbNo of
          Nothing -> ("problemApprox.cnf", "problemCounter.cnf")
          Just no -> 
              let file x = "CNF" </> "problem" ++ show paramCNF ++ "-" ++ showZero 3 no ++ "-" ++ x <.> "cnf"
              in  (file "approx", file "counter")
  -- print "writing.." -- [debug]
  writeFile fileApprox =<< strDIMACSwithTrue cnfApprox []
  writeFile fileCounter =<< strDIMACSwithTrue cnfCounter []
  -- > writeProblem ((([(2,3)],2),3),(1,1)) Nothing -- (5,10)
  -- > writeProblem ((([(2,2),(2,3)],2),2),(2,2)) Nothing -- (10,20)
  -- > wsl -- ./minisat problem.cnf

writeProblems :: IO ()
writeProblems = do
  let paramCNF = ((([(2,2),(2,3)],2),2),(2,2)) :: ParameterCNF -- (10,20)
  -- let paramCNF = ((([(2,3)],2),3),(1,1)) :: ParameterCNF -- (5,10)
  forM_ [0..999] $ \i -> do
      print i
      writeProblem paramCNF $ Just i

checkResult = do
  let paramCNF = ((([(2,2),(2,3)],2),2),(2,2)) :: ParameterCNF -- (10,20)
  --- let paramCNF = ((([(2,3)],2),3),(1,1)) :: ParameterCNF -- (5,10)
  let fileID = "problem" ++ show paramCNF
      which = "approx" -- "counter"
      nos = [0..999]
  forM_ nos \no -> do
    print no
    system $ "wsl -- ./minisat ./CNF/" ++ escape fileID ++ "-" ++ showZero 3 no ++ "-" ++ which <.> "cnf > the.log"
    -- readFile "the.log" >>= putStr -- [debug]
    readFile "the.log" >>= appendFile (fileID ++ "-" ++ which <.> "log")
