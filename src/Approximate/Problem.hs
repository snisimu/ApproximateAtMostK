{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Problem where

import Control.Monad

import Text.Printf

import Base
import Lib
import Counter
import Approximate.Base
import Approximate.Lib
import Approximate.Encoding

pN = 10/100
pK = 1/100

solutionNums :: KN -> [Int]
solutionNums (k,n) =
    let limitSolutionNum :: Int
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
    let nums = solutionNums (k, n)
    -- print nums -- [debug]
    litss <- concat <$> forM [0..n] \k -> do
        let num = nums !! k
            newSolution :: [[Int]] -> IO [Int]
            newSolution js's = do
                -- print $ length js's -- [debug]
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
    let cnfRepr = return $ map (\i -> (True, Repr i)) [0..l-1]
        cnfX = flip concatMap [0..l-1] \i ->
            flip map [1..n] \j ->
                (False, Repr i) : [(litss !! i) !! (j-1)]
    return $ cnfRepr ++ cnfX

writeProblem :: ParameterCNF -> IO ()
writeProblem paramCNF = do
    let ((paramT, k'), (nFalse, nTrue)) = paramCNF
        (kT, nT) = knOfTree paramT k'
        (kS, nS) = knOfSpace paramCNF
    cnfSolution <- generateProblem (kS, nS)
    let cnfApprox = approxOrderWith counter id paramT k'
        cnfFT = map (\i -> [(False, X i)]) [nT-nTrue-nFalse..nT-nTrue]
            ++ map (\i -> [(True, X i)]) [nT-nTrue..nT]
        cnf = cnfSolution ++ cnfApprox ++ cnfFT
    printCNF cnf -- [debug]
    writeFile "problem.cnf" =<< strDIMACSwithTrue cnf []
    -- > wsl -- ./minisat problem.cnf
