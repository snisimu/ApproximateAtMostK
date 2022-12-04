{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Statistics where

import Base
import Lib
import Evaluation

-- solutionNumsForActualRate :: KN -> Float -> [Int]
solutionNumsForActualRate (k,n) r =
  flip map (zip (reverse [0..k]) $ iterate (/10) r) \(k', r') ->
    (k', r')

{-
actualRate :: ParameterCNF -> IO ()
actualRate paramCNF = do
  let ((paramT, k'), (nFalse, nTrue)) = paramCNF
      (kT, nT) = knOfTree (paramT, k')
      (kS, nS) = knOfSpace paramCNF
      nums = solutionNums (k, n)
      file = "" </> 

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
  -}
