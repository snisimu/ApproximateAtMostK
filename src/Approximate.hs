{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate where

import Prelude hiding (not)

import System.Exit

import Control.Monad

import Data.Maybe
import Data.List

import Base
import Binomial

approximate :: NumberConstraint
approximate = \_ _ _ -> []

type Height = Int
type Width = Int
type HW = (Height, Width)

multipleCheck :: [HW] -> IO ()
multipleCheck = multCheck <$> fst . head <*> tail
  where
  multCheck :: Height -> [HW] -> IO ()
  multCheck h = \case
    [] -> return ()
    (h', w') : hws -> do
      unless ((h'*w') `mod` h == 0) $ die $ show (h'*w') ++ " mod " ++ show h ++ " /= 0"
      multCheck h' hws

labeling :: [HW] -> [([Int], Height)]
labeling = tail . concat . foldl makeH'Isss [[([], 0)]]
  where
  makeH'Isss ishss (h, w) =
    ishss ++ [[ (is ++ [i], h) | (is, _) <- last ishss, i <- [1..w] ]]

approxP :: NumberConstraint -> VarScope -> [HW] -> (CNF, [[Int]])
approxP atMost vScope hws =
  let p is j = (True, vScope $ P is j)
      is'hs = labeling hws
      cnfOrder = flip concatMap is'hs \(is, h) ->
        flip map [2..h] \j ->
          [ not $ p is j, p is $ j-1 ]
      cnfAtMost = flip concatMap is'hs \(is, h) ->
        let theIs'hs = filter ((==) is . init . fst) is'hs
            ps = concatMap (\(is, h) -> p is <$> [1..h]) theIs'hs
            h' = if null theIs'hs then 0 else snd $ head theIs'hs
            w' = length theIs'hs
        in  flip concatMap [1..h] \j ->
              let theScope = vScope . Scope ("approxP:" ++ show is ++ show j)
              in  map ((:) $ p is j) $ 
                    atMost theScope ps $ (h'*w'*(j-1)) `div` h
      isLeafs =
        let iss = map fst is'hs
        in  filter ((==) (length hws) . length) iss
  in  (cnfOrder ++ cnfAtMost, isLeafs)

approx :: NumberConstraint -> VarScope -> [HW] -> Int -> CNF
approx atMost vScope hws k =
  let vScopeNext sID = vScope . Scope ("approx:" ++ sID)
      p is j = (True, vScope $ P is j)
      (h, w) = head hws
      cnfTop = atMost (vScopeNext "top") [ (True, P [i] j) | i <- [1..w], j <- [1..h] ] k
      (cnfP, isLeafs) = approxP atMost vScope{- (vScopeNext "P") -} hws
      (h', w') = last hws
      m = product $ map snd $ init hws
      xss = splitBy h' $ literalXs $ h'*w'*m
      cnfX = flip concatMap (zip isLeafs xss) \(is, xs) -> 
        flip concatMap [1..h'] \j ->
          map ((:) $ p is j) $ atMost (vScopeNext $ "X:" ++ show is ++ show j) xs $ j-1
  in  cnfTop ++ cnfP ++ cnfX
  -- > generateDIMACSwithTrue (approx counter id [(2,2),(2,2)] 2) [1,2,3,4]
  
isPossible :: [HW] -> Int -> [Int] -> IO Bool
isPossible hws k js = do
  let (h', w') = head hws
  unless (k < h'*w') $ die "k: too large"
  let (h, w) = last hws
      m = product $ map snd $ init hws
      n = h * w * m
  unless (null $ filter (> n) js) $ die "js: out of range"
  let bss = splitBy h $ foldr makeTrueAt (replicate n False) js
        where
        makeTrueAt m bs =
          let (hd, tl) = splitAt m bs
          in  init hd ++ [True] ++ tl
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
      jsKs = combinations [1..n] k
      -- jsLtKs = [] : concatMap (combinations [1..n]) [1..k-1]
      check jss = forM jss \js -> do
        bl <- isPossible hws k' js
        return (js, bl)
  jsK'bls <- check jsKs
  -- jsLtK'bls <- check jsLtKs
  let jsK'Trues = filter snd jsK'bls
      -- jsLtK'Trues = filter snd jsLtK'bls
      lK = length jsK'bls
      lKTrue = length jsK'Trues
  putStrLn $ show lKTrue ++ "/" ++ show lK ++ showPercentage lKTrue lK
  -- print js'rPoss; print js'rs -- [debug]
  -- > reportOf $ counter (literalXs 20) 10

{-
byIsPossible :: [HW] -> Int -> IO [[Bool]]
byIsPossible hws k' = do
  let (k, n) = knOf hws k'
      jsKs = combinations [1..n] k
      check jss = forM jss \js -> do
        bl <- isPossible hws k' js
        return (js, bl)
  jsK'bls <- check jsKs
  let jsK'Trues = filter snd jsK'bls
      jss = map fst jsK'Trues
      makeTrueAt m bs =
        let (hd, tl) = splitAt m bs
        in  init hd ++ [True] ++ tl
  return $ map (foldr makeTrueAt (replicate n False)) jss
-}



possible22bl :: Int -> [[Bool]]
possible22bl d = nub $ pble22 d 1 2
  where
  (+++) a b = (++) <$> a <*> b
  pble22 d d' = \case
    0 -> pb22 0 +++ pb22 0
    1 -> pb22 1 +++ pb22 0
          ++ pb22 0 +++ pb22 1
    2 -> pb22 2 +++ pb22 0
          ++ pb22 1 +++ pb22 1
          ++ pb22 1 +++ pb22 0 --
          ++ pb22 0 +++ pb22 1 --
          ++ pb22 0 +++ pb22 2
    3 -> pb22 2 +++ pb22 1
          ++ pb22 1 +++ pb22 1 --
          ++ pb22 1 +++ pb22 0 --
          ++ pb22 0 +++ pb22 1 --
          ++ pb22 0 +++ pb22 2 --
          ++ pb22 1 +++ pb22 2
    4 -> pb22 2 +++ pb22 2
          ++ pb22 2 +++ pb22 1 --
          ++ pb22 2 +++ pb22 0 --
          ++ pb22 1 +++ pb22 0 --
          ++ pb22 0 +++ pb22 1 --
          ++ pb22 0 +++ pb22 2 --
          ++ pb22 1 +++ pb22 2 --
   where
    pb22 k = if d == d'
      then concatMap (flip ftss 2) [0..k]
      else pble22 d (d'+1) (k*2)
ftss k n = filter ((==) k . length . filter id) $ allFTssOf n

combinationNum r n = (product [1..n]) `div` (product [1..r] * product [1..n-r])
combinationLTnum r n = sum $ map (flip combinationNum n) [0..r]

possibilityRate22 d = do
  let n = length $ possible22bl d
      a = 4 * 2^(d-1)
      m = combinationLTnum (a `div` 2) a
  putStrLn $ show n ++ "/" ++ show m ++ showPercentage n m
