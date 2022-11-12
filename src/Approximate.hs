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
          let l'ss = divideInto w' ls
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

overall22 :: Int -> Int -> Int
overall22 lv k =
  let f = overall22 (lv-1)
  in  case k of
        0 -> 1
        1 -> if lv == 0 then 4 else
              f 2 * f 0
              + f 1 * f 1
              + f 0 * f 2
        2 -> if lv == 0 then 6 else
              f 4 * f 0
              + f 3 * f 1
              + f 2 * f 2
              + f 1 * f 3
              + f 0 * f 4
        3 -> if lv == 0 then 4 else
              f 3 * f 0
              + f 2 * f 1
              + f 1 * f 2
              + f 0 * f 3
        4 -> 1

possible22 :: Int -> Int
possible22 lv = 
  let f = p22 $ lv-1
  in  if lv == 0 then 6 else
        f 2 * f 0
        + f 1 * f 1
        + f 0 * f 2
  where
    p22 lv k =
      let f = p22 (lv-1)
      in  case k of
            0 -> 1
            1 -> if lv == 0 then 6 else
                  f 2 * f 0
                  + f 1 * f 1
                  + f 0 * f 2
            2 -> 1

possible22' :: Int -> [[Bool]]
possible22' lv = 
  let f = p22 $ lv-1
  in  if lv == 0 then fts 2 4 else
        ((++) <$> f 2 <*> f 0)
        ++ ((++) <$> f 1 <*> f 1)
        ++ ((++) <$> f 0 <*> f 2)
  where
    fts k n = filter ((==) k . length . filter id) $ allFTssOf n
    p22 lv k =
      let f = p22 (lv-1)
      in  case k of
            0 -> [replicate 4 False]
            1 -> if lv == 0 then fts 1 2 else
                  ((++) <$> f 2 <*> f 0)
                  ++ ((++) <$> f 1 <*> f 1)
                  ++ ((++) <$> f 0 <*> f 2)
            2 -> [replicate 4 True]
