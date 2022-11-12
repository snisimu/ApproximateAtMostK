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
  multCheck h' = \case
    [] -> return ()
    (h, w) : hws -> do
      unless ((h*w) `mod` h' == 0) $ die $ show (h*w) ++ " mod " ++ show h' ++ " /= 0"
      multCheck h hws    

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
            ps = map (uncurry p) theIs'hs
            w = length theIs'hs
        in  flip concatMap [1..h] \j ->
              map ((:) $ p is j) $ atMost (vScope . Scope ("approxP:" ++ show is ++ show j)) ps $ w*(j-1)
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
  -- > generateDIMACSwithTrue (approx counter id [(2,2)] 2) [1,2,3]

{-
isPossible :: (Int, Int, Int) -> Int -> [Int] -> IO Bool
isPossible (h, w, d) k js = do
  unless (k < h*w) $ die "k: too large"
  let n = h * w^(d+1)
  unless (null $ filter (> n) js) $ die "js: out of range"
  let bss = splitBy h $ foldr makeTrueAt (replicate n False) js
        where
        makeTrueAt m bs =
          let (hd, tl) = splitAt m bs
          in  init hd ++ [True] ++ tl
      integrate :: [Int] -> IO Int
      integrate ls = do
        -- print ls -- [debug]
        let l's = integr [] ls
        if length l's <= w
          then do
            -- print l's -- [debug]
            return $ sum l's
          else integrate l's
        where
        integr gs = \case
          [] -> gs
          hs ->
            let (hHD, hTL) = splitAt w hs
                (y, z) = sum hHD `divMod` w
                r = y + if z == 0 then 0 else 1
            in  integr (gs ++ [r]) hTL
  z <- integrate $ map (length . filter id) bss
  -- print z -- [debug]
  return $ z <= k

reportApprox :: NumberConstraint -> Bool -> (Int, Int, Int) -> Int -> IO ()
reportApprox atMost blPossibilityRate (h, w, d) k' = do
  let k = k' * w^d
      n = h * w^(d+1)
  putStrLn $ "(k=" ++ show k ++ ",n=" ++ show n ++ ")"
  reportOf $ approx atMost id (h, w, d) k'
  when blPossibilityRate $ do
    let ftss = filter ((>=) k . length . filter id) $ allFTssOf n
        jss = flip map ftss \fts ->
          catMaybes $ flip map (zip [1..] fts) \(j, bl) ->
            if bl then Just j else Nothing
    js'rs <- forM jss \js -> do
      r <- isPossible (h, w, d) k' js
      return (js, r)
    let js'rPossibles = filter snd js'rs
        l = length js'rs
        lPossible = length js'rPossibles
    putStrLn $ show lPossible ++ "/" ++ show l ++ showPercentage lPossible l
    -- print js'rPossibles; print js'rs -- [debug]
  -- > reportPossible (5,2,1) 5
  -- > reportOf $ counter (literalXs 20) 10
-}