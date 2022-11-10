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

type Width = Int
type Height = Int
type WH = (Width, Height)

data WHtree
  = Nil
  | WHtree WH [WHtree]
  deriving (Eq, Show)

{-
allWHsss :: WHtree -> [[[WH]]]
allWHsss = makeWHsss []
  where
  makeWHsss :: [[[WH]]] -> WHtree -> [[[WH]]]
  makeWHsss whsss = \case
    Nil -> whsss
    WHTree wh whTrees -> 
      let
      in  whsss ++ 
-}

allIss :: Int -> Int -> [[Int]]
allIss w d = concat $ makeIsss (d-1) $ return $ map return [1..w]
  where
  makeIsss :: Int -> [[[Int]]] -> [[[Int]]]
  makeIsss d' isss = if d' == 0
    then isss
    else makeIsss (d'-1) $ isss ++ [[ is ++ [i] | is <- last isss, i <- [1..w] ]]

approxP :: NumberConstraint -> VarScope -> (Int, Int, Int) -> (CNF, [[Int]])
approxP atMost vScope (h, w, d) =
  let p is j = (True, vScope $ P is j)
      iss = allIss w d
      cnfOrder = flip concatMap iss \is ->
        flip map [2..h] \j ->
          [ not $ p is j, p is $ j-1 ]
      cnfAtMost = flip concatMap iss \is ->
        let ps = p <$> filter ((==) is . init) iss <*> [1..h]
        in  flip concatMap [1..h] \j ->
              map ((:) $ p is j) $ atMost (vScope . Scope "appr") ps $ w*(j-1)
      isLeafs =
        let m = maximum $ map length iss
        in  filter ((==) m . length) iss
  in  (cnfOrder ++ cnfAtMost, isLeafs)

approx :: NumberConstraint -> VarScope -> (Int, Int, Int) -> Int -> CNF
approx atMost vScope (h, w, d) k =
  let vScopeNext sID = vScope . Scope ("approx:" ++ sID)
      p is j = (True, vScope $ P is j)
      cnfTop = atMost (vScopeNext "top") [ (True, P [i] j) | i <- [1..w], j <- [1..h] ] k
      (cnfP, isLeafs) = approxP atMost (vScopeNext "P") (h, w, d)
      xss = splitBy (h*w) $ literalXs $ (length isLeafs) * h*w
      cnfX = flip concatMap (zip isLeafs xss) \(is, xs) -> 
        flip concatMap [1..h] \j ->
          map ((:) $ p is j) $ atMost (vScopeNext "X") xs $ w*(j-1)
  in  cnfTop ++ cnfP ++ cnfX
  -- > generateDIMACSwithTrue (approx id (2,2,1) 2) [1,2,3,4]

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
  