{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}

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

data Tree a
  = Node a [Tree a]
  deriving (Eq, Show, Foldable)
instance Functor Tree where
  fmap f (Node a as) = Node (f a) $ fmap (fmap f) as
-- [Node 2 [Node 2 [], Node 2 [], Node 2 []], Node 2 [Node 3 [], Node 3 []]]

multipleCheck :: [Tree Height] -> IO ()
multipleCheck = mapM_ multCheck
  where
  multCheck (Node h trHs) = do
    let hSum = sum $ map (\(Node h' _) -> h') trHs
    unless (hSum `mod` h == 0) $ die $ show $ Node h trHs
    mapM_ multCheck trHs

heightToHIss :: [Tree Height] -> [Tree (Height, [Int])]
heightToHIss = hToIss []
  where
  hToIss :: [Int] -> [Tree Height] -> [Tree (Height, [Int])]
  hToIss is trHs = flip map (zip [1..] trHs) \(i, Node h trH's) ->
    let i's = is ++ [i]
    in  Node (h, i's) $ hToIss i's trH's

is'xsOf :: [Tree Height] -> [([Int], [Var])]
is'xsOf = makeIs'xs [] . heightToHIss
  where
  makeIs'xs :: [([Int], [Var])] -> [Tree (Height, [Int])] -> [([Int], [Var])]
  makeIs'xs is'xs = \case
    [] -> is'xs
    Node (h, is) [] : trI'ss -> 
      let m = maximum $ (:) 0 $ flip map (concat $ map snd is'xs) \case
            X i -> i
            _ -> 0
          xs = map X [m+1..m+h]
      in  makeIs'xs (is'xs ++ [(is, xs)]) trI'ss
    Node _ trI'ss : trI''ss -> makeIs'xs (makeIs'xs is'xs trI'ss) trI''ss

approxP :: NumberConstraint -> VarScope -> [Tree Height] -> (CNF, [([Int], [Var])])
approxP atMost vScope trHs =
  let p is j = (True, vScope $ P is j)
      h'iss = concatMap concat $ map (fmap return) $ heightToHIss trHs
      cnfOrder = flip concatMap h'iss \(h, is) ->
        flip map [2..h] \j ->
          [ not $ p is j, p is $ j-1 ]
      cnfAtMost = flip concatMap h'iss \(h, is) ->
        let h'is's = filter ((==) is . init . snd) h'iss
            ps = [ p is j | (h', i's) <- h'is's, j <- [1..h'] ]
            n = length ps
        in  flip concatMap [1..h] \j ->
              map ((:) $ p is j) $ atMost (vScope . Scope "approxP") ps $ (n*(j-1)) `div` h
  in  (cnfOrder ++ cnfAtMost, is'xsOf trHs)

{-
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
-}

