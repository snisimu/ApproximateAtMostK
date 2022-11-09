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
approximate xs k = 
    let n = length xs
        x i = xs !! (i-1)
        p is j = (True, P is j)
    in  []

allIsss :: Int -> Int -> [[[Int]]]
allIsss w d = makeIsss (d-1) $ return $ map return [1..w]
  where
  makeIsss :: Int -> [[[Int]]] -> [[[Int]]]
  makeIsss d isss = if d == 0
    then isss
    else makeIsss (d-1) $ isss ++ [[ is ++ [i] | is <- last isss, i <- [1..w] ]]
allIss w d = concat $ allIsss w d

approx :: (Int, Int, Int) -> (CNF, [[Int]])
approx (h, w, d) =
  let n = h * w^d
      p is j = (True, P is j)
      iss = allIss w d
      order = flip concatMap iss \is ->
        flip map [2..h] \j ->
          [ not $ p is j, p is $ j-1 ]
      atMost = flip concatMap iss \is ->
        let ps = p <$> filter ((==) is . init) iss <*> [1..h]
        in  flip concatMap [1..h] \j ->
              map ((:) $ p is j) $ binomial ps $ w*(j-1)
      isLeafs =
        let n = maximum $ map length iss
        in  filter ((==) n . length) iss
  in  (order ++ atMost, isLeafs)

approxWithX :: (Int, Int, Int) -> CNF
approxWithX (h, w, d) =
  let p is j = (True, P is j)
      (cnfP, isLeafs) = approx (h, w, d)
      xss = splitBy (h*w) $ literalXs $ (length isLeafs) * h*w
      cnfX = flip concatMap (zip isLeafs xss) \(is, xs) -> 
        flip concatMap [1..h] \j ->
          map ((:) $ p is j) $ binomial xs $ w*(j-1)
  in  cnfP ++ cnfX
  -- > generateDIMACSwithTrue (approxWithX (2,2,1) ++ binomial [ (True, P [i] j) | i <- [1..2], j <- [1..2] ] 2) [1,2,3,4]

isPossible :: (Int, Int, Int) -> [Int] -> Int -> IO Bool
isPossible (h, w, d) js k = do
  unless (k < h*w) $ die "k: too large"
  let n = h * w^(d+1)
  unless (null $ filter (> n) js) $ die "js: out of range"
  let isss = allIsss w d
      bss = splitBy h $ foldr makeTrueAt (replicate n False) js
        where
        makeTrueAt m bs =
          let (hd, tl) = splitAt m bs
          in  init hd ++ [True] ++ tl
      integrate :: [Int] -> Int
      integrate ls = case integr [] ls of
        [z] -> z
        l's -> integrate l's
        where
        integr gs = \case
          [] -> gs
          hs ->
            let (hHD, hTL) = splitAt w hs
                (y, z) = sum hHD `divMod` w
                r = y + if z == 0 then 0 else 1
            in  integr (gs ++ [r]) hTL
      z = integrate $ map (length . filter id) bss
  return $ z < k

reportPossible :: (Int, Int, Int) -> Int -> IO ()
reportPossible (h, w, d) k = do
  let n = h * w^(d+1)
      ftss = filter ((>=) (k^(d+1)) . length . filter id) $ allFTssOf n
      jss = flip map ftss \fts ->
        catMaybes $ flip map (zip [1..] fts) \(j, bl) ->
          if bl then Just j else Nothing
  js'rs <- forM jss \js -> do
    r <- isPossible (h, w, d) js k
    return (js, r)
  let js'rPossibles = filter snd js'rs
  putStrLn $ show (length js'rPossibles) ++ "/" ++ show (length js'rs)
  print js'rPossibles; print js'rs -- [debug]
