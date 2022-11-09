{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate where

import Prelude hiding (not)

import System.Exit

import Control.Monad

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
  let n = h * w^d
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

{-
countObjBss n =
  let bssAll = countAllBss n
      l = maximum $ map length bssAll
  in  filter ((==) l . length) bssAll
vars'cnfsCount strId n ord =
  let vf = VarCountOf10 strId
      cnfOneHot bss = cnf ("CountOf10Body:OneHot:" ++ show bss) (bvssOne (map (vf False bss) [0..10]))
      cnfCnt m bs p = if m == 0
        then []
        else flip concatMap [0..10] \q ->
          let r = p - (q - p)
              commentHead = "CountOf10Body:" ++ show bs ++ show p ++ ":"
          in  if 0 <= r && r <= 10
              then cnf (commentHead ++ show q ++ "-" ++ show r) (bvssWhen (vf False bs p) $ bvssWhen (vf False (bs ++ [False]) q) $ bvssAtLeast 1 [vf False (bs ++ [True]) r])
                ++ cnfCnt (m - 1) (bs ++ [False]) q
                ++ cnfCnt (m - 1) (bs ++ [True]) r
              else cnf (commentHead ++ show q ++ ":N/A") $ bvssWhen (vf False bs p) $ bvssNo [vf False (bs ++ [False]) q] ++ bvssNo [vf False (bs ++ [True]) q]
      bvssOrd = case ord of
        LT -> bvssAtMost
        EQ -> bvssEq
        GT -> bvssAtLeast
      cnfObjs = flip concatMap ((,) <$> countObjBss n <*> [0..10]) \(bs, k) ->
        cnf ("CountOf10Body:" ++ show bs ++ show k) $ bvssWhen (vf False bs k) $ bvssOrd k $ map (vf True bs) [0..9]
      cnfs = concatMap cnfOneHot (countAllBss n) ++ concatMap (cnfCnt n []) [0..10] ++ cnfObjs
      vars = (vf False <$> countAllBss n <*> [0..10])
        ++ (vf True <$> countObjBss n <*> [0..9])
  in  (vars, cnfs)
  -- > mapM_ (putStrLn . showCnfLineHuman) $ cnfCount "tr" 1
-}