{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Evaluation where

import Prelude hiding (not)
import qualified Prelude (not)

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

import Data.Maybe
import Data.List

import Base
import Binomial

import Approximate.Base
import Approximate.Lib
import Approximate.Encoding

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
      jss = [] : concatMap (combinations [1..n]) [1..k]
      check jss = forM jss \js -> do
        bl <- isPossible hws k' js
        return (js, bl)
  js'bls <- check jss
  let js'Trues = filter snd js'bls
      l = length js'bls
      lTrue = length js'Trues
  putStrLn $ show lTrue ++ "/" ++ show l ++ showPercentage lTrue l

-- for (2, 2)

possible22bssJust :: Int -> [[Bool]]
possible22bssJust d = pble22 d 1 2
  where
  (+++) a b = (++) <$> a <*> b
  pble22 d d' = \case
    0 -> pb22 0 +++ pb22 0
    1 -> pb22 1 +++ pb22 0
          ++ pb22 0 +++ pb22 1
    2 -> pb22 2 +++ pb22 0
          ++ pb22 1 +++ pb22 1
          ++ pb22 0 +++ pb22 2
    3 -> pb22 2 +++ pb22 1
          ++ pb22 1 +++ pb22 2
    4 -> pb22 2 +++ pb22 2
   where
    pb22 k = if d == d'
      then ftss k 2
      else pble22 d (d'+1) (k*2)

fileFor :: (Int, Int) -> FilePath
fileFor (k, n) = "work" </> show k ++ "-" ++ show n <.> "txt"

lengthOf :: (Int, Int) -> IO Int
lengthOf (k, n) = do
  let fileIn = fileFor (k, n)
  bl <- doesFileExist fileIn
  unless bl $ die "not exist"
  return . length . lines =<< readFile fileIn

-- procedure

makeInit d = do
    let n = 4 * 2^(d-1)
        k = n `div` 2
    writeFile (fileFor (k, n)) $ unlines $
        map (show . findIndices id) $ possible22bssJust d

resumeDrop :: String -> IO ()
resumeDrop file = do
    let filePath = "work" </> file
    ls <- lines <$> readFile filePath
    let n = length ls
    case findIndex ((/=) "[[" . take 2) ls of
        Nothing -> return ()
        Just l -> do
            putStrLn $ show (l+1) ++ "/" ++ show n
            let is = (read :: String -> [Int]) $ ls !! l
                iss = flip map [0 .. length is - 1] \h ->
                    let (i1s, _ : i2s) = splitAt h is
                    in  i1s ++ i2s
                (l1s, _ : l2s) = splitAt l ls
            writeFile filePath $ unlines $ l1s ++ [show iss] ++ l2s
            resumeDrop file

resumeNub :: String -> IO ()
resumeNub = rsNub Nothing
    where
    rsNub :: Maybe [[Int]] -> String -> IO ()
    rsNub mbIss file = do
        let filePath = "work" </> file
        ls <- lines <$> readFile filePath
        let n = length ls
        case findIndex ((==) "[[" . take 2) ls of
            Nothing -> return ()
            Just j -> do
                putStrLn $ show (j+1) ++ "/" ++ show n
                let (lPreviouss, l : l's) = splitAt j ls
                    isPreviouss = fromMaybe (map (read :: String -> [Int]) lPreviouss) mbIss
                    iss = (read :: String -> [[Int]]) l
                    is's = filter (Prelude.not . flip elem isPreviouss) iss
                    strIs's = if null is's then [] else [show is's]
                writeFile filePath $ unlines $ map show isPreviouss ++ strIs's ++ l's
                rsNub (Just $ isPreviouss ++ is's) file

total :: KN -> IO ()
total (k, n) = do
    ls <- forM [0..k] \i -> lengthOf (i, n)
    print $ sum ls