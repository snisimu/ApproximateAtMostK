{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Base where

import Prelude hiding (not)
import qualified Prelude (not)

import Control.Monad

import Data.Maybe
import Data.Either
import Data.List (nub, intercalate)

-- lib [begin]

combinations :: [a] -> Int -> [[a]]
combinations xs n | n > 0 = 
    go n (length (take n xs) == n) (drop n xs) xs
    where
    go n' b p ~(x:x's)
        | n' == 0 = [[]]
        | Prelude.not b  = []
        | null p = [(x:x's)]
        | otherwise = map (x:) (go (n'-1) b p x's)
                        ++ go n' b (tail p) x's

allFTssOf :: Int -> [[Bool]]
allFTssOf n = filter (\ss -> length ss == n) allFTss
  where
  allFTss = if n == 0 then [[]] else [] : makeFTss (n - 1) [[False],[True]]
  makeFTss m bss = if m == 0
    then bss
    else makeFTss (m - 1) $ bss ++ [ bs ++ [False] | bs <- bss ] ++ [ bs ++ [True] | bs <- bss ]

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy m xs = xs1:(splitBy m xs2)
    where
    (xs1, xs2) = splitAt m xs

-- lib [end]

data Var
  = X Int
  | B Int Int | T Int Int -- binary
  | R Int Int -- counter
  | C [String] Int Int -- commander
  | A [String] Int [Int] -- product
   deriving (Eq, Show)

isAux :: Var -> Bool
isAux =  \case
  X _ -> False
  _ -> True

type Literal = (Bool, Var)

literalXs :: Int -> [Literal]
literalXs n = [ (True, X i) | i <- [1..n] ]

not :: Literal -> Literal
not (bl, v) = (Prelude.not bl, v)

type CNF = [[Literal]]

auxsOf :: CNF -> [Var]
auxsOf = nub . map snd . concatMap (filter $ isAux . snd)

printCNF :: CNF -> IO ()
printCNF = do
  mapM_ \literals -> do
    putStrLn $ intercalate " or " $ flip map literals \(bl, v) ->
      (if bl then "" else "~ ") ++
        case v of
          C sIDs i j -> "C " ++ showSIDs sIDs ++ " " ++ show i ++ " " ++ show j
          A sIDs i js -> "A " ++ showSIDs sIDs ++ " " ++ show i ++ " " ++ show js
          _ -> show v
  where
  showSIDs sIDs = if null sIDs
    then ""
    else "(" ++ (intercalate ">" $ reverse sIDs) ++ ")"

type NumberConstraint
  = [Literal] -> Int -> CNF

atLeastBy :: NumberConstraint -> NumberConstraint
atLeastBy atMost literals k = atMost (map not literals) (length literals - k + 1)

type KN = (Int, Int)
