{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Base where

import Prelude hiding (not)
import qualified Prelude (not)

import Control.Monad

import Data.Maybe
import Data.Either
import Data.List (nub, intercalate)

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

newtype VarX = X Int
    deriving (Eq, Show)

type Literal a = (Bool, a)

literalXs :: Int -> [Literal VarX]
literalXs n = [ (True, X i) | i <- [1..n] ]

not :: Literal a -> Literal a
not (b, v) = (Prelude.not b, v)

type CNF a = [[Literal a]]
{-
cnfMap :: (a -> b) -> CNF a -> CNF b
cnfMap = fmap . fmap . fmap . fmap
-}

auxsOf :: Eq b => CNF (Either a b) -> [b]
auxsOf cnf = nub $ flip concatMap cnf \literals ->
  catMaybes $ flip map literals \case
    (_, Left _) -> Nothing
    (_, Right v) -> Just v

printCNF cnf = do
  forM_ cnf \literals -> do
    putStrLn $ intercalate " or " $ flip map literals \(b, v) ->
      (if b then "" else "~ ") ++
        case v of
          Right v' -> show v'
          Left v' -> show v'

type NumberConstraint a b = [Literal a] -> Int -> CNF (Either a b)

atLeastBy :: NumberConstraint a b -> NumberConstraint a b
atLeastBy atMost literals k = atMost (map not literals) (length literals - k + 1)

type KN = (Int, Int)
