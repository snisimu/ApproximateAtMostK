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

data Var a
    = X Int
    | Aux a
    deriving (Eq, Show)
instance Functor Var where
  fmap f = \case
    X i -> X i
    Aux v -> Aux $ f v

type Literal a = (Bool, Var a)
literalMap:: (a -> b) -> Literal a -> Literal b
literalMap = fmap . fmap
liftX :: Literal a -> Literal (Either a b)
liftX = literalMap Left

literalXs :: Int -> [Literal a]
literalXs n = [ (True, X i) | i <- [1..n] ]

not :: Literal a -> Literal a
not (b, v) = (Prelude.not b, v)

type CNF a = [[Literal a]]
cnfMap :: (a -> b) -> CNF a -> CNF b
cnfMap = fmap . fmap . fmap . fmap

auxsOf :: Eq a => CNF a -> [a]
auxsOf cnf = nub $ flip concatMap cnf \bvs ->
  catMaybes $ flip map bvs \case
    (_, Aux v) -> Just v
    _ -> Nothing

printCNF bvss = do
  forM_ bvss \bvs -> do
    putStrLn $ intercalate " or " $ flip map bvs \(bl, v) ->
      (if bl then "" else "~ ") ++
        case v of
          Aux va -> show va
          _ -> show v

type NumberConstraint a b = [Literal a] -> Int -> CNF (Either a b)

atLeastBy :: NumberConstraint a b -> NumberConstraint a b
atLeastBy atMost literals k = atMost (map not literals) (length literals - k + 1)

type KN = (Int, Int)
