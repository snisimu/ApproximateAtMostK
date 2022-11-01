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

data Var a
   = X Int
   | Aux a
   deriving (Eq, Show)

-- instance Functor Var where

type Literal a = (Bool, Var a)

lift :: Literal a -> Literal (Either a b)
lift = fmap \case
  X i -> X i
  Aux v -> Aux $ Left v
lifts :: [Literal a] -> [Literal (Either a b)]
lifts = map lift

literalXs :: Int -> [Literal ()]
literalXs n = [ (True, X i) | i <- [1..n] ]

not :: Literal a -> Literal a
not (bl, v) = (Prelude.not bl, v)

type CNF a = [[Literal a]]

auxsOf :: Eq a => CNF a -> [a]
auxsOf cnf = nub $ flip concatMap cnf \literals ->
  catMaybes $ flip map literals \case
    (_, Aux v) -> Just v
    _ -> Nothing

printCNF :: (Show a, Show b) => CNF (Either a b) -> IO ()
printCNF cnf = do
  forM_ cnf \literals -> do
    putStrLn $ intercalate " or " $ flip map literals \(bl, v) ->
      (if bl then "" else "~ ") ++
        case v of
          Aux (Right v') -> show v'
          _ -> show v

type NumberConstraint a b
  = [Literal a] -> Int -> CNF (Either a b)

atLeastBy :: NumberConstraint a b -> NumberConstraint a b
atLeastBy atMost literals k = atMost (map not literals) (length literals - k + 1)

type KN = (Int, Int)
