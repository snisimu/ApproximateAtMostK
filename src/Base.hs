{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Base where

import Control.Monad

import Data.Maybe
import Data.List (nub, intercalate)

combinations :: [a] -> Int -> [[a]]
combinations xs n | n > 0 = 
    go n (length (take n xs) == n) (drop n xs) xs
    where
    go n' b p ~(x:x's)
        | n' == 0 = [[]]
        | not b  = []
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

data Var a b
    = X Int
    | Vaux a
    | Vlocal String b
    deriving (Eq, Show)

type Literal a b = (Bool, Var a b)

type CNF a b = [[Literal a b]]

auxVarsOf :: Eq a => CNFwith a -> [a]
auxVarsOf cnf = nub $ flip concatMap cnf \bvs ->
  catMaybes $ flip map bvs \case
    (_, VarAux v) -> Just v
    _ -> Nothing

type KN = (Int, Int)

printCNF :: Show a => CNFwith a -> IO ()
printCNF bvss = do
  forM_ bvss \bvs -> do
    putStrLn $ intercalate " or " $ flip map bvs \(bl, v) ->
      (if bl then "" else "~ ") ++
        case v of
          VarAux va -> show va
          _ -> show v
