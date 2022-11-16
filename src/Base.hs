{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Base where

import Prelude hiding (not)
import qualified Prelude (not)

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.Either
import Data.List (nub, intercalate)

-- from MyLib

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
allFTssOf =  foldr (liftA2 (:)) [[]] . flip replicate [(minBound :: Bool)..]

divideInto :: Int -> [a] -> [[a]]
divideInto n xs = splitBy (((length xs) + n - 1) `div` n) xs
splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy m xs = xs1:(splitBy m xs2)
    where
    (xs1, xs2) = splitAt m xs

showPercentage :: Int -> Int -> String
showPercentage m n =
  let m' = fromInteger (toInteger m) :: Float
      n' = fromInteger (toInteger n) :: Float
  in  "(" ++ show (fromInteger (toInteger $ floor $ m' / n' * 1000) / 10) ++ "%)"

-- MyLib candidate

conbinationssOn :: Bounded a => Int -> [[a]]
conbinationssOn =  foldr (liftA2 (:)) [[]] . flip replicate [(minBound :: a)..]

distribution :: [[a]] -> [[a]]
distribution = foldl1 (liftA2 (++))

-- 

type ScopeID = String

data Var
  = X Int
  | B Int Int | T Int Int -- binary
  | R Int Int -- counter
  | C Int Int -- commander
  | A Int [Int] -- product
  | P [Int] Int -- approximate
  | Scope ScopeID Var
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

xsOf :: CNF -> [Var]
xsOf = nub . map snd . concatMap (filter $ Prelude.not . isAux . snd)

auxsOf :: CNF -> [Var]
auxsOf = nub . map snd . concatMap (filter $ isAux . snd)

printCNF :: CNF -> IO ()
printCNF = do
  mapM_ \literals -> do
    putStrLn $ intercalate " or " $ flip map literals \(bl, v) ->
      (if bl then "" else "~ ") ++
        case v of
          Scope sID v -> "(" ++ sID ++ ")" ++ show v
          _ -> show v
showSIDs sIDs = if null sIDs
  then ""
  else "(" ++ (intercalate ">" $ reverse sIDs) ++ ")"

type NumberConstraint
  = VarScope -> [Literal] -> Int -> CNF
type VarScope = Var -> Var

atLeastBy :: NumberConstraint -> NumberConstraint
atLeastBy atMost vScope literals k
  = atMost vScope (map not literals) (length literals - k + 1)

type KN = (Int, Int)

reportOf :: CNF -> IO ()
reportOf cnf = do
  putStrLn $ "aux vars: " ++ show (length $ auxsOf cnf)
  putStrLn $ "clauses : " ++ show (length cnf)
  putStrLn $ "literals: " ++ show (sum $ map length cnf)
