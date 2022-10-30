{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander(atMostOn) where

import Base

factorial n = product [1 .. n]
combinationNum n r = factorial n `div` (factorial r * factorial (n-r))
divideInto :: Int -> [a] -> [[a]]
divideInto n xs = splitBy (((length xs) + n - 1) `div` n) xs
    where
    splitBy :: Int -> [a] -> [[a]]
    splitBy _ [] = []
    splitBy m xs = xs1:(splitBy m xs2)
        where
        (xs1, xs2) = splitAt m xs

data VAux = C Int Int
    deriving (Eq, Show)

atMostBinomial k xs = map (zip $ repeat False) $
    combinations xs $ (length xs) - k + 1

atMostOn :: Int -> KN -> CNFwith VAux
atMostOn g (k, n) =
    let hss = divideInto g [1..n]
        c1M = flip map [0 .. g-1] \i -> 
                atMostBinomial k $ (map X $ hss !! i) ++ flip map []

appendCnfWhenAtMostCommander3 :: String -> Variable -> Int -> [Variable] -> GenCnfConstraint ()
appendCnfWhenAtMostCommander3 strId v n vs = do
  let strId' = strId ++ ":WhenAtMostCommander3:AtMost" ++ show n
  let vss = divideInto 3 vs
      ns = [0 .. n]
  withNewVarsDivided strId (VarCountCommander strId <$> [0..2]) ns $ do
    forM_ [0..2] \x -> do
      let vs = vss !! x
      appendCnf $ cnf (strId' ++ ":Max:Group" ++ show x) $
        bvssWhen v $ bvssAtMost n vs
    forM_ ((,) <$> [0..2] <*> tail ns) \(x,y) -> do
      let vs = vss !! x
      appendCnf $ cnf (strId ++ ":Group" ++ show x ++ ":LT" ++ show y) $
        bvssUnless (VarCountCommander strId x y) $ bvssAtMost (y-1) vs
    let vs = uncurry (VarCountCommander strId) <$> ((,) <$> [0..2] <*> tail ns)
    appendCnf $ cnf (strId ++ ":CountCommanders") $
      bvssWhen v $ bvssAtMost n vs
