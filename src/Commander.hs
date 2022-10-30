{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander(atMostOn) where

import Base

divideInto :: Int -> [a] -> [[a]]
divideInto n xs = splitBy (((length xs) + n - 1) `div` n) xs
    where
    splitBy :: Int -> [a] -> [[a]]
    splitBy _ [] = []
    splitBy m x's = xs1:(splitBy m xs2)
        where
        (xs1, xs2) = splitAt m x's

data VAux = C Int Int
    deriving (Eq, Show)


atMostBinomial :: Int -> [(Bool, VarWith a)] -> CNFwith a
atMostBinomial k bvs = map (map \(bl,v) -> (not bl, v)) $
    combinations bvs $ k + 1

atMostOn :: Int -> KN -> CNFwith VAux
atMostOn g (k, n) =
    let hss = divideInto g [1..n]
        c1M = flip concatMap [1..g] \i -> 
                atMostBinomial k $
                    (map (\h -> (True, X h)) $ hss !! (i-1)) ++
                        [ (False, VarAux $ C i j) | j <- [1..k] ]
        c1L = flip concatMap [1..g] \i -> 
                atMostBinomial k $ (map (\h -> (False, X h)) $ hss !! (i-1)) ++
                    [ (True, VarAux $ C i j) | j <- [1..k] ]
        c2 = [ [(True, VarAux $ C i j), (True, VarAux $ C i (j+1))]
                | i <- [1..g]
                , j <- [1 .. k-1]
             ]
        c3 = atMostBinomial k $
                [ (True, VarAux $ C i j) | i <- [1..g], j <- [1..k] ]
    in  foldr1 (++) [c1M, c1L, c2, c3]

{-
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
-}
