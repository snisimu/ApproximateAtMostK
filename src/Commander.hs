{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander(commander) where

import Base

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy m xs = xs1:(splitBy m xs2)
    where
    (xs1, xs2) = splitAt m xs

data Vcounter
    = C Int Int
    deriving (Eq, Show)

commander :: NumberConstraint a b -> Int -> NumberConstraint (Either a b) Vcounter
commander atMost s literals k =
    let hss = splitBy s [1..n]
        g = length hss
        n = length literals
        literal's = map liftLeft literals
        c i j = (True, Right $ C i j)
        c1 = flip concatMap [1..g] \i -> 
                let lits = [ literals !! (h-1) | h <- hss !! (i-1) ]
                        ++ [ c i j | j <- [1..k] ]
                in  atMost lits k ++ atLeastBy atMost lits k
        c2 = [ [c i j, c i (j+1)]
                | i <- [1..g]
                , j <- [1 .. k-1]
             ]
        c3 = atMost [ c i j | i <- [1..g], j <- [1..k] ] k
    in  c1 ++ c2 ++ c3 

{-
appendCnfWhenAtMostCommander3 :: String -> Variable -> Int -> [Variable] -> GenCnfConstraint ()
appendCnfWhenAtMostCommander3 strId v n vs = do
  let strId' = strId ++ ":WhenAtMostCommander3:NumberConstraint" ++ show n
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
