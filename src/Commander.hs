{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander(commander) where

import Base

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy m xs = xs1:(splitBy m xs2)
    where
    (xs1, xs2) = splitAt m xs

data Vcommander
    = C Int Int
    deriving (Eq, Show)

commander :: NumberConstraint (Either a Vcommander) b -> Int -> NumberConstraint a (Either (Int, Int, b) Vcommander)
commander atMost s literals k =
    let literal's = lifts literals
        hss = splitBy s [1..n]
        g = length hss
        n = length literals
        c i j = (True, Aux $ Right $ C i j)
        trans :: (Int, Int) -> CNF (Either (Either a Vcommander) b) -> CNF (Either a (Either (Int, Int, b) Vcommander))
        trans (y,z) cnf = flip map cnf \literals ->
          flip map literals \(bl, v) -> (,) bl $ case v of
            X i -> X i
            Aux (Left (Left v)) -> Aux $ Left v
            Aux (Left (Right v)) -> Aux $ Right $ Right v
            Aux (Right v) -> Aux $ Right $ Left (y, z, v)
        -- c1 :: CNF (Either a (Either (Int, Int, b) Vcommander))
        c1 = flip concatMap [1..g] \i -> 
                let -- lits :: [Literal (Either a Vcommander)]
                    lits = [ literal's !! (h-1) | h <- hss !! (i-1) ]
                        ++ [ c i j | j <- [1..k] ]
                in  trans (1, i) (atMost lits k)
                      ++ trans (2, i) (atLeastBy atMost lits k)
                      :: CNF (Either a (Either (Int, Int, b) Vcommander))
        c2 :: CNF (Either a (Either (Int, Int, b) Vcommander))
        c2 = mapRight Right
              [ [c i j, c i (j+1)]
              | i <- [1..g]
              , j <- [1 .. k-1]
              ]
        c3 :: CNF (Either a (Either (Int, Int, b) Vcommander))
        c3 = trans (3, 0) $ atMost [ c i j | i <- [1..g], j <- [1..k] ] k
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
