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

commander :: NumberConstraint (Either a Vcommander) b -> Int -> NumberConstraint (Either a Vcommander) (Int, Int, b)
commander atMost s literals k =
    let hss = splitBy s [1..n]
        g = length hss
        n = length literals
        c :: Int -> Int -> Literal (Either a Vcommander)
        c i j = (True, Aux $ Right $ C i j)
        -- liftLocal :: Int -> Int -> CNF (Either a b) -> CNF (Either a (Int,Int,b))
        liftLocal numID0 numID1 cnf = flip map cnf \lits ->
          flip map lits $ fmap \case
            Aux (Right v) -> Aux $ Right (numID0, numID1, v)
            Aux (Left v) -> Aux $ Left v
            X i -> X i
        replaceRight cnf = flip map cnf \lits ->
          flip map lits $ fmap \case
            Aux (Right v) -> Aux $ Left $ Right v
            Aux (Left v) -> Aux $ Left $ Left v
            X i -> X i
        -- c1 :: CNF (Either (Either a Vcommander) (Int, Int, b))
        c1 =  flip concatMap [1..g] \i -> 
                let lits :: [Literal (Either a Vcommander)]
                    lits = -- [ lift $ literals !! (h-1) | h <- hss !! (i-1) ]
                        let 
                        in  lift
                        ++ [ c i j | j <- [1..k] ]
                in  liftLocal 1 i (atMost lits k)
                      ++ liftLocal 2 i (atLeastBy atMost lits k)
        c2 :: CNF (Either (Either a Vcommander) (Int, Int, b))
        c2 =  replaceRight
                [ [c i j, c i (j+1)]
                | i <- [1..g]
                , j <- [1 .. k-1]
                ]
        -- c3 :: CNF (Either (Either a Vcommander) (Int, Int, b))
        c3 = liftLocal 3 0 $ atMost [ c i j | i <- [1..g], j <- [1..k] ] k
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
