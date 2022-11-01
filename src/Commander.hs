{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Base

data Vcommander
    = C Int Int
    deriving (Eq, Show)

commander :: NumberConstraint (Either a Vcommander) b -> Int -> NumberConstraint a (Either (String, b) Vcommander)
commander atMost s literals k =
    let hss = splitBy s [1..n]
        g = length hss
        n = length literals
        c :: Int -> Int -> Literal (Either a Vcommander)
        c i j = (True, Aux $ Right $ C i j)
        arrange13 :: String -> CNF (Either (Either a Vcommander) b) -> CNF (Either a (Either (String, b) Vcommander))
        arrange13 str = map \lits ->
          flip map lits $ fmap \case
            X i -> X i
            Aux (Left (Left v)) -> Aux $ Left v
            Aux (Left (Right v)) -> Aux $ Right $ Right v
            Aux (Right v) -> Aux $ Right $ Left (str, v)
        arrange2 :: CNF (Either a Vcommander) -> CNF (Either a (Either (String, b) Vcommander))
        arrange2 = map \lits ->
          flip map lits $ fmap \case 
            X i -> X i
            Aux (Left v) -> Aux $ Left v
            Aux (Right v) -> Aux $ Right $ Right v
        c1 =  flip concatMap [1..g] \i -> 
                let lits
                      =  [ lift $ literals !! (h-1) | h <- hss !! (i-1) ]
                      ++ [ not $ c i j | j <- [1..k] ]
                in  arrange13 ("c1-AM" ++ show i) (atMost lits k)
                      ++ arrange13 ("c1-AL" ++ show i) (atLeastBy atMost lits k)
        c2 :: CNF (Either a (Either (String, b) Vcommander))
        c2 =  arrange2
                [ [not $ c i j, c i (j+1)]
                | i <- [1..g]
                , j <- [1 .. k-1]
                ]
        c3 = arrange13 "c3" $ atMost [ c i j | i <- [1..g], j <- [1..k] ] k
    in  c1 ++ c2 ++ c3 
