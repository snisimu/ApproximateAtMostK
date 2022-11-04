{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Data.Maybe

import Base

commander
    :: Maybe (NumberConstraint a b c)
    -> Int
    -> NumberConstraint a Vcommander (Either (String, b) (String, c))
commander mbAtMost xs k = if length xs == k + 1 then [map not $ lifts xs] else
    let hss = splitBy s [1..n]
        g = length hss
        n = length xs
        s = min 10 $ n `div` 3
        c :: Int -> Int -> Literal (Either a (Either Vcommander b))
        c i j = (True, Aux $ Right $ Left $ C i j)
        arrange13
            :: String
            -> CNF (Either a (Either Vcommander b))
            -> CNF (Either a (Either Vcommander (Either (String, b) (String, c))))
        arrange13 strId = fmapCNF \case
            Left v -> Left v
            Right (Left v) -> Right $ Left v
            Right (Right v) -> Right $ Right $ Left (strId, v)
        arrange2
            :: CNF (Either a Vcommander)
            -> CNF (Either a (Either Vcommander (Either (String, b) (String, c))))
        arrange2 = fmapCNF \case 
            Left v -> Left v
            Right v -> Right $ Left v
        arrangeRecur
            :: CNF (Either a (Either Vcommander (Either (String, b) (String, c))))
            -> CNF (Either a (Either Vcommander (Either (String, Vcommander) (String, c))))
        arrangeRecur = fmapCNF \case
            Left v -> Left v
            Right (Left v) -> Right $ Right $ Left ("recur", v)
            Right (Right (Left v)) -> Right $ Right $ Right $ Left ("recur", v)
            Right (Right (Right v)) -> Right $ Right $ Right $ Right ("recur", v)
        tr1 :: NumberConstraint a Vcommander (Either (String, b) (String, c))
        tr1 = commander Nothing s
        tr2 :: NumberConstraint a Vcommander (Either (String, Vcommander) (String, c))
        tr2 = arrangeRecur . commander Nothing s
        atMost :: NumberConstraint a b c
        atMost = fromMaybe tr2 mbAtMost
        c1 :: CNF (Either a (Either Vcommander (Either (String, b) (String, c))))
        c1 =  flip concatMap [1..g] \i -> 
                let lits
                      =  [ lift $ xs !! (h-1) | h <- hss !! (i-1) ]
                      ++ [ not $ c i j | j <- [1..k] ]
                in  arrange13 ("c1-AM" ++ show i) (atMost lits k)
                      ++ arrange13 ("c1-AL" ++ show i) (atLeastBy atMost lits k)
        c2 :: CNF (Either a (Either Vcommander (Either (String, b) (String, c))))
        c2 =  arrange2
                [ [not $ c i j, c i (j+1)]
                | i <- [1..g]
                , j <- [1..k-1]
                ]
        c3 :: CNF (Either a (Either Vcommander (Either (String, b) (String, c))))
        c3 = arrange13 "c3" $ atMost [ c i j | i <- [1..g], j <- [1..k] ] k
    in  c1 ++ c2 ++ c3
