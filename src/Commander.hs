{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Data.Maybe

import Base

data Vcommander
    = C Int Int
    deriving (Eq, Show)

{-
commander
    :: Maybe (NumberConstraint a b)
    -> Int
    -> NumberConstraint a (Either (String, b) Vcommander)
-}
commander mbAtMost s xs k = if length xs == k + 1 then [map not $ lifts xs] else
    let hss = splitBy s [1..n]
        g = length hss
        n = length xs
        c :: Int -> Int -> Literal (Either a Vcommander)
        c i j = (True, Aux $ Right $ C i j)
        arrange13
            :: String
            -> CNF (Either (Either a Vcommander) b)
            -> CNF (Either a (Either (String, b) Vcommander))
        arrange13 strId = fmapCNF \case
            Left (Left v) -> Left v
            Left (Right v) -> Right $ Right v
            Right v -> Right $ Left (strId, v)
        arrange2
            :: CNF (Either a Vcommander)
            -> CNF (Either a (Either (String, b) Vcommander))
        arrange2 = fmapCNF \case 
            Left v -> Left v
            Right v -> Right $ Right v
        {-
        arrangeRecur
            :: CNF (Either a (Either (String, b) c))
            -> CNF (Either a (Either (String, b) Vcommander))
        -}
        arrangeRecur = fmapCNF \case
            Left v -> Left v
            Right (Left v) -> Right $ Left v
            Right (Right v) -> Right $ Left ("recur", v)
        -- tr1 :: NumberConstraint a (Either (String, b) Vcommander)
        tr1 = commander Nothing s
        -- tr2 :: NumberConstraint a (Either (String, Vcommander) Vcommander)
        tr2 = arrangeRecur . commander Nothing s
        -- atMost :: NumberConstraint a b
        atMost = fromMaybe tr2 mbAtMost
        c1 =  flip concatMap [1..g] \i -> 
                let lits
                      =  [ lift $ xs !! (h-1) | h <- hss !! (i-1) ]
                      ++ [ not $ c i j | j <- [1..k] ]
                in  arrange13 ("c1-AM" ++ show i) (atMost lits k)
                      ++ arrange13 ("c1-AL" ++ show i) (atLeastBy atMost lits k)
        c2 :: CNF (Either a (Either (String, b) Vcommander))
        c2 =  arrange2
                [ [not $ c i j, c i (j+1)]
                | i <- [1..g]
                , j <- [1..k-1]
                ]
        c3 = arrange13 "c3" $ atMost [ c i j | i <- [1..g], j <- [1..k] ] k
    in  c1 ++ c2 ++ c3 
