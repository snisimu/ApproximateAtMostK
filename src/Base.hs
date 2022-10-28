module Base where

combinations :: [a] -> Int -> [[a]]
combinations xs n | n > 0 = 
    go n (length (take n xs) == n) (drop n xs) xs
    where
    go n b p ~(x:xs)
        | n == 0 = [[]]
        | not b  = []
        | null p = [(x:xs)]
        | otherwise = map (x:) (go (n-1) b p xs)
                        ++ go n b (tail p) xs

data Var a
    = X Int
    | New a
    deriving (Eq, Show)

newtype CNF a = [[(Bool, Var a)]]

data Result a = Result { newvars :: [Var a], cnf :: CNF a}
