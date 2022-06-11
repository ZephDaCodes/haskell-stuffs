module CompFinals where

-- The following files are my answers for COMP1100 S1 Finals

{-
rpPower:
Given two integers as input:

rpPower x 0 = 1
rpPower x 2 = x^x
rpPower x 3 = x^(x^x)

..etc
-}

rpPower :: Integer -> Integer -> Integer
rpPower n power = foldr (^) 1 (replicate (fromIntegral power) n)

{-
separate:
Given an Int and List, return a list of lists.

separate 3 [1,2,3,4,5] = [[1,2,3],[4,5]]
-}

separate :: Int -> [a] -> [[a]]
separate n list = case (n, list) of
    (_, [])   -> []
    (n, list)
     | n < 0     -> []
     | otherwise -> x : separate n xs
       where (x, xs) = splitAt n list


