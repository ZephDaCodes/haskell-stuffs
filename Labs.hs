module Labs where

-- Computes factorials

factorial :: Integer -> Integer
factorial n
    |n == 0    = 1
    |n < 0     = error("factorial: Negative inputs are invalid")
    |otherwise = n * (factorial (n-1))

-- Finds length of list

lengthList :: [Integer] -> Integer
lengthList list = case list of
    []   -> 0
    _:xs -> 1 + lengthList xs

-- Joins two lists together

join :: [a] -> [a] -> [a]
join listA listB = case (listA, listB) of
    ([], ys) -> ys
    (x:xs, ys) -> x : join xs ys

-- Reverses a list
listRev :: [a] -> [a]
listRev num = revHelper [] num
    where
        revHelper :: [a] -> [a] -> [a]
        revHelper acc list = case list of
            [] -> acc
            x:xs -> revHelper ([x] ++ acc) xs