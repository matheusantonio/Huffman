module Tree where

import Data.Word (Word8)

data Value = Value Word8 | NotValue deriving (Show, Read)

data Tree = Node Value Tree Tree 
          | Empty 
          deriving (Show, Read)

{- 
height :: Tree -> Int
height Empty = 0
height (Node _ t1 t2) = if h1 > h2 then h1 + 1 else h2 + 1
                        where
                            h1 = height t1
                            h2 = height t2 -}


find :: Tree -> Word8 -> [Int]
find (Node (Value v) _ _) s
    | v == s = []
    | otherwise = [-1]
find (Node NotValue t1 t2) s
    | null res1 = 0 : res1
    | null res2 = 1 : res2
    | last res1 == -1 = 1 : res2
    | last res2 == -1 = 0 : res1
    | otherwise = [5]
    where
        res1 = find t1 s
        res2 = find t2 s

recover :: Tree -> [Int] -> (Word8, [Int])
recover (Node (Value v) t1 t2) [] = (v, [])
recover (Node (Value v) t1 t2) (x:xs) = (v, (x:xs))
recover (Node NotValue t1 t2) (x:xs)
    | x == 0 = recover t1 xs
    | x == 1 = recover t2 xs
