module Tree where

import Data.Word (Word8)
import qualified Data.Heap as H

data Value = Value Word8 | NotValue deriving (Show, Read)

data Tree = Node Value Tree Tree 
          | Empty 
          deriving (Show, Read)

type TreeHeap = H.MinPrioHeap Int Tree

find :: Tree -> Word8 -> [Int]
find (Node (Value v) _ _) s
    | v == s = []
    | otherwise = [-1]
find (Node NotValue t1 t2) s
    | null res1 = 0 : res1
    | null res2 = 1 : res2
    | last res1 == -1 = 1 : res2
    | last res2 == -1 = 0 : res1
    -- | otherwise = [5]
    where
        res1 = find t1 s
        res2 = find t2 s

recover :: Tree -> [Int] -> (Word8, [Int])
recover (Node (Value v) t1 t2) [] = (v, [])
recover (Node (Value v) t1 t2) (x:xs) = (v, (x:xs))
recover (Node NotValue t1 t2) (x:xs)
    | x == 0 = recover t1 xs
    | x == 1 = recover t2 xs

buildTree :: TreeHeap -> Tree
buildTree heap = case H.view heap of
    Just ((pos1, tree1), heap1) -> case H.view heap1 of
        Nothing -> tree1
        Just ((pos2, tree2), heap2) -> do
            let newTree = (Node NotValue tree2 tree1)
            buildTree $ H.insert (pos1+pos2, newTree) heap2