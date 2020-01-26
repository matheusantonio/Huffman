module Frequency where

import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Tree
import Data.Word (Word8)

{- type Table = M.Map Char Int

new :: Table
new = M.empty

listTable :: [Char] -> [(Char, Int)]
listTable s = M.toList $ generateTable s new

generateTable :: [Char] ->Table -> Table
generateTable [] t = t
generateTable (k:xs) t = generateTable xs (case M.lookup k t of
                Nothing -> M.insert k 1 t
                Just n -> M.insert k (n+1) t) -}

type Table = M.Map Word8 Int

new :: Table
new = M.empty

listTable :: B.ByteString -> [(Int, Tree)]
listTable s = invert (M.toList (generateTable s new))

generateTable :: B.ByteString ->Table -> Table
generateTable bs t = case B.uncons bs of
                        Nothing -> t
                        (Just (w, bs')) -> generateTable bs' (
                            case M.lookup w t of
                                Nothing -> M.insert w 1 t
                                Just n -> M.insert w (n+1) t)

invert :: [(Word8, Int)] -> [(Int, Tree)]
invert [] = []
invert ((w, i):xs) = (i, (Node (Value w) Empty Empty)) : invert xs