module Frequency where

import qualified Data.Map.Strict as M
import qualified Data.Heap as H
import qualified Data.ByteString as B
import Tree
import Data.Word (Word8)

type Table = M.Map Word8 Int


new :: Table
new = M.empty

listTable :: B.ByteString -> TreeHeap
listTable s = M.foldrWithKey
                (\w i t -> H.insert (i, (Node (Value w) Empty Empty)) t)
                (H.empty :: TreeHeap)
                (generateTable s new)

generateTable :: B.ByteString ->Table -> Table
generateTable bs t = case B.uncons bs of
                        Nothing -> t
                        (Just (w, bs')) -> generateTable bs' (
                            case M.lookup w t of
                                Nothing -> M.insert w 1 t
                                Just n -> M.insert w (n+1) t)