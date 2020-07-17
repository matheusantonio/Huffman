module Compressor where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.Bits
import Tree
import Data.ByteString.Char8 (pack, unpack)
import Frequency (listTable)

zip :: B.ByteString -> B.ByteString
zip b = do
    let tree = buildTree $ listTable b
    let bits = compress b tree
    B.append (pack (show tree)) (B.append (pack "$") bits)
    
compress :: B.ByteString -> Tree -> B.ByteString
compress b t = do
    let compressedBits = compress' b t
    let ignoredBits = 8 - ( mod(length compressedBits) 8 )
    let result = (take ignoredBits (repeat 1)) ++ compressedBits
    B.cons (fromIntegral ignoredBits) (buildWord result)

buildWord :: [Int] -> B.ByteString
buildWord [] = B.empty
buildWord w = B.cons (fromIntegral (shiftBits byte) :: Word8) (buildWord rest)
            where
                (byte, rest) = splitAt 8 w
    
compress' :: B.ByteString -> Tree -> [Int]
compress' b t = case B.uncons b of
    Just (w, bs) -> (find t w) ++ compress' bs t
    Nothing -> []

shiftBits :: [Int] -> Int
shiftBits [] = 0
shiftBits (x:xs) = (x `shiftL` (length (x:xs) -1)) .|. shiftBits xs


{- -------------------------- -}
unzip :: B.ByteString -> B.ByteString
unzip b = do
    let (t, rest) = B.span (\x -> x /= B.head (pack "$")) b
    let tree' = read $ unpack t :: Tree
    case B.uncons (B.tail rest) of
        Nothing -> B.empty
        Just (ignored, compressed) -> do
            let bits = drop (fromIntegral (toInteger ignored)) (recoverBits compressed)
            recoverData tree' bits

recoverData :: Tree -> [Int] -> B.ByteString
recoverData _ [] = B.empty
recoverData t i = B.cons w (recoverData t res)
                where
                    (w, res) = recover t i

recoverBits :: B.ByteString -> [Int]
recoverBits bs = case B.uncons bs of
                    Nothing -> []
                    Just (w, rest) -> adjust (intBit (fromInteger (toInteger w))) ++ recoverBits rest

adjust :: [Int] -> [Int]
adjust i
    | length i == 8 = i
    | otherwise = take n (repeat 0) ++ i
                where n = 8 - length i

intBit :: Int -> [Int]
intBit 0 = []
intBit i = intBit res ++ [val]
           where
               res = quot i 2
               val = mod i 2