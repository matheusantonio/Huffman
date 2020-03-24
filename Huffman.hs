module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.ByteString as B
import Compressor

import System.TimeIt

main :: IO ()
main = timeIt $ huffman =<< execParser opt
     where
         opt = info (config)
             ( fullDesc <> progDesc "A simple compressor using Huffman's compression algorithm")

data Option = Zip | Unzip

data Zipper = Zipper
    { origin :: String
    , destiny :: String
    , option :: Option}

config :: Parser Zipper
config = Zipper
       <$> strOption
           ( long "src"
          <> short 's'
          <> help "Original file to be compressed" )
       <*> strOption
           ( long "dest"
          <> short 'd'
          <> help "Destiny file to the compression")
       <*> flag Zip Unzip
           ( long "unzip"
          <> short 'u'
          <> help "Wheter it is a file to be decompressed or not")

huffman :: Zipper -> IO ()
huffman (Zipper o d Unzip) = do
    f <- B.readFile o
    let res = readTree f
    if res == B.empty then
        putStrLn $ "The decompression process went wrong, maybe you provided an unrecognizable file or the file is corrupted." 
    else do
        B.writeFile d res
        putStrLn $ "Decompression process succesful!"
huffman (Zipper o d Zip) = do
    f <- B.readFile o
    let res = writeTree f
    if res == B.empty then
        putStrLn $ "A problem occurred during the compression proccess! Please, try again or report the problem."
    else do
        B.writeFile d res
        putStrLn $ "Compression proccess successful!"