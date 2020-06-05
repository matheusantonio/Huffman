# Huffman Compression Algorithm
*An implementation of Huffman's compression algorithm for learning purposes.*

This is an implementation of Huffman's compressing algorithm written in Haskell to practice Functional Programming.

**Packages used**

- optparse-applicative package to recover the options by command line: (http://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html).
- TimeIt package to measure the execution time, since it is not as optmal as I want it to be yet.

**Instructions**

- Run the program with cabal in the command line inside the project folder: cabal run

- The following list of arguments are:
  
  - src or -s to specify the source file to be compressed/decompressed
  - dest or -d to specify the destiny file for the compression/decompression
  - (optional) unzip or -u to specify that you want to decompress the file. If you just want to compress it, ignore this option.
  
- When running with cabal, use two dashes before passing the arguments:
  
  - *cabal run -- --src files/original --dest files/destination -u*