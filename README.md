**Huffman Compression Algorithm**

This is an implementation of Huffman's compression algorithm written in Haskell for learning purposes.

It uses optparse-applicative package to recover the options by command line (http://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html).

I'm also using TimeIt package to measure the execution time, since it is not as optmal as I want it to be yet.

**Instructions**:
    When running the Huffman.hs file, pass the following arguments:
    - src or -s to specify the source file to be compressed/decompressed
    - dest or -d to specify the destiny file for the compression/decompression
    - unzip or -u to specify that you want to decompress the file. If you just want to compress it, ignore this option.