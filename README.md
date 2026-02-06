# 3rd Order Context Model File Compression Algorithm

Uses a 16 MiB Dictionary to store the possible values, which is trained and updated by the file(s).  
_Can also learn and encode a file which is similar to others._  
Example:  
* If a file 'B.dat' is similar to a file 'A.dat', we can encode 'B.dat' by depending on the trained context model by file 'A.dat': `3f -a A.dat B.dat`  
* In some cases, a trained model can be used to compress a file further, and we can save the trained model in a compressed file: `3f -a a.txt -od a.txt.dictionary a.txt`  
Decompression: `3f -x -d a.txt.dictionary a.txt.3f`
