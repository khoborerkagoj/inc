As detailed in the instructions, we create a file fixnum.c. Compile this
file with `clang -S -m32 -fomit-frame-pointers fixnum.c`.  This will generate
a file `fixnum.s`, which you can confirm works. We will soon trim this file, so
copy fixnum.s to fixnumorig.s. I experimentally determined the minimum file
that CLANG will accept, which is the file `fixnum.s`. We can verify that both
of these files will work:

    clang -m32 -o Orig.exe startup.c fixnumorig.s
    clang -m32 -o Trim.exe startup.c fixnum.s

Executing either of the files `Orig.exe` or `Trim.exe` produces the desired
result of `7`.
