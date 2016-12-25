Inc: an incrementally developed compiler
=======================================

Setup instructions for Ubuntu x86
---------------------------------

- [Install Petite Chez Scheme](http://www.scheme.com/download/index.html#sec:petitechezscheme)
- `$ sudo apt-get install libc6-dev-i386`

Setup instructions for Windows
-------------------------------
- [Install Petite Chez Scheme](http://www.scheme.com/download)
    - The web site says that the version is out of date, but it worked for
      me; I installed the Win64 version.
- Install compiler; use the CLANG version from the
  [Clang download page](http://releases.llvm.org/download.html). Again, I
  installed the Win64 version.
- CLANG requires the libraries and header files from Visual Studio to be able
  compile on Windows. For this, I installed Visual Studio 2015 (community 
  edition).
- Why not use Visual Studio in the first place? There are a couple of reasons:
    - Visual Studio does not compile assembly language and C files in the
      same command, like CLANG and gcc do. 
    - Additionally, the scheme compiler setup (which we will look at soon)
      rely on a single compile command in order to do this. While it is
      possible to adjust the compiler commands, I just used this for
      convenience.
    - The assembly dialect used by Visual Studio is slightly different from
      what is documented.
    - However, I chose to use the Intel syntax as opposed to the documented
      AT&T syntax for the assembly language files. More details on this in
      the step1 readme.

- Now we will have to set up our environment. Visual Studio creates a shortcut
  that starts a command prompt with the appropriate environment variables set.
  We will have to ensure that the environment within which scheme is started
  can
    - Access the CLANG executables (the CLANG installation does add CLANG to
      the path).
    - Find the include files and other files from Visual Studio
    - Is in the correct directory.

  To do this, go to the directory where you will be doing the development. In
  this directory, create a batch file with the following contents (adjust the
  paths to suit your installations):


```batch
REM adjust the paths to suit your needs
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86
"C:\Program Files (x86)\Chez Scheme Version 8.4\bin\a6nt\petite.exe"
```

  Executing the batch file should start up Chez Scheme in the proper directory,
  with Visual Studio and CLANG properly set up.

# Using the compiler with CLANG
The original files (as forked from [here](https://github.com/namin/inc))
have the build happening with gcc. This section details the changes necessary
to work with CLANG and Visual Studio.

* We choose to build 32 bit executables, and the CLANG setup will reflect that.
  This is because we will then have to write 32 bit assembly. 
* Change the line in tests-drivers.scm from the original

```scheme
(unless (zero? (system (format "gcc -m32 -Wall -o stst ~a ~a stst.s"
```

to

```scheme
(unless (zero? (system (format "clang -m32 -Wall -o stst.exe ~a ~a stst.s"
```

* There are two definitions of the function `execute`. Both of these should
  change from `(unless (fxzero? (system "./stst > stst.out"))` to
  `(unless (fxzero? (system "stst.exe > stst.out"))`.

* The PDF documents ([paper](../docs/paper.pdf) and [tutorial](../docs/tutorial.pdf)) refer to creating a runtime file called
  `runtime.c`; the code in this repository assumes it is called
  `startup.c`. Use the file `startup.c` whenever you see `runtime.c`.

* The code also assumes a library file called `lib.s`. The first couple of
  assignments will not use it, but the tests will not run without it. Just
  create an empty file called `lib.s`.


Quick instructions to run all tests
-----------------------------------
    $ petite compiler.scm
      > (compile-lib)     ;; just once
      > (test-all)        ;; run all the tests

Full original instructions
--------------------------

To run the tests, make sure that your compiler file is called
`compiler.scm`, and that at the top of that file, you have:
`(load "tests-driver.scm")` ; this should come first
`(load "tests-1.1-req.scm")` ; and any other test files you may have.

Also, make sure that your compiler defines the function
`emit-program` that takes an expression and uses `emit` to emit the
appropriate instructions.

The `tests-driver` defines the procedure `test-all` that will run all
the tests provided, get the output, redirect it to a file `stst.s`,
and invokes gcc on that file as well as the startup.c file that you
should have written, and the `lib.s` file that is generated from the
`compile-lib` thunk, which you'll need to call once and every time you
change the `emit-library` thunk, which you can define.

The tests-driver is written for [Petite] Chez Scheme 7.  You can
obtain a copy of Petite Chez Scheme from:
  [http://www.scheme.com](http://www.scheme.com)

The `tests-driver` also assumes that you have the GNU C compiler `gcc`
already setup and added to your pathname.  How you do this depends
on your platform.  If you have a different C compiler that you wish
to use, you can edit the `tests-driver` yourself (look for the
definition of the `build` procedure).

If all is well, then invoking `petite` on your compiler and typing
`(test-all)` should run all the tests as in the following sample
transcript.

    $ petite compiler.scm
    Petite Chez Scheme Version 7.0a
    Copyright (c) 1985-2005 Cadence Research Systems

    > (test-all)
      test 0:#f ... ok
      test 1:#t ... ok
      test 2:() ... ok
      test 3:0 ... ok
      test 4:1 ... ok
      test 5:-1 ... ok
      test 6:2736 ... ok
      test 7:-2736 ... ok
      test 8:536870911 ... ok
      test 9:-536870912 ... ok
      test 10:#\nul ... ok
      ...
      test 131:#\y ... ok
      test 132:#\z ... ok
      test 133:#\{ ... ok
      test 134:#\| ... ok
      test 135:#\} ... ok
      test 136:#\~ ... ok
      test 137:#\rubout ... ok
      passed all 138 tests
    >

Enjoy.

Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
