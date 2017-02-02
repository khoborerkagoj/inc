Inc: an incrementally developed compiler
=======================================

## Quick instructions to run all tests
    $ petite compiler.scm
      > (compile-lib)     ;; just once
      > (test-all)        ;; run all the tests

# Full original instructions
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

# Setup instructions for Windows
- [Install Petite Chez Scheme](http://www.scheme.com/download)
    - The web site says that the version is out of date, but it worked for
      me; I installed the Win64 version.
- Install compiler; use the CLANG version from the
  [Clang download page](http://releases.llvm.org/download.html). Again, I
  installed the Win64 version.
- CLANG requires the libraries and header files from Visual Studio to be able
  to compile on Windows. For this, I installed Visual Studio 2015 (community
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

* The PDF documents ([paper](../docs/paper.pdf) and
  [tutorial](../docs/tutorial.pdf)) refer to creating a runtime file called
  `runtime.c`; the code in this repository assumes it is called
  `startup.c`. Use the file `startup.c` whenever you see `runtime.c`.

* The code also assumes a library file called `lib.s`. The first couple of
  assignments will not use it, but the tests will not run without it. Just
  create an empty file called `lib.s`.

* The instructions also ask to include the driver file `test-driver.scm`, while
  the code actually has the file `tests-driver.scm` (note the added `s`).


# Running fewer tests
While the `(test-all)` command works and is comprehensive, it is onerous: by
step 7, you will be running close to 500 tests.  Additionally, before you can
even run the tests, you should look at the output of some "test" expressions.
We document some commands that allow you to run parts of tests as well as test
expressions interactively.

## Filtering the results of `test-all`
When  you load the compiler with the tests included, a variable called
`all-tests` is created. This contains a list of "categories" of tests and the
actual tests as well (in the list below, the results are heavily truncated).
```scheme
> all-tests
(("boolean?" ((boolean? #t) string "#t\n") ((boolean? #f) string "#t\n")
   ((boolean? 0) string "#f\n") ((boolean? 1) string "#f\n")
   ((boolean? -1) string "#f\n") ((boolean? ()) string "#f\n")
   ((boolean? #\a) string "#f\n")
   ((boolean? (boolean? 0)) string "#t\n")
   ((boolean? (fixnum? (boolean? 0))) string "#t\n"))
  ("if" ((if #t 12 13) string "12\n") ((if #f 12 13) string "13\n")
    ((if 0 12 13) string "12\n") ((if () 43 ()) string "43\n")
    ((if #t (if 12 13 4) 17) string "13\n")
    ((if #\X (if 1 2 3) (if 4 5 6)) string "2\n")
    ((if (not (boolean? #t)) 15 (boolean? #f)) string "#t\n")
    ((if (if (char? #\a) (boolean? #\b) (fixnum? #\c)) 119 -23)
      string
      "-23\n")
    ((not (if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7))
      string
      "#f\n")
    ((if (char? 12) 13 14) string "14\n")
    ((if (char? #\a) 13 14) string "13\n")
    ((fxadd1 (if (fxsub1 1) (fxsub1 13) 14)) string "13\n"))
  ("if" ((if (fx= 12 13) 12 13) string "13\n")
    ((if (fx= 12 12) 13 14) string "13\n")
    ((if (fx< 12 13) 12 13) string "12\n")
    ((if (fx< 12 12) 13 14) string "14\n")
    ((if (fx< 13 12) 13 14) string "14\n")
    ((if (fx>= 12 12) 12 13) string "12\n")
    ((if (fx>= 13 12) 13 14) string "13\n")))
```

It is tiresome to run all the tests when you have a failure on test 153. For
this reason, we created an additional library `tests-filter.scm` which can
show the "test categories" defined, allow you to filter them, and then reset
back to original. Once bugs are fixed, you should run all tests!

You will need to load this library explicitly.

```scheme
> (show-tests)
  ("integers" "immediate constants" "fxlognot" "not" "char?"
   "boolean?" "null?" "fxzero?" "fixnum?"
   "fixnum->char and char->fixnum" "fxsub1" "fxadd1" "and/or"
   "if" "binary primitives" "if" "fx>=" "fx>" "fx<=" "fx<"
   "fx=" "fxlogand and fxlogor" "fx*" "fx-" "fx+")
```
Filter to only have a few:
```scheme
> (filter-tests "binary primitives" "fx*" "boolean?")
> (show-tests)
  ("boolean?" "binary primitives" "fx*")
```
You can then use `(test-all)` to run these tests. To reset to the original list
(perhaps to choose a different subset), run
```Scheme
> (reset-tests)
> (show-tests)
  ("integers" "immediate constants" "fxlognot" "not" "char?"
   "boolean?" "null?" "fxzero?" "fixnum?"
   "fixnum->char and char->fixnum" "fxsub1" "fxadd1" "and/or"
   "if" "binary primitives" "if" "fx>=" "fx>" "fx<=" "fx<"
   "fx=" "fxlogand and fxlogor" "fx*" "fx-" "fx+")
```

## Running tests interactively
Sometimes we even cannot get as far as filtering out test categories: we have
an issue, or suspect we have an issue, with a particular construction. When we
have just written the code, we wish to test it with particular constructions.
The original `tests-drivers` does contain most of what we need; however it is
not documented. So here goes the documentation.
For this example, I will use the expression
`(letrec ([f (lambda (n) (fx* n n))]) (f (fx+ 2 3)))` as our test.
```Scheme
(define expr '(letrec ([f (lambda (n) (fx* n n))]) (f (fx+ 2 3))))
;; Can also use emit-program, which is our procedure
(compile-program expr) ; Outputs the assembly to the screen.
(run-compile expr)     ; Outputs the assembly to stst.s
(build)                ; Compiles previously generated code into an Executable
(build-program expr)   ; Calls run-compile and then build, generates exe.
(execute)              ; runs stst.exe, output to stst.output
(get-string)           ; Reads stst.out and returns it as string.
```
For convenience, we also created a procedure `test-expr` that generates,
compiles, executes and then returns the value. This is essentially what
`test-all` does before comparing the returned result to a stored value.
`test-expr` is included within `tests-drivers.scm`.
```Scheme
(define expr '(letrec ([f (lambda (n) (fx* n n))]) (f (fx+ 2 3))))
(define (test-expr expr)
  (run-compile expr)                    ; output to stst.s
  (build)                               ; make the exe out of it
  (execute)                             ; run the executable
  (get-string))                         ; get the string output and return it
(test-expr expr)
=> "25\n"
```
### Loading expressions from file
As we move forward with the compiler, the expressions we would
like to test with get more complicated. As a result, it is painful to
type them directly in the REPL. To this end, we authored a function called
`expr-from-file` that read an expression from a file and evaluates the
resulting string. This is available as part of `compiler.scm` starting at step
8.

Make sure you quote the expression in the file! To use, let
us say the file `test.scm` contains the following:
```scheme
'(letrec ([f (lambda (n) (if (fx< n 3) 0 (g (fx- n 2) 0 1)))]
          [g (lambda (n f1 f2)
               (if (fxzero? n) f2 (g (fx- n 1) f2 (fx+ f1 f2))))])
  (f 20))
```
which provides a procedure `f` that computes a Fibonacci number. Note that the
`letrec` is quoted. You can then call
`(define expr (expr-from-file "test.scm")` which will return the expression.
You can then `(compile-program expr)` or `(test-expr expr)`. You can even
`(eval expr)` to evaluate it in your current Scheme REPL.

The implementation of `expr-from-file` is:
```scheme
(define (expr-from-file fn)
  ;; Read the expression from file with name fn
  (with-input-from-file fn (lambda () (eval (read)))))
```
