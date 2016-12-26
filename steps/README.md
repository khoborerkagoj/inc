# Notes
* Each subdirectory in this directory contain one step as described in the
  instructions. Though each step builds upon the previous, and there is hence
  some redundancy, this choice helps to see the increments in the "incremental
  contruction" of the Scheme compiler.
* The steps rely on being in the proper directory (because of the relative
  paths I use to load the infrastructure files). The Chez Scheme commands for
  this are `(current-directory)` to show the  current directory, and 
  `(current-directory "<absolute-or-relative-path">)` to change the directory.
* The batch file `startScheme.bat` has been created in this directory. To
  proceed, run the batch file; this will start scheme in this directory. You
  can then execute
  ```scheme
  (current-directory "step1")
  (load "compiler.scm")
  ```
  You will then be ready to execute `(test-all)` to run the tests.

Incidentally, I am not a Scheme expert, so if you have a better suggestion,
please let me know.
