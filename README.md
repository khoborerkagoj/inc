Step-by-step development of a Scheme-to-x86 compiler, based on
Abdulaziz Ghuloum's [paper][1], _An Incremental Approach to Compiler
Construction_, and extended draft [tutorial][2], _Compilers: Backend to
Frontend and Back to Front Again_.

The CPS conversion is based on Matt Might's [web article][3], _How to
compile with continuations_.

This repository will contain the steps I used to go through the tutorial,
including compilation on windows. The actual code as well as the steps
differ in small ways from the paper and tutorial; I will detail those
steps as well.

[1]: https://github.com/namin/inc/blob/master/docs/paper.pdf?raw=true
[2]: https://github.com/namin/inc/blob/master/docs/tutorial.pdf?raw=true
[3]: http://matt.might.net/articles/cps-conversion/

### Running tests
See [src/README.md](src/README.md) for instructions on how to proceed and run
tests.
