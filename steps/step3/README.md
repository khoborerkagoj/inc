# Notes

Implementation of the tutorial section 1.3, **Unary Primitives**.
The code is pretty straightforward.  The big thing to understand is the
`define-primitive` syntactic form, and the properties for a primitive symbol.

The assembly code is mostly straightforward. In comparison to the previous
step, some of the code has been clean up, with constants added for bit masks
and boolean values (instead of using immediate values).

# TODO
It would be nice to have a test for return type. For instance, it would not
make sense to compile the following:

```scheme
(emit-program '(fxadd1 (boolean? 32)))
```
yet this will compile just fine. We could have a scheme where we list the
return type of primitives, but this gets complicated when we are dealing with
more than primitives. Skipping this for now.


