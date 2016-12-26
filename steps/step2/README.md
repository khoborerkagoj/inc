# Notes

Nothing much here. The only thing is that you have to follow the instructions
and handle each case. However, if you examine the test file `tests-1.2-req.scm`,
we have to print each character as Scheme would output it. This works well for
the standard characters `A`, `@` and so on, but needs special handling for
things like `\n`, which should be printed `#\newline`. To determine what
Chez Scheme would print, I used the following code:

~~~scheme
(do ([i 0 (+ 1 i)]) ((> i 255) 0)
      (display (format "~s ~s\n" i (integer->char i))))
~~~

As a result of this, the following were added:
Character | String
----------|-------
0         | `nul`
7         | `alarm`
8 (`\b`)  | `backspace`
9 (`\t`)  | `tab`
10 (`\n`) | `newline`
11        | `vtab`
12        | `page`
13 (`\r`) | `return`
32 (`' '`)| `space`
127       | `delete`

Note that all of these are *not* tested, but better to be future proof.
