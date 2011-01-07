cl-anonfun
==========

cl-anonfun is a Common Lisp library that provides a simple anonymous function notation.

Usage
-----

### fn

    (fn &rest body)

`fn` macro returns `lambda` form of `body`. If special symbols starting with `%` are found in `body`, its symbols will be placed to lambda-list of `lambda` form. Special symbol `%` represents its first argument. Special symbol formed `%<n>` represents its `n`'th argument. Special symbol `%&` represents its rest of arguments.

#### Examples

    (macroexpand '(fn * % %))
    ; => #'(LAMBDA (%) (* % %))
    (funcall (fn * % %) 3)
    ; => 9
    (macroexpand '(fn mapcar %2 %1))
    ; => #'(LAMBDA (%1 %2) (mapcar %2 %1))
    (funcall (fn mapcar %2 %1) '(1 2 3) (fn * % %))
    ; => 1 4 9
    (macroexpand '(fn apply #'+ 1 2 3 %&))
    ; => #'(LAMBDA (&REST %&) (APPLY #'+ 1 2 3 %&))
    (funcall (fn apply #'+ 1 2 3 %&) 4 5)
    ; => 15

### fnn

    (fnn narg &rest body)

Like `fn` except that `fnn` macro can take a number of arguments of the anonymous functions.

#### Examples

    (macroexpand '(fnn 3 eq %2 1))
    ; => #'(LAMBDA (#:IGNORE_1_896 %2 #:IGNORE_3_897)
            (DECLARE (IGNORE #:IGNORE_1_896 #:IGNORE_3_897))
            (EQ %2 1))
    (funcall (fnn 3 eq %2 1) 3 1 2)
    ; => T

### enable-fn-syntax

    (enable-fn-syntax)

By calling this function, you can use special syntax `#%(...)` instead of `fn` macro. Any forms of `fn` macro can be used. If an integer is suppied after `#%`, `fnn` macro will be used with the integer instead of `fn`.

#### Examples

    (enable-fn-syntax)
    (funcall #%(* % %) 3)
    ; => 9
    (funcall #%3(eq %2 1) 3 1 2)
    ; => T

License
-------

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>.
Licensed under the LLGPL License.
