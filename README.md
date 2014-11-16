## Alternatives (0.1.20141115)

When someone is writing code, there is often more than one way they
could accomplish a given task.  After they have explored a number of
options, they usually remove all but the final version from the code.
If they were being extraordinarily careful, there might be separate
commits in the version control system which contained the
alternatives.  If they were being extraordinarily forward-looking,
they may have left a comment in the code about what alternatives they
tried and why they settled on the one that they did.

Even if they did both of those things, there might be reasons you
would want to try out the alternatives again to see if the original
assumptions still hold.  You could futz around with version control
and try to bring the old versions of that function into the current
code base.  Or, you could try to rewrite and re-debug the alternatives
yourself.  But, wouldn't it be nice if the alternatives were still all
at your fingertips, at least while the code is still in active
development?

The `ALTERNATIVES` macro provides one way of accomplishing this.  One
can specify tagged alternatives.  The macro will expand into the body
of one of the alternative clauses.  Each alternative clause is of the
form `(TAG &BODY BODY)` where the `TAG` is either a `SYMBOL` or
`STRING`.

If any clause is preceded by a symbol or string `***`, the macro
expands to the `BODY` of the first such clause.  Otherwise, if the
`TAG` of any clause is the symbol or string `***`, `FINAL`, or
`BLESSED`, then the macro expands to the body of the first such
clause.  Otherwise, the macro expands to the body of the last
alternative clause.

In the following example, the macro expands into the body of the final
alternative clause:

    (defun sum-of-squares (n)
      (alternatives
        (i-wanted-to-do-this
         (loop :for i :to n :summing (* i i)))

        (my-first-attempt-was-something-like-this
         (do ((i 0 (1+ i))
              (sum 0 (+ sum (* i i))))
             ((> i n) sum)))

        (but-i-could-not-do-that-because
         "Some people find a do-loop too hard to read.")

        (now-i-know-better-and-can-do-this
         (/ (* n (1+ n) (1+ (+ n n)) 6)))))

If we wanted to quickly try out the `MY-FIRST-ATTEMPT-...` clause, we
could precede it with a `***`:

    (defun sum-of-squares (n)
      (alternatives
        (i-wanted-to-do-this
         (loop :for i :to n :summing (* i i)))

        ***
        (my-first-attempt-was-something-like-this
         (do ((i 0 (1+ i))
              (sum 0 (+ sum (* i i))))
             ((> i n) sum)))

        (but-i-could-not-do-that-because
         "Some people find a do-loop too hard to read.")

        (now-i-know-better-and-can-do-this
         (/ (* n (1+ n) (1+ (+ n n)) 6)))))
