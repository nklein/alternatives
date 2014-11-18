;;;; named.lisp

(in-package :alternatives)

(defmacro alternatives* (name &body clauses)
  "This macro chooses one of the alternative CLAUSES.  Each
alternative clause is of the form (TAG &BODY BODY) where the TAG is
either a SYMBOL or STRING.

If any clause is preceded by a symbol or string `***`, the macro
expands to the BODY of the first such clause.  Otherwise, if the TAG
of any clause is the symbol or string ***, FINAL, or BLESSED, then the
macro expands to the body of the first such clause.  Otherwise, the
macro expands to the body of the last alternative clause.

This macro also sets the documentation of type ALTERNATIVES for
the given NAME to the TAG and docstring of the BODY."

  (assert (every #'%valid-p clauses))

  (%destructure-clause (tag docstring body) (%find-selected-clause clauses)
    (setf (documentation name 'alternatives:alternatives)
          (format nil "~A~@[~%~A~]" tag docstring))
    `(progn
       ,@body)))
