;;;; alternatives.lisp

(in-package :alternatives)

(defun %valid-p (clause)
  (cond
    ((symbolp clause)
     (unless (string= "***" (symbol-name clause))
       (error "Unexpected symbol ~A." clause))
     t)

    ((listp clause)
     (let ((tag (first clause)))
       (unless (or (symbolp tag)
                   (stringp tag))
         (error "Unexpected tag ~S.  Tags must be a SYMBOL or STRING."
                tag)))
     t)

    (t
     (error "Unexpected clause ~S." clause))))

(defgeneric %to-string (item)
  (:method ((item string))
    item)

  (:method ((item symbol))
    (symbol-name item)))

(defun %is-***-p (item)
  (when (atom item)
    (string= "***" (%to-string item))))

(defun %final-clause-p (clause)
  (when (listp clause)
    (destructuring-bind (tag &body body) clause
      (when (member (%to-string tag) '("***" "FINAL" "BLESSED")
                    :test #'string=)
        body))))

(defmacro alternatives (&body clauses)
  "This macro chooses one of the alternative CLAUSES.  Each
alternative clause is of the form (TAG &BODY BODY) where the TAG is
either a SYMBOL or STRING.

If any clause is preceded by a symbol or string `***`, the macro
expands to the BODY of the first such clause.  Otherwise, if the TAG
of any clause is the symbol or string ***, FINAL, or BLESSED, then the
macro expands to the body of the first such clause.  Otherwise, the
macro expands to the body of the last alternative clause."

  (assert (every #'%valid-p clauses))

  (anaphora:acond
    ((member-if #'%is-***-p clauses)
     (let ((clause (first (rest anaphora:it))))
       `(progn
          ,@(rest clause))))

    ((find-if #'%final-clause-p clauses)
     `(progn
        ,@(rest anaphora:it)))

    ((last clauses)
     `(progn
        ,@(rest (first anaphora:it))))))
