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


(defun %find-selected-clause (clauses)
  (anaphora:acond
    ((member-if #'%is-***-p clauses)
     (first (rest anaphora:it)))

    ((find-if #'%final-clause-p clauses)
     anaphora:it)

    ((last clauses)
     (first anaphora:it))))

(defmacro %destructure-clause ((tagv docstringv bodyv) clause &body body)
  (let ((all (gensym "ALL-"))
        (tag (gensym "TAG-"))
        (rest (gensym "REST-"))
        (doc (gensym "DOC-"))
        (bod (gensym "BOD-")))
    `(let* ((,all ,clause)
            (,tag (first ,all))
            (,rest (rest ,all))
            (,doc (and (< 1 (length ,rest))
                       (stringp (first ,rest))
                       (first ,rest)))
            (,bod (if ,doc
                     (rest ,rest)
                     ,rest)))
       (let ((,tagv ,tag)
             (,docstringv ,doc)
             (,bodyv ,bod))
         ,@body))))

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

  (%destructure-clause (tag docstring body) (%find-selected-clause clauses)
    (declare (ignore tag docstring))
    `(progn
       ,@body)))
