;;;; exports.lisp

(in-package #:alternatives-tests)

(nst:def-test-group basic-tests-of-package ()
  (nst:def-test exported-alternatives (:equal :external)
    (nth-value 1 (find-symbol "ALTERNATIVES" :ALTERNATIVES)))

  (nst:def-test alternatives-is-macro (:true)
    (macro-function (find-symbol "ALTERNATIVES" :ALTERNATIVES)))

  (nst:def-test exported-alternatives* (:equal :external)
    (nth-value 1 (find-symbol "ALTERNATIVES*" :ALTERNATIVES)))

  (nst:def-test alternatives*-is-macro (:true)
    (macro-function (find-symbol "ALTERNATIVES*" :ALTERNATIVES)))

  (nst:def-test package-nickname (:true)
    (eq (find-package "ALTERNATIVES")
        (find-package "ALT"))))
