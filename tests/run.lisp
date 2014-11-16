;;;; run.lisp

(in-package #:alternatives-tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package :alternatives-tests)))
