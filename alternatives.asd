;;;; alternatives.asd

(asdf:defsystem #:alternatives
  :description "Macro for specifying alternatives when coding."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20141117"
  :license "Free"
  :depends-on (:anaphora)
  :components ((:static-file "README.md")
               (:file "package")
               (:file "alternatives" :depends-on ("package"))
               (:file "named" :depends-on ("package"
                                           "alternatives"))))

(asdf:defsystem #:alternatives-tests
  :description "Tests for the alternatives library."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20141117"
  :license "Free"
  :depends-on (#:alternatives #:nst)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "run" :depends-on ("package"))
                             (:file "exports" :depends-on ("package"))
                             (:file "alternatives" :depends-on ("package"
                                                                "exports"))
                             (:file "named" :depends-on ("package"
                                                         "exports"
                                                         "alternatives"))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :alternatives))))
  (asdf:load-system :alternatives-tests)
  (funcall (find-symbol (symbol-name :run-tests) :alternatives-tests)))
