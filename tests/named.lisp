;;;; named.lisp

(in-package #:alternatives-tests)

(nst:def-test-group selected-alternatives-named ()
  (nst:def-test no-clauses (:equal nil)
    (alt:alternatives* no-clauses))

  (nst:def-test only-***-clause (:equal :b)
    (alt:alternatives* only-***-clause
      (:a :a)
      ***
      (:b :b)
      (:c :c)))

  (nst:def-test first-***-clause (:equal :b)
    (alt:alternatives* first-***-clause
      (:a :a)
      ***
      (:b :b)
      ***
      (:c :c)))

  (nst:def-test only-tagged-***-clause (:equal :b)
    (alt:alternatives* only-tagged-***-clause
      (:a :a)
      (*** :b)
      (:c :c)))

  (nst:def-test first-tagged-***-clause (:equal :b)
    (alt:alternatives* first-tagged-***-clause
      (:a :a)
      ("***" :b)
      (*** :c)))

  (nst:def-test only-tagged-final-clause (:equal :b)
    (alt:alternatives* only-tagged-final-clause
      (:a :a)
      (final :b)
      (:c :c)))

  (nst:def-test first-tagged-final-clause (:equal :b)
    (alt:alternatives* first-tagged-final-clause
      ("A" :a)
      ("FINAL" :b)
      (final :c)))

  (nst:def-test only-tagged-blessed-clause (:equal :b)
    (alt:alternatives* only-tagged-blessed-clause
      (:a :a)
      (blessed :b)
      (:c :c)))

  (nst:def-test first-tagged-blessed-clause (:equal :b)
    (alt:alternatives* first-tagged-blessed-clause
      ("A" :a)
      ("BLESSED" :b)
      (blessed :c))))

(nst:def-test-group eliminates-docstrings-named ()
  (nst:def-test eliminates-docstring-before-body
      (:values (:each (:not (:predicate stringp)))
               :true)
    (macroexpand-1 '(alt:alternatives* eliminates-docstring-before-body
                     (:a
                      "docstring here"
                      :a))))

  (nst:def-test keeps-docstring-if-only-body (:equal "docstring here")
    (alt:alternatives* keeps-docstring-if-only-body
      (:a
       "docstring here"))))

(nst:def-test-group malformed-alternatives-named ()
  (nst:def-test non-***-symbol (:err)
    (alt:alternatives* non-***-symbol
      :abcd))

  (nst:def-test bad-tag (:err)
    (alt:alternatives* bad-tag
      (37 t))))

(nst:def-test-group documentation-named ()
  (nst:def-test symbol-named-alternative (:equal "A")
    (progn
      (alt:alternatives* symbol-named-alternative
        (:a
         :a))
      (documentation 'symbol-named-alternative 'alt:alternatives)))

  (nst:def-test string-named-alternative (:equal "a")
    (progn
      (alt:alternatives* string-named-alternative
        ("a"
         :a))
      (documentation 'string-named-alternative 'alt:alternatives)))

  (nst:def-test string-named-alternative-with-docstring (:equal "a
docstring here")
    (progn
      (alt:alternatives* string-named-alternative
        ("a"
         "docstring here"
         :a))
      (documentation 'string-named-alternative 'alt:alternatives)))
  )
