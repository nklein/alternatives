;;;; alternatives.lisp

(in-package #:alternatives-tests)

(nst:def-test-group selected-alternatives ()
  (nst:def-test only-***-clause (:equal :b)
    (alt:alternatives
      (:a :a)
      ***
      (:b :b)
      (:c :c)))

  (nst:def-test first-***-clause (:equal :b)
    (alt:alternatives
      (:a :a)
      ***
      (:b :b)
      ***
      (:c :c)))

  (nst:def-test only-tagged-***-clause (:equal :b)
    (alt:alternatives
      (:a :a)
      (*** :b)
      (:c :c)))

  (nst:def-test first-tagged-***-clause (:equal :b)
    (alt:alternatives
      (:a :a)
      ("***" :b)
      (*** :c)))

  (nst:def-test only-tagged-final-clause (:equal :b)
    (alt:alternatives
      (:a :a)
      (final :b)
      (:c :c)))

  (nst:def-test first-tagged-final-clause (:equal :b)
    (alt:alternatives
      ("A" :a)
      ("FINAL" :b)
      (final :c)))

  (nst:def-test only-tagged-blessed-clause (:equal :b)
    (alt:alternatives
      (:a :a)
      (blessed :b)
      (:c :c)))

  (nst:def-test first-tagged-blessed-clause (:equal :b)
    (alt:alternatives
      ("A" :a)
      ("BLESSED" :b)
      (blessed :c))))

(nst:def-test-group malformed-alternatives ()
  (nst:def-test non-***-symbol (:err)
    (alt:alternatives
      :abcd))

  (nst:def-test bad-tag (:err)
    (alt:alternatives
      (37 t))))
