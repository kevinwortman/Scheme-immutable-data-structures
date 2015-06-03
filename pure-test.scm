
(import
 (chibi test)
 (generators)
 (pure)
 (scheme base)
 (scheme write)
 (srfi 1)
 (srfi 26))

(define n (expt 10 2)) ; moderately large n

(let ((d0 (ideque))
      (d1 (ideque 1))
      (d3 (ideque 1 2 3))
      (d9 (ideque 1 2 3 4 5 6 7 8 9)))

  ;; ideque
  (test-assert (procedure? ideque))
  (test '() (ideque->list d0))
  (test '(1) (ideque->list d1))
  (test '(1 2 3) (ideque->list d3))

  ;; ideque-tabulate
  (test-assert (procedure? ideque-tabulate))
  (test '(2 3 4) (ideque->list (ideque-tabulate 3 (cute + 2 <>))))

  ;; TODO ideque-unfold ideque-unfold-right

  (test-assert (procedure? ideque?))
  (test #t (ideque? d0))
  (test #t (ideque? d1))
  (test #t (ideque? d3))
  (test #t (ideque? d9))
  (test #f (ideque? '()))
  (test #f (ideque? (finger-tree/empty)))

  (test-assert (procedure? ideque-empty?))
  (test #t (ideque-empty? d0))
  (test #f (ideque-empty? d1))
  (test #f (ideque-empty? d3))
  (test #f (ideque-empty? d9))

  (test-assert (procedure? ideque-front))
  (test-error (ideque-front d0))
  (test 1 (ideque-front d1))
  (test 1 (ideque-front d3))
  (test 1 (ideque-front d9))

  (test-assert (procedure? ideque-back))
  (test-error (ideque-back d0))
  (test 1 (ideque-back d1))
  (test 3 (ideque-back d3))
  (test 9 (ideque-back d9))

  (test-assert (procedure? ideque-remove-front))
  (test-error (ideque-remove-front d0))
  (test '() (ideque->list (ideque-remove-front d1)))
  (test '(2 3) (ideque->list (ideque-remove-front d3)))
  (test '(2 3 4 5 6 7 8 9) (ideque->list (ideque-remove-front d9)))

  (test-assert (procedure? ideque-remove-back))
  (test-error (ideque-remove-back d0))
  (test '() (ideque->list (ideque-remove-back d1)))
  (test '(1 2) (ideque->list (ideque-remove-back d3)))
  (test '(1 2 3 4 5 6 7 8) (ideque->list (ideque-remove-back d9)))

  (test-assert (procedure? ideque-add-front))
  (test '(0) (ideque->list (ideque-add-front d0 0)))
  (test '(0 1) (ideque->list (ideque-add-front d1 0)))
  (test '(0 1 2 3) (ideque->list (ideque-add-front d3 0)))
  (test '(0 1 2 3 4 5 6 7 8 9) (ideque->list (ideque-add-front d9 0)))

  (test-assert (procedure? ideque-add-back))
  (test '(0) (ideque->list (ideque-add-back d0 0)))
  (test '(1 0) (ideque->list (ideque-add-back d1 0)))
  (test '(1 2 3 0) (ideque->list (ideque-add-back d3 0)))
  (test '(1 2 3 4 5 6 7 8 9 0) (ideque->list (ideque-add-back d9 0)))

  (test-assert (procedure? ideque-take))
  (test '() (ideque->list (ideque-take d3 0)))
  (test '(1 2 3) (ideque->list (ideque-take d3 3)))

  ) ; TODO
