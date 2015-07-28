
(import
 (chibi test)
 (comparators)
 (generators)
 (immutable set)
 (scheme base)
 (scheme write)
 (selector)
 (srfi 1)
 (srfi 26))

(let* ((cmp number-comparator)
       (s0 (iset cmp))
       (l3 '(2 4 6))
       (s3 (iset cmp 6 4 2))
       (always-false (lambda () #f))
       (at-least-3 (cute >= <> 3))
       (add1 (cute + 1 <>)))

  ;; iset
  (test '() (iset->list s0))
  (test '(2 4 6) (iset->list s3))

  ;; iset-tabulate
  (test '(1 4 9)
	(iset->list
	 (iset-tabulate cmp
			3
			(lambda (i)
			  (expt (+ 1 i) 2)))))

  ;; iset-unfold
  (test '(-2 -1 0)
	(iset->list
	 (iset-unfold cmp
		      (cute = 3 <>)
		      (cute - <>)
		      (cute + <> 1)
		      0)))

  ;; iset?
  (test #t (iset? s0))
  (test #t (iset? s3))
  (test #f (iset? '()))

  ;; iset-empty?
  (test #t (iset-empty? s0))
  (test #f (iset-empty? s3))

  ;; iset-member?
  (test #f (iset-member? s0 1))
  (test #f (iset-member? s3 1))
  (test #t (iset-member? s3 2))
  (test #f (iset-member? s3 3))
  (test #t (iset-member? s3 4))
  (test #f (iset-member? s3 5))
  (test #t (iset-member? s3 6))
  (test #f (iset-member? s3 7))

  ;; iset-min
  (test-error (iset-min s0))
  (test 2 (iset-min s3))

  ;; iset-max
  (test-error (iset-max s0))
  (test 6 (iset-max s3))

  ;; iset-comparator
  (test-assert (eqv? number-comparator (iset-comparator s3)))

  ;; iset-predecessor
  ;; iset-successor
  ;; TODO
  
  ;; iset-adjoin
  (let ((s (iset-adjoin s0 7)))
    (test '(7) (iset->list s))
    (test 1 (iset-size s)))
  (let ((s (iset-adjoin s3 1)))
    (test '(1 2 4 6) (iset->list s))
    (test 4 (iset-size s)))
  (let ((s (iset-adjoin s3 2)))
    (test '(2 4 6) (iset->list s))
    (test 3 (iset-size s)))

  ;; iset-adjoin-all
  (let ((s (iset-adjoin-all s3 '(8 -4 4))))
    (test '(-4 2 4 6 8) (iset->list s))
    (test 5 (iset-size s)))

  ;; iset-replace
  ;; effectual
  (let ((s (iset-replace s3 1)))
    (test '(1 2 4 6) (iset->list s))
    (test 4 (iset-size s)))
  ;; ineffectual
  (let ((s (iset-replace s3 4)))
    (test '(2 4 6) (iset->list s))
    (test 3 (iset-size s)))

  ;; iset-delete
  ;; effectual
  (let ((s (iset-delete s3 4)))
    (test '(2 6) (iset->list s))
    (test 2 (iset-size s)))
  ;; ineffectual
  (let ((s (iset-delete s3 5)))
    (test '(2 4 6) (iset->list s3))
    (test 3 (iset-size s)))

  ;; iset-delete-elements
  ;; effectual
  (let ((s (iset-delete-elements s3 '(5 4))))
    (test '(2 6) (iset->list s))
    (test 2 (iset-size s)))
  ;; ineffectual
  (let ((s (iset-delete-elements s3 '(5 1 3))))
    (test '(2 4 6) (iset->list s))
    (test 3 (iset-size s)))

  ;; iset-find
  ;; success
  (test 2 (iset-find s3 2 always-false))
  (test 4 (iset-find s3 4 always-false))
  (test 6 (iset-find s3 6 always-false))
  ;; failure
  (test #f (iset-find s3 1 always-false))
  (test #f (iset-find s3 3 always-false))
  (test #f (iset-find s3 5 always-false))
  (test #f (iset-find s3 7 always-false))

  ;; iset-count
  (test 3 (iset-count even? s3))
  (test 0 (iset-count odd? s3))

  ;; iset-any
  (test #t (iset-any even? s3))
  (test #f (iset-any odd? s3))

  ;; iset-every
  (test #t (iset-every even? s3))
  (test #f (iset-every odd? s3))

  ;; iset-range=
  ;; iset-range<
  ;; iset-range>
  ;; iset-range<=
  ;; iset-range>=
  ;; TODO

  ;; iset-filter
  (test '(4 6)
	(iset->list (iset-filter at-least-3 s3)))

  ;; iset-remove
  (test '(2)
	(iset->list (iset-remove at-least-3 s3)))

  ;; iset-partition
  (let-values (((yay nay) (iset-partition at-least-3 s3)))
    (test '(4 6) (iset->list yay))
    (test '(2) (iset->list nay)))

  ;; iset-fold
  (test (fold + 0 l3)
	(iset-fold + 0 s3))

  ;; iset-fold-right
  (test (fold-right cons '() l3)
	(iset-fold-right cons '() s3))

  ;; iset-map/monotone
  ;; same comparator
  (test '(3 5 7)
	(iset->list (iset-map/monotone add1 s3)))
  ;; new comparator
  (test '(2.0 4.0 6.0)
	(iset->list (iset-map/monotone inexact s3 real-comparator)))

  ;; iset-map
  ;; monotone tests work
  (test '(3 5 7)
	(iset->list (iset-map add1 s3)))
  (test '(2.0 4.0 6.0)
	(iset->list (iset-map inexact s3 real-comparator)))
  ;; non-monotone
  (test '(-6 -4 -2)
	(iset->list (iset-map - s3)))

  ;; iset-for-each
  (let ((sum 0))
    (iset-for-each (lambda (x)
		     (set! sum (+ sum x)))
		   s3)
    (test 12 sum))

  ;; iset=?
  ;; iset<?
  ;; iset>?
  ;; iset<=?
  ;; iset>=?
  ;; iset-union
  ;; iset-intersection
  ;; iset-difference
  ;; iset-xor
  ;; TODO

  ;; iset->list
  (test '() (iset->list s0))
  (test '(2 4 6) (iset->list s3))

  ;; increasing-list->iset
  (test '(1 2 3)
	(iset->list (increasing-list->iset cmp '(1 2 3))))

  ;; list->iset
  (test '(1 2 3)
	(iset->list (list->iset cmp '(3 2 1))))

  ;; iset->generator
  (test '(2 4 6)
	(generator->list (iset->generator s3)))

  ;; increasing-generator->iset
  (test '(1 2 3)
	(iset->list (increasing-generator->iset cmp (make-generator 1 2 3))))

  ;; generator->iset
  (test '(1 2 3)
	(iset->list (generator->iset cmp (make-generator 3 2 1))))
  
  ) ; let
