
(import
 (chibi test)
 (scheme generator)
 (immutable deque)
 (scheme base)
 (scheme write) ; TODO: remove
 (srfi 1)
 (srfi 8)
 (srfi 26))

(let* ((d0 (ideque))
       (d1 (ideque 1))
       (l1 '(1))
       (d5 (ideque 1 2 3 4 5))
       (l5 '(1 2 3 4 5))
       (l100 (iota 100))
       (d100 (list->ideque l100)))

  ;; ideque
  (test-assert (ideque? d0))
  (test '() (ideque->list d0))
  (test-assert (ideque? d1))
  (test '(1) (ideque->list d1))
  (test-assert (ideque? d5))
  (test '(1 2 3 4 5) (ideque->list d5))
  (test-assert (ideque? d100))
  (test l100 (ideque->list d100))

  ;; ideque-tabulate
  (test (list-tabulate 100 -)
	(ideque->list (ideque-tabulate 100 -)))

  ;; ideque-unfold
  ;; ideque-unfold-right
  ;; first 4 perfect squares
  (let ((stop? (cute > <> 4))
	(mapper (cute expt <> 2))
	(successor (cute + <> 1))
	(seed 1))
    (test '(1 4 9 16) (ideque->list (ideque-unfold stop? mapper successor seed)))
    (test '(16 9 4 1) (ideque->list (ideque-unfold-right stop? mapper successor seed))))

  ;; ideque?
  (test #t (ideque? d0))
  (test #t (ideque? d1))
  (test #t (ideque? d5))
  (test #t (ideque? d100))
  (test #f (ideque? '()))
  (test #f (ideque? 42))

  ;; ideque-empty?
  (test #t (ideque-empty? d0))
  (test #f (ideque-empty? d1))
  (test #f (ideque-empty? d5))
  (test #f (ideque-empty? d100))

  ;; ideque-front
  (test-error (ideque-front d0))
  (test 1 (ideque-front d1))
  (test 1 (ideque-front d5))
  (test 0 (ideque-front d100))

  ;; ideque-back
  (test-error (ideque-back d0))
  (test 1 (ideque-back d1))
  (test 5 (ideque-back d5))
  (test 99 (ideque-back d100))

  ;; ideque-remove-front
  (test-error (ideque-remove-front d0))
  (test '() (ideque->list (ideque-remove-front d1)))
  (test '(2 3 4 5) (ideque->list (ideque-remove-front d5)))
  (test (cdr l100) (ideque->list (ideque-remove-front d100)))

  ;; ideque-remove-back
  (test-error (ideque-remove-back d0))
  (test '() (ideque->list (ideque-remove-back d1)))
  (test '(1 2 3 4) (ideque->list (ideque-remove-back d5)))
  (test (take l100 99) (ideque->list (ideque-remove-back d100)))

  ;; ideque-add-front
  (test '(9) (ideque->list (ideque-add-front d0 9)))
  (test '(9 1) (ideque->list (ideque-add-front d1 9)))
  (test '(9 1 2 3 4 5) (ideque->list (ideque-add-front d5 9)))
  (test (cons 9 l100) (ideque->list (ideque-add-front d100 9)))

  ;; ideque-add-back
  (test '(9) (ideque->list (ideque-add-back d0 9)))
  (test '(1 9) (ideque->list (ideque-add-back d1 9)))
  (test '(1 2 3 4 5 9) (ideque->list (ideque-add-back d5 9)))
  (test (append l100 (list 9)) (ideque->list (ideque-add-back d100 9)))

  (do ((i 0 (+ 1 i)))
      ((= i 10))

    (let* ((li (iota i))
	   (di (list->ideque li)))

      ;; ideque-take
      (test (take l100 i) (ideque->list (ideque-take d100 i)))

      ;; ideque-take-right
      (test (take-right l100 i) (ideque->list (ideque-take-right d100 i)))

      ;; ideque-drop
      (test (drop l100 i) (ideque->list (ideque-drop d100 i)))

      ;; ideque-drop-right
      (test (drop-right l100 i) (ideque->list (ideque-drop-right d100 i)))

      ;; ideque-split-at
      (test-values (split-at l100 i)
		   (receive (l r) (ideque-split-at d100 i)
			    (values (ideque->list l) (ideque->list r))))

      ;; ideque-length
      (test i (ideque-length (list->ideque (iota i))))

      ;; ideque-append
      (test (append l1 li) (ideque->list (ideque-append d1 di)))
      (test (append l5 li) (ideque->list (ideque-append d5 di)))
      (test (append l100 li) (ideque->list (ideque-append d100 di)))

      ;; ideque-concatenate TODO

      ;; ideque-count
      (test (count positive? li) (ideque-count positive? di))

      ;; ideque-zip TODO

    )) ; do loop

  ;; ideque-reverse
  (test '() (ideque->list (ideque-reverse d0)))
  (test '(1) (ideque->list (ideque-reverse d1)))
  (test (reverse l5) (ideque->list (ideque-reverse d5)))
  (test (reverse l100) (ideque->list (ideque-reverse d100)))

  ;; ideque-map
  (let ((double (cute * 2 <>)))
    (test (map double '())  (ideque->list (ideque-map double d0)))
    (test (map double l1)   (ideque->list (ideque-map double d1)))
    (test (map double '())  (ideque->list (ideque-map double d0)))
    (test (map double l5)   (ideque->list (ideque-map double d5)))
    (test (map double l100) (ideque->list (ideque-map double d100))))

  ;; ideque-for-each
  (let ((total 0))
    (ideque-for-each (lambda (i)
		       (set! total (+ i total)))
		     d100)
    (test (fold + 0 l100) (+ 0 total)))

  ;; ideque-fold
  (test (fold cons '() '()) (ideque-fold cons '() d0))
  (test (fold cons '() l5) (ideque-fold cons '() d5))
  (test (fold cons '() l100) (ideque-fold cons '() d100))

  ;; ideque-fold-right
  (test (fold-right cons '() '()) (ideque-fold-right cons '() d0))
  (test (fold-right cons '() l5) (ideque-fold-right cons '() d5))
  (test (fold-right cons '() l100) (ideque-fold-right cons '() d100))

  ;; ideque-append-map
  ;; TODO

  ;; ideque-filter
  (test (filter even? l100) (ideque->list (ideque-filter even? d100)))

  ;; ideque-remove
  (test (remove even? l100) (ideque->list (ideque-remove even? d100)))

  ;; ideque-partition
  (test-values (partition even? l100)
	       (receive (pre suf)
			(ideque-partition even? d100)
	         (values (ideque->list pre) (ideque->list suf))))

  ;; ideque-find
  ;; ideque-find-right
  ;; ideque-take-while
  ;; ideque-take-while-right
  ;; ideque-drop-while
  ;; ideque-drop-while-right
  ;; ideque-span
  ;; ideque-break
  ;; ideque-any
  ;; ideque-every
  ;; TODO

  ;; list->ideque
  ;; ideque->list
  (test '() (ideque->list d0))
  (test l5 (ideque->list d5))
  (test l100 (ideque->list d100))
  ;; round-trip
  (test '() (ideque->list (list->ideque (ideque->list d0))))
  (test l5 (ideque->list (list->ideque (ideque->list d5))))
  (test l100 (ideque->list (list->ideque (ideque->list d100))))

  ;; generator->ideque
  ;; ideque->generator
  ;; (implicitly tested by list->ideque and ideque->list)

  ;; ideque-comparator
  ;; make-ideque-comparator
  ;; TODO

  )
