
(import
 (chibi test)
 (generators)
 (pure)
 (scheme base)
 (scheme write)
 (srfi 1)
 (srfi 26))

(define n (expt 10 2)) ; moderately large n

(let* ((madd max)
       (mget (lambda (x)
	       x))
       (to-list finger-tree->list)
       (f0 (finger-tree/empty)) ; empty
       (f1 (finger-tree madd mget 1)) ; single
       (f2 (finger-tree madd mget 1 2)) ; 2-4 case
       (f3 (finger-tree madd mget 1 2 3))
       (f4 (finger-tree madd mget 1 2 3 4))
       (f5 (finger-tree madd mget 1 2 3 4 5)) ; 5-8 case
       (f6 (finger-tree madd mget 1 2 3 4 5 6))
       (f7 (finger-tree madd mget 1 2 3 4 5 6 7))
       (f8 (finger-tree madd mget 1 2 3 4 5 6 7 8))
       (f9 (finger-tree madd mget 1 2 3 4 5 6 7 8 9)) ; 9+ case
       (fn (list->finger-tree madd mget (iota n))))

  ;; finger-tree/empty
  (test-assert (procedure? finger-tree/empty))
  (test-assert (finger-tree? f0))
  (test-assert (finger-tree-empty? f0))
  (test '() (to-list f0))

  ;; finger-tree
  (test-assert (procedure? finger-tree))
  (test-assert (eqv? f0 (finger-tree madd mget))) ; equivalent to (finger-tree/empty)
  (test '(1) (to-list f1))
  (test '(1 2) (to-list f2))
  (test '(1 2 3) (to-list f3))
  (test '(1 2 3 4) (to-list f4))
  (test '(1 2 3 4 5) (to-list f5))
  (test '(1 2 3 4 5 6) (to-list f6))
  (test '(1 2 3 4 5 6 7) (to-list f7))
  (test '(1 2 3 4 5 6 7 8) (to-list f8))
  (test '(1 2 3 4 5 6 7 8 9) (to-list f9))

  ;; finger-tree?
  (test-assert (procedure? finger-tree?))
  (test #f (finger-tree? 7))
  (test #f (finger-tree? '()))
  (test #f (finger-tree? #f))
  (test #t (finger-tree? f0))
  (test #t (finger-tree? f1))
  (test #t (finger-tree? f2))
  (test #t (finger-tree? f3))
  (test #t (finger-tree? f4))
  (test #t (finger-tree? f5))
  (test #t (finger-tree? f6))
  (test #t (finger-tree? f7))
  (test #t (finger-tree? f8))
  (test #t (finger-tree? f9))

  ;; finger-tree-empty?
  (test-assert (procedure? finger-tree-empty?))
  (test #t (finger-tree-empty? f0))
  (test #f (finger-tree-empty? f1))
  (test #f (finger-tree-empty? f2))
  (test #f (finger-tree-empty? f3))
  (test #f (finger-tree-empty? f4))
  (test #f (finger-tree-empty? f5))
  (test #f (finger-tree-empty? f6))
  (test #f (finger-tree-empty? f7))
  (test #f (finger-tree-empty? f8))
  (test #f (finger-tree-empty? f9))

  ;; finger-tree-length
  (test-assert (procedure? finger-tree-length))
  (test 0 (finger-tree-length f0))
  (test 1 (finger-tree-length f1))
  (test 2 (finger-tree-length f2))
  (test 3 (finger-tree-length f3))
  (test 4 (finger-tree-length f4))
  (test 5 (finger-tree-length f5))
  (test 6 (finger-tree-length f6))
  (test 7 (finger-tree-length f7))
  (test 8 (finger-tree-length f8))
  (test 9 (finger-tree-length f9))

  ;; finger-tree-left
  (test-assert (procedure? finger-tree-left))
  (test-error (finger-tree-left f0))
  (test 1 (finger-tree-left f1))
  (test 1 (finger-tree-left f2))
  (test 1 (finger-tree-left f3))
  (test 1 (finger-tree-left f4))
  (test 1 (finger-tree-left f5))
  (test 1 (finger-tree-left f6))
  (test 1 (finger-tree-left f7))
  (test 1 (finger-tree-left f8))
  (test 1 (finger-tree-left f9))

  ;; finger-tree-right
  (test-assert (procedure? finger-tree-right))
  (test-error (finger-tree-right f0))
  (test 1 (finger-tree-right f1))
  (test 2 (finger-tree-right f2))
  (test 3 (finger-tree-right f3))
  (test 4 (finger-tree-right f4))
  (test 5 (finger-tree-right f5))
  (test 6 (finger-tree-right f6))
  (test 7 (finger-tree-right f7))
  (test 8 (finger-tree-right f8))
  (test 9 (finger-tree-right f9))

  ;; finger-tree-push-left
  (test-assert (procedure? finger-tree-push-left))
  (test '(10) (to-list (finger-tree-push-left madd mget f0 10)))
  (test '(10 1) (to-list (finger-tree-push-left madd mget f1 10)))
  (test '(10 1 2) (to-list (finger-tree-push-left madd mget f2 10)))
  (test '(10 1 2 3) (to-list (finger-tree-push-left madd mget f3 10)))
  (test '(10 1 2 3 4) (to-list (finger-tree-push-left madd mget f4 10)))
  (test '(10 1 2 3 4 5) (to-list (finger-tree-push-left madd mget f5 10)))
  (test '(10 1 2 3 4 5 6) (to-list (finger-tree-push-left madd mget f6 10)))
  (test '(10 1 2 3 4 5 6 7) (to-list (finger-tree-push-left madd mget f7 10)))
  (test '(10 1 2 3 4 5 6 7 8) (to-list (finger-tree-push-left madd mget f8 10)))
  (test '(10 1 2 3 4 5 6 7 8 9) (to-list (finger-tree-push-left madd mget f9 10)))

  ;; finger-tree-push-right
  (test-assert (procedure? finger-tree-push-right))
  (test '(10) (to-list (finger-tree-push-right madd mget f0 10)))
  (test '(1 10) (to-list (finger-tree-push-right madd mget f1 10)))
  (test '(1 2 10) (to-list (finger-tree-push-right madd mget f2 10)))
  (test '(1 2 3 10) (to-list (finger-tree-push-right madd mget f3 10)))
  (test '(1 2 3 4 10) (to-list (finger-tree-push-right madd mget f4 10)))
  (test '(1 2 3 4 5 10) (to-list (finger-tree-push-right madd mget f5 10)))
  (test '(1 2 3 4 5 6 10) (to-list (finger-tree-push-right madd mget f6 10)))
  (test '(1 2 3 4 5 6 7 10) (to-list (finger-tree-push-right madd mget f7 10)))
  (test '(1 2 3 4 5 6 7 8 10) (to-list (finger-tree-push-right madd mget f8 10)))
  (test '(1 2 3 4 5 6 7 8 9 10) (to-list (finger-tree-push-right madd mget f9 10)))

  ;; finger-tree-pop-left
  (test-assert (procedure? finger-tree-pop-left))
  (test-error (finger-tree-pop-left f0))
  (test '() (to-list (finger-tree-pop-left f1)))
  (test '(2) (to-list (finger-tree-pop-left f2)))
  (test '(2 3) (to-list (finger-tree-pop-left f3)))
  (test '(2 3 4) (to-list (finger-tree-pop-left f4)))
  (test '(2 3 4 5) (to-list (finger-tree-pop-left f5)))
  (test '(2 3 4 5 6) (to-list (finger-tree-pop-left f6)))
  (test '(2 3 4 5 6 7) (to-list (finger-tree-pop-left f7)))
  (test '(2 3 4 5 6 7 8) (to-list (finger-tree-pop-left f8)))
  (test '(2 3 4 5 6 7 8 9) (to-list (finger-tree-pop-left f9)))

  ;; finger-tree-pop-right
  (test-assert (procedure? finger-tree-pop-right))
  (test-error (finger-tree-pop-right f0))
  (test '() (to-list (finger-tree-pop-right f1)))
  (test '(1) (to-list (finger-tree-pop-right f2)))
  (test '(1 2) (to-list (finger-tree-pop-right f3)))
  (test '(1 2 3) (to-list (finger-tree-pop-right f4)))
  (test '(1 2 3 4) (to-list (finger-tree-pop-right f5)))
  (test '(1 2 3 4 5) (to-list (finger-tree-pop-right f6)))
  (test '(1 2 3 4 5 6) (to-list (finger-tree-pop-right f7)))
  (test '(1 2 3 4 5 6 7) (to-list (finger-tree-pop-right f8)))
  (test '(1 2 3 4 5 6 7 8) (to-list (finger-tree-pop-right f9)))  

  ;; finger-tree-append
  (test-assert (procedure? finger-tree-append))
  (let ((trial (lambda (list l r)
		 (test list (to-list (finger-tree-append madd mget l r))))))
    ;; empty left
    (trial '() f0 f0)
    (trial '(1) f0 f1)
    (trial '(1 2 3) f0 f3)

    ;; empty right
    (trial '() f0 f0)
    (trial '(1) f1 f0)
    (trial '(1 2 3) f3 f0)

    ;; single left
    (trial '(1) f1 f0)
    (trial '(1 1) f1 f1)
    (trial '(1 1 2 3) f1 f3)

    ;; single right
    (trial '(1) f0 f1)
    (trial '(1 1) f1 f1)
    (trial '(1 2 3 1) f3 f1)

    ;; both operands deep
    (trial '(1 2 1 2) f2 f2) ; 2 loose
    (trial '(1 2 3 1 2) f3 f2) ; 3 loose
    (trial '(1 2 3 4 1 2) f4 f2) ; 4 loose
    (trial '(1 2 3 4 5 1 2) f5 f2) ; 5 loose
    (trial '(1 2 3 4 5 6 1 2) f6 f2) ; 6 loose
    (trial '(1 2 3 4 5 6 7 1 2) f7 f2) ; 7 loose
    (trial '(1 2 3 4 5 6 7 8 1 2) f8 f2) ; 8 loose

    ;; recurses into spine
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3) f9 f3)
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3 4) f9 f4)
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3 4 5) f9 f5)
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6) f9 f6)
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7) f9 f7)
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8) f9 f8)
    (trial '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9) f9 f9))

  ;; finger-tree-scan
  (test-assert (procedure? finger-tree-scan))
  (let ((trial (lambda (exp tree query)
		 (test exp
		       (finger-tree-scan madd
					 mget
					 (cute >= <> query)
					 0
					 tree
					 (lambda (match)
					   match)
					 (lambda ()
					   #f))))))
    (trial #f f0 1)

    (trial 1 f1 1)
    (trial #f f1 2)

    (trial 1 f2 1)
    (trial 2 f2 2)
    (trial #f f2 3)

    (let ((all-trials (lambda (tree)
			(generator-for-each (lambda (e)
					      (trial e tree e))
					    (finger-tree->generator tree))
			(trial #f tree (+ 1 (finger-tree-length tree))))))
      (all-trials f3)
      (all-trials f4)
      (all-trials f5)
      (all-trials f6)
      (all-trials f7)
      (all-trials f8)
      (all-trials f9)
      (all-trials fn)))

  ;; finger-tree-scan/context
  (test-assert (procedure? finger-tree-scan/context))
  (let* ((scan/ctx (lambda (tree query)
		     (finger-tree-scan/context madd
					       mget
					       (cute >= <> query)
					       0
					       tree
					       (lambda (pre e suf)
						 (values (to-list (pre))
							 e
							 (to-list (suf))))
					       (lambda ()
						 #f))))
	 (trial/absent (lambda (tree query)
			 (test #f (scan/ctx tree query))))
	 (trial/match (lambda (pre e suf tree query)
			(test-values (values pre e suf) (scan/ctx tree query)))))

    (trial/absent f0 1)

    (trial/match '() 1 '() f1 1)
    (trial/absent f1 2)

    (trial/match '() 1 '(2) f2 1)
    (trial/match '(1) 2 '() f2 2)
    (trial/absent f2 3)

    (trial/match '() 1 '(2 3) f3 1)
    (trial/match '(1) 2 '(3) f3 2)
    (trial/match '(1 2) 3 '() f3 3)
    (trial/absent f3 4)

    (let ((all-trials (lambda (tree)
			(let ((lst (finger-tree->list tree))
			      (n (finger-tree-length tree)))
			  (generator-for-each (lambda (e)
						(trial/match (filter (cute < <> e) lst)
							     e
							     (filter (cute > <> e) lst)
							     tree
							     e)
						(trial/absent tree (+ 1 n)))
					      (finger-tree->generator tree))))))
      (all-trials f4)
      (all-trials f5)
      (all-trials f6)
      (all-trials f7)
      (all-trials f8)
      (all-trials f9)
      (all-trials fn)))

  ;; generator->finger-tree
  ;; finger-tree->generator
  ;; (nontrivial cases are covered by the list conversion procedures below)
  (test-assert (procedure? generator->finger-tree))
  (test '(1 2 3) (to-list (generator->finger-tree madd mget (make-generator 1 2 3))))
  (test-assert (procedure? finger-tree->generator))
  (test '(1 2 3) (generator->list (finger-tree->generator f3)))

  ;; TODO reverse-finger-tree->generator

  ;; list->finger-tree
  ;; finger-tree->list
  (test-assert (procedure? list->finger-tree))
  (test-assert (procedure? finger-tree->list))
  (do ((i 0 (+ 1 i)))
      ((= i n))
    (let ((lst (iota i)))
      (test lst  (finger-tree->list (list->finger-tree madd mget lst)))))

  ) ;; finger-tree tests

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
