
(import
 (chibi test)
 (comparators)
 (generators)
 (immutable finger-tree)
 (scheme base)
 (scheme write) ; TODO: remove
 (srfi 1)
 (srfi 26)
 (srfi 95))

(let* ((n 100) ; moderately large n

       (identity (lambda (x)
		   x))

       (madd max)
       (mget identity)

       (kcmp number-comparator)
       (kget identity)
       (false-thunk (lambda () #false))
       (merge-common (lambda (l r)
		       l))

       (to-list finger-tree->list)

       (sort-numbers (cute sort <> (cute <? number-comparator <> <>)))

       (f0 (finger-tree)) ; empty
       (f1 (finger-tree madd mget 1)) ; single
       (f2 (finger-tree madd mget 1 2)) ; 2-4 case
       (f3 (finger-tree madd mget 1 2 3))
       (f4 (finger-tree madd mget 1 2 3 4))
       (f5 (finger-tree madd mget 1 2 3 4 5)) ; 5-8 case
       (f6 (finger-tree madd mget 1 2 3 4 5 6))
       (f7 (finger-tree madd mget 1 2 3 4 5 6 7))
       (f8 (finger-tree madd mget 1 2 3 4 5 6 7 8))
       (f9 (finger-tree madd mget 1 2 3 4 5 6 7 8 9)) ; 9+ case
       (ln (iota n))
       (fn (list->finger-tree madd mget ln)))

  ;; finger-tree
  (test-assert (procedure? finger-tree))
  (test-assert (eqv? f0 (finger-tree madd mget)))
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

  ;; finger-tree-front
  (test-assert (procedure? finger-tree-front))
  (test-error (finger-tree-front f0))
  (test 1 (finger-tree-front f1))
  (test 1 (finger-tree-front f2))
  (test 1 (finger-tree-front f3))
  (test 1 (finger-tree-front f4))
  (test 1 (finger-tree-front f5))
  (test 1 (finger-tree-front f6))
  (test 1 (finger-tree-front f7))
  (test 1 (finger-tree-front f8))
  (test 1 (finger-tree-front f9))

  ;; finger-tree-back
  (test-assert (procedure? finger-tree-back))
  (test-error (finger-tree-back f0))
  (test 1 (finger-tree-back f1))
  (test 2 (finger-tree-back f2))
  (test 3 (finger-tree-back f3))
  (test 4 (finger-tree-back f4))
  (test 5 (finger-tree-back f5))
  (test 6 (finger-tree-back f6))
  (test 7 (finger-tree-back f7))
  (test 8 (finger-tree-back f8))
  (test 9 (finger-tree-back f9))

  ;; finger-tree-add-front
  (test-assert (procedure? finger-tree-add-front))
  (test '(10) (to-list (finger-tree-add-front madd mget f0 10)))
  (test '(10 1) (to-list (finger-tree-add-front madd mget f1 10)))
  (test '(10 1 2) (to-list (finger-tree-add-front madd mget f2 10)))
  (test '(10 1 2 3) (to-list (finger-tree-add-front madd mget f3 10)))
  (test '(10 1 2 3 4) (to-list (finger-tree-add-front madd mget f4 10)))
  (test '(10 1 2 3 4 5) (to-list (finger-tree-add-front madd mget f5 10)))
  (test '(10 1 2 3 4 5 6) (to-list (finger-tree-add-front madd mget f6 10)))
  (test '(10 1 2 3 4 5 6 7) (to-list (finger-tree-add-front madd mget f7 10)))
  (test '(10 1 2 3 4 5 6 7 8) (to-list (finger-tree-add-front madd mget f8 10)))
  (test '(10 1 2 3 4 5 6 7 8 9) (to-list (finger-tree-add-front madd mget f9 10)))

  ;; finger-tree-add-back
  (test-assert (procedure? finger-tree-add-back))
  (test '(10) (to-list (finger-tree-add-back madd mget f0 10)))
  (test '(1 10) (to-list (finger-tree-add-back madd mget f1 10)))
  (test '(1 2 10) (to-list (finger-tree-add-back madd mget f2 10)))
  (test '(1 2 3 10) (to-list (finger-tree-add-back madd mget f3 10)))
  (test '(1 2 3 4 10) (to-list (finger-tree-add-back madd mget f4 10)))
  (test '(1 2 3 4 5 10) (to-list (finger-tree-add-back madd mget f5 10)))
  (test '(1 2 3 4 5 6 10) (to-list (finger-tree-add-back madd mget f6 10)))
  (test '(1 2 3 4 5 6 7 10) (to-list (finger-tree-add-back madd mget f7 10)))
  (test '(1 2 3 4 5 6 7 8 10) (to-list (finger-tree-add-back madd mget f8 10)))
  (test '(1 2 3 4 5 6 7 8 9 10) (to-list (finger-tree-add-back madd mget f9 10)))

  ;; finger-tree-remove-front
  (test-assert (procedure? finger-tree-remove-front))
  (test-error (finger-tree-remove-front f0))
  (test '() (to-list (finger-tree-remove-front f1)))
  (test '(2) (to-list (finger-tree-remove-front f2)))
  (test '(2 3) (to-list (finger-tree-remove-front f3)))
  (test '(2 3 4) (to-list (finger-tree-remove-front f4)))
  (test '(2 3 4 5) (to-list (finger-tree-remove-front f5)))
  (test '(2 3 4 5 6) (to-list (finger-tree-remove-front f6)))
  (test '(2 3 4 5 6 7) (to-list (finger-tree-remove-front f7)))
  (test '(2 3 4 5 6 7 8) (to-list (finger-tree-remove-front f8)))
  (test '(2 3 4 5 6 7 8 9) (to-list (finger-tree-remove-front f9)))

  ;; finger-tree-remove-back
  (test-assert (procedure? finger-tree-remove-back))
  (test-error (finger-tree-remove-back f0))
  (test '() (to-list (finger-tree-remove-back f1)))
  (test '(1) (to-list (finger-tree-remove-back f2)))
  (test '(1 2) (to-list (finger-tree-remove-back f3)))
  (test '(1 2 3) (to-list (finger-tree-remove-back f4)))
  (test '(1 2 3 4) (to-list (finger-tree-remove-back f5)))
  (test '(1 2 3 4 5) (to-list (finger-tree-remove-back f6)))
  (test '(1 2 3 4 5 6) (to-list (finger-tree-remove-back f7)))
  (test '(1 2 3 4 5 6 7) (to-list (finger-tree-remove-back f8)))
  (test '(1 2 3 4 5 6 7 8) (to-list (finger-tree-remove-back f9)))  

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
						 (values (to-list pre)
							 e
							 (to-list suf)))
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
			  (for-each (lambda (e)
				      (trial/match (filter (cute < <> e) lst)
						   e
						   (filter (cute > <> e) lst)
						   tree
						   e))
				    lst)
			  (trial/absent tree (+ 1 n))))))
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

  ;; reverse-finger-tree->generator
  (test '() (generator->list (reverse-finger-tree->generator f0)))
  (test '(1) (generator->list (reverse-finger-tree->generator f1)))
  (test '(2 1) (generator->list (reverse-finger-tree->generator f2)))
  (test '(3 2 1) (generator->list (reverse-finger-tree->generator f3)))
  (test '(4 3 2 1) (generator->list (reverse-finger-tree->generator f4)))
  (test '(5 4 3 2 1) (generator->list (reverse-finger-tree->generator f5)))
  (test '(6 5 4 3 2 1) (generator->list (reverse-finger-tree->generator f6)))
  (test '(7 6 5 4 3 2 1) (generator->list (reverse-finger-tree->generator f7)))
  (test '(8 7 6 5 4 3 2 1) (generator->list (reverse-finger-tree->generator f8)))
  (test '(9 8 7 6 5 4 3 2 1) (generator->list (reverse-finger-tree->generator f9)))
  (test (reverse ln)
	(generator->list (reverse-finger-tree->generator fn)))

  ;; list->finger-tree
  ;; finger-tree->list
  (test-assert (procedure? list->finger-tree))
  (test-assert (procedure? finger-tree->list))
  (do ((i 0 (+ 1 i)))
      ((= i n))
    (let ((lst (iota i)))
      (test lst  (finger-tree->list (list->finger-tree madd mget lst)))))

  ;; pseudoset-finger-tree-find
  (let ((find (cute pseudoset-finger-tree-find kcmp kget fn <> identity false-thunk)))
    (do ((i 0 (+ 1 i)))
	((= i 10))
      (test i (find i))
      (test #f (find (- i 0.5)))
      (test #f (find (+ i 0.5)))))

  ;; pseudoset-finger-tree-update
  (do ((i 0 (+ 1 i)))
      ((= i n))

    (let ((epsilon (+ i 0.5)))

      ;; ignore absent element
      (test #false
	    (pseudoset-finger-tree-update kcmp kget fn epsilon
					  (lambda (e replace remove)
					    #true)
					  (lambda (insert)
					    #false)))

      ;; insert absent element
      (test (sort-numbers (cons epsilon ln))
	    (finger-tree->list
	     (pseudoset-finger-tree-update kcmp kget fn epsilon
					   (lambda (e replace remove)
					     (error "unexpected state"))
					   (lambda (insert)
					     (insert)))))

      ;; delete element
      (test (remove (cute = i <>) ln)
	    (finger-tree->list
	     (pseudoset-finger-tree-update kcmp kget fn i
					   (lambda (e replace remove)
					     (remove))
					   (lambda (insert)
					     (error "unexpected state")))))

      ;; replace element
      (test (map (lambda (x)
		   (if (= x i)
		       epsilon
		       x))
		 ln)
	    (finger-tree->list
	     (pseudoset-finger-tree-update kcmp kget fn i
					   (lambda (e replace remove)
					     (replace epsilon))
					   (lambda (insert)
					     (error "unexpected state")))))
    
      ))
  ;; TODO

  ;; increasing-generator->pseudoset-finger-tree
  (test '(1 2 3 4 5)
	(finger-tree->list
	 (increasing-generator->pseudoset-finger-tree
	  (make-generator 1 2 3 4 5))))
  
  ;; set-theoretic procedures
  (define (lset<binary x y)
    (and (lset<= = x y)
	 (not (lset= = x y))))
  (define (lset< x y z)
    (and (lset<binary x y) (lset<binary y z)))

  (let ((powerset (fold (lambda (x subsets)
			  (append subsets
				  (map (lambda (set)
					 (append set (list x)))
				       subsets)))
			'(())
			(iota 3))))

    (for-each
     (lambda (a)
       (for-each
	(lambda (b)
	  (for-each
	   (lambda (c)
	     (let ((fa (list->finger-tree madd mget a))
		   (fb (list->finger-tree madd mget b))
		   (fc (list->finger-tree madd mget c)))
	       
	       (test (lset< a b c)
		     (pseudoset-finger-tree<? kcmp kget fa fb fc))

	       (test (lset<= = a b c)
		     (pseudoset-finger-tree<=? kcmp kget fa fb fc))

	       (test (lset= = a b c)
		     (pseudoset-finger-tree=? kcmp kget fa fb fc))

	       (test (lset<= = c b a)
		     (pseudoset-finger-tree>=? kcmp kget fa fb fc))

	       (test (lset< c b a)
		     (pseudoset-finger-tree>? kcmp kget fa fb fc))

	       (test (sort-numbers (lset-union = a b c))
		     (finger-tree->list
		      (pseudoset-finger-tree-union merge-common kcmp kget fa fb fc)))

	       (test (sort-numbers (lset-intersection = a b c))
		     (finger-tree->list
		      (pseudoset-finger-tree-intersection merge-common kcmp kget fa fb fc)))

	       (test (sort-numbers (lset-difference = a b c))
		     (finger-tree->list
		      (pseudoset-finger-tree-difference kcmp kget fa fb fc)))

	       (test (sort-numbers (lset-xor = a b))
		     (finger-tree->list
		      (pseudoset-finger-tree-xor kcmp kget fa fb)))

	       ))

	   powerset))
	powerset))
     powerset))

  ) ;; let
