
(import
 (chibi test)
 (scheme comparator)
 (scheme generator)
 (immutable finger-tree)
 (scheme base)
 (srfi 1)
 (srfi 26)
 (srfi 95))

(define number-comparator
  (make-comparator real? = < (lambda (x . o) (exact (abs (round x))))))

(define sort-numbers (cute sort <> (cute <? number-comparator <> <>)))

;; set-theoretic procedures
(define (binary-lset< x y)
  (and (lset<= = x y)
       (not (lset= = x y))))
(define (lset< x y z)
  (and (binary-lset< x y)
       (binary-lset< y z)))

;; make-measure
(test-assert (procedure? make-measure))
(test-assert (measure? (make-measure car + 0)))

;; measure?
(test-assert (procedure? measure?))
(test #t (measure? (make-measure car + 0)))
(test #f (measure? 7))

;; measure-get-proc
;; measure-add-proc
(test-assert (procedure? measure-get-proc))
(test-assert (procedure? measure-add-proc))
(let ((meas (make-measure 'a 'b 0)))
  (test 'a (measure-get-proc meas))
  (test 'b (measure-add-proc meas)))

;; measure-get
;; measure-add
;; measure-get+add
(test-assert (procedure? measure-get))
(test-assert (procedure? measure-add))
(test-assert (procedure? measure-get+add))
(let ((meas (make-measure car + 0)))
  (test 5 (measure-get meas '(5 6)))
  (test 7 (measure-add meas 3 4)) ;; 2 measures
  (test 12 (measure-add meas 3 4 5)) ;; 3 measures
  (test 15 (measure-get+add meas 10 '(5 6))))  
(let* ((n 100) ; moderately large n

       (order (make-set-order (lambda (x) x)
			      number-comparator))
       (meas (set-order-measure order))
       
       (to-list finger-tree->list)

       (f0 (finger-tree meas)) ; empty
       (f1 (finger-tree meas 1)) ; single
       (f2 (finger-tree meas 1 2)) ; 2-4 case
       (f3 (finger-tree meas 1 2 3))
       (f4 (finger-tree meas 1 2 3 4))
       (f5 (finger-tree meas 1 2 3 4 5)) ; 5-8 case
       (f6 (finger-tree meas 1 2 3 4 5 6))
       (f7 (finger-tree meas 1 2 3 4 5 6 7))
       (f8 (finger-tree meas 1 2 3 4 5 6 7 8))
       (f9 (finger-tree meas 1 2 3 4 5 6 7 8 9)) ; 9+ case
       (ln (iota n))
       (fn (list->finger-tree meas ln)))

  ;; make-finger-tree
  (test-assert (procedure? make-finger-tree))
  (test-assert (finger-tree-empty? (make-finger-tree)))

  ;; finger-tree
  (test-assert (procedure? finger-tree))
  (test '() (to-list f0))
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

  ;; finger-tree-non-empty?
  (test-assert (procedure? finger-tree-non-empty?))
  (test #f (finger-tree-non-empty? f0))
  (test #t (finger-tree-non-empty? f1))
  (test #t (finger-tree-non-empty? f2))
  (test #t (finger-tree-non-empty? f3))
  (test #t (finger-tree-non-empty? f4))
  (test #t (finger-tree-non-empty? f5))
  (test #t (finger-tree-non-empty? f6))
  (test #t (finger-tree-non-empty? f7))
  (test #t (finger-tree-non-empty? f8))
  (test #t (finger-tree-non-empty? f9))

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

  ;; finger-tree-add-left
  (test-assert (procedure? finger-tree-add-left))
  (test '(10) (to-list (finger-tree-add-left meas f0 10)))
  (test '(10 1) (to-list (finger-tree-add-left meas f1 10)))
  (test '(10 1 2) (to-list (finger-tree-add-left meas f2 10)))
  (test '(10 1 2 3) (to-list (finger-tree-add-left meas f3 10)))
  (test '(10 1 2 3 4) (to-list (finger-tree-add-left meas f4 10)))
  (test '(10 1 2 3 4 5) (to-list (finger-tree-add-left meas f5 10)))
  (test '(10 1 2 3 4 5 6) (to-list (finger-tree-add-left meas f6 10)))
  (test '(10 1 2 3 4 5 6 7) (to-list (finger-tree-add-left meas f7 10)))
  (test '(10 1 2 3 4 5 6 7 8) (to-list (finger-tree-add-left meas f8 10)))
  (test '(10 1 2 3 4 5 6 7 8 9) (to-list (finger-tree-add-left meas f9 10)))

  ;; finger-tree-add-right
  (test-assert (procedure? finger-tree-add-right))
  (test '(10) (to-list (finger-tree-add-right meas f0 10)))
  (test '(1 10) (to-list (finger-tree-add-right meas f1 10)))
  (test '(1 2 10) (to-list (finger-tree-add-right meas f2 10)))
  (test '(1 2 3 10) (to-list (finger-tree-add-right meas f3 10)))
  (test '(1 2 3 4 10) (to-list (finger-tree-add-right meas f4 10)))
  (test '(1 2 3 4 5 10) (to-list (finger-tree-add-right meas f5 10)))
  (test '(1 2 3 4 5 6 10) (to-list (finger-tree-add-right meas f6 10)))
  (test '(1 2 3 4 5 6 7 10) (to-list (finger-tree-add-right meas f7 10)))
  (test '(1 2 3 4 5 6 7 8 10) (to-list (finger-tree-add-right meas f8 10)))
  (test '(1 2 3 4 5 6 7 8 9 10) (to-list (finger-tree-add-right meas f9 10)))

  ;; finger-tree-remove-left
  (test-assert (procedure? finger-tree-remove-left))
  (test-error (finger-tree-remove-left f0))
  (test '() (to-list (finger-tree-remove-left f1)))
  (test '(2) (to-list (finger-tree-remove-left f2)))
  (test '(2 3) (to-list (finger-tree-remove-left f3)))
  (test '(2 3 4) (to-list (finger-tree-remove-left f4)))
  (test '(2 3 4 5) (to-list (finger-tree-remove-left f5)))
  (test '(2 3 4 5 6) (to-list (finger-tree-remove-left f6)))
  (test '(2 3 4 5 6 7) (to-list (finger-tree-remove-left f7)))
  (test '(2 3 4 5 6 7 8) (to-list (finger-tree-remove-left f8)))
  (test '(2 3 4 5 6 7 8 9) (to-list (finger-tree-remove-left f9)))

  ;; finger-tree-remove-right
  (test-assert (procedure? finger-tree-remove-right))
  (test-error (finger-tree-remove-right f0))
  (test '() (to-list (finger-tree-remove-right f1)))
  (test '(1) (to-list (finger-tree-remove-right f2)))
  (test '(1 2) (to-list (finger-tree-remove-right f3)))
  (test '(1 2 3) (to-list (finger-tree-remove-right f4)))
  (test '(1 2 3 4) (to-list (finger-tree-remove-right f5)))
  (test '(1 2 3 4 5) (to-list (finger-tree-remove-right f6)))
  (test '(1 2 3 4 5 6) (to-list (finger-tree-remove-right f7)))
  (test '(1 2 3 4 5 6 7) (to-list (finger-tree-remove-right f8)))
  (test '(1 2 3 4 5 6 7 8) (to-list (finger-tree-remove-right f9)))

  ;; finger-tree-append
  (test-assert (procedure? finger-tree-append))
  (let ((trial (lambda (list l r)
		 (test list (to-list (finger-tree-append meas l r))))))
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
  (let ((trial (lambda (expect tree query)
		 (test expect
		       (finger-tree-scan meas
					 (cute >= <> query)
					 tree
					 (lambda (m match)
					   match)
					 (lambda (m)
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

  ;; finger-tree-bisect
  (test-assert (procedure? finger-tree-bisect))
  (let* ((bisect (lambda (tree query)
		   (finger-tree-bisect meas
				       (cute >= <> query)
				       tree
				       (lambda (m pre suf)
					 (values (to-list pre)
						 (to-list suf)))
				       (lambda (m)
					 #f))))
	 (trial/absent (lambda (tree query)
			 (test #f (bisect tree query))))
	 (trial/match (lambda (pre suf tree query)
			(test-values (values pre suf) (bisect tree query)))))

    (trial/absent f0 1)

    (trial/match '() '(1) f1 1)
    (trial/absent f1 2)

    (trial/match '() '(1 2) f2 1)
    (trial/match '(1) '(2) f2 2)
    (trial/absent f2 3)

    (trial/match '() '(1 2 3) f3 1)
    (trial/match '(1) '(2 3) f3 2)
    (trial/match '(1 2) '(3) f3 3)
    (trial/absent f3 4)

    (let ((all-trials (lambda (tree)
			(let ((lst (finger-tree->list tree))
			      (n (finger-tree-length tree)))
			  (for-each (lambda (e)
				      (trial/match (filter (cute < <> e) lst)
						   (filter (cute >= <> e) lst)
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

  ;; finger-tree-reverse
  (test-assert (procedure? finger-tree-reverse))
  (test '() (to-list (finger-tree-reverse meas f0)))
  (test '(1) (to-list (finger-tree-reverse meas f1)))
  (test '(2 1) (to-list (finger-tree-reverse meas f2)))
  (test '(3 2 1) (to-list (finger-tree-reverse meas f3)))
  (test '(4 3 2 1) (to-list (finger-tree-reverse meas f4)))
  (test '(5 4 3 2 1) (to-list (finger-tree-reverse meas f5)))
  (test '(6 5 4 3 2 1) (to-list (finger-tree-reverse meas f6)))
  (test '(7 6 5 4 3 2 1) (to-list (finger-tree-reverse meas f7)))
  (test '(8 7 6 5 4 3 2 1) (to-list (finger-tree-reverse meas f8)))
  (test '(9 8 7 6 5 4 3 2 1) (to-list (finger-tree-reverse meas f9)))

  ;; generator->finger-tree
  ;; finger-tree->generator
  ;; (nontrivial cases are covered by the list conversion procedures below)
  (test-assert (procedure? generator->finger-tree))
  (test '(1 2 3) (to-list (generator->finger-tree meas (list->generator '(1 2 3)))))
  (test-assert (procedure? finger-tree->generator))
  (test '(1 2 3) (generator->list (finger-tree->generator f3)))

  ;; list->finger-tree
  ;; vector->finger-tree
  ;; finger-tree->list
  ;; finger-tree->vector
  (test-assert (procedure? list->finger-tree))
  (test-assert (procedure? finger-tree->list))
  (test-assert (procedure? vector->finger-tree))
  (test-assert (procedure? finger-tree->vector))
  (do ((i 0 (+ 1 i)))
      ((= i n))
    (let* ((lst (iota i))
	   (vect (list->vector lst)))
      (test lst  (finger-tree->list (list->finger-tree meas lst)))
      (test vect (finger-tree->vector (vector->finger-tree meas vect)))))
  
  ;; finger-tree->reverse-generator
  (test '() (generator->list (finger-tree->reverse-generator f0)))
  (test '(1) (generator->list (finger-tree->reverse-generator f1)))
  (test '(2 1) (generator->list (finger-tree->reverse-generator f2)))
  (test '(3 2 1) (generator->list (finger-tree->reverse-generator f3)))
  (test '(4 3 2 1) (generator->list (finger-tree->reverse-generator f4)))
  (test '(5 4 3 2 1) (generator->list (finger-tree->reverse-generator f5)))
  (test '(6 5 4 3 2 1) (generator->list (finger-tree->reverse-generator f6)))
  (test '(7 6 5 4 3 2 1) (generator->list (finger-tree->reverse-generator f7)))
  (test '(8 7 6 5 4 3 2 1) (generator->list (finger-tree->reverse-generator f8)))
  (test '(9 8 7 6 5 4 3 2 1) (generator->list (finger-tree->reverse-generator f9)))
  (test (reverse ln)
	(generator->list (finger-tree->reverse-generator fn))))

(let ((order (make-set-order car number-comparator)))

  ;; make-set-order
  (test-assert (procedure? make-set-order))
  (test-assert (set-order? order))

  ;; set-order?
  (test #t (set-order? order))
  (test #f (set-order? 7))

  ;; set-order-measure
  (let ((meas (set-order-measure order)))

    (test-assert (measure? meas))

    (test-assert (eq? car (measure-get-proc meas)))
    
    ;; measure-add always picks the rightmost argument
    (test 6 (measure-add meas 5 6))
    (test 8 (measure-add meas 5 8))
    (test 7 (measure-add meas 5 6 7))
    (test 3 (measure-add meas 1 2 3))

    ;; set-order-comparator
    (test-assert (eq? number-comparator (set-order-comparator order)))

    ;; set-order-key
    (test 5 (set-order-key order '(5 6)))
    (test 7 (set-order-key order '(7 8)))))

(let* ((order (make-set-order (lambda (x) x)
			      number-comparator))
       (s0 (increasing-list->finger-tree-set order '()))
       (s1 (increasing-list->finger-tree-set order '(1)))
       (s2 (increasing-list->finger-tree-set order '(1 2)))
       (s3 (increasing-list->finger-tree-set order '(1 2 3)))
       (s4 (increasing-list->finger-tree-set order '(1 2 3 4)))
       (s5 (increasing-list->finger-tree-set order '(1 2 3 4 5)))
       (s6 (increasing-list->finger-tree-set order '(1 2 3 4 5 6)))
       (s7 (increasing-list->finger-tree-set order '(1 2 3 4 5 6 7)))
       (s8 (increasing-list->finger-tree-set order '(1 2 3 4 5 6 7 8)))
       (s9 (increasing-list->finger-tree-set order '(1 2 3 4 5 6 7 8 9)))
       (all-sets (list s0 s1 s2 s3 s4 s5 s6 s7 s8 s9)))

  ;; finger-tree-set-search
  ;; finger-tree-set-update (search functionality)
  (let ((search (lambda (tree query)
		  (finger-tree-set-search order
					  tree
					  query
					  (lambda (match) match)
					  (lambda () #f))))
	(update (lambda (tree query)
		  (finger-tree-set-update order
					  tree
					  query
					  (lambda (x remove replace)
					    x)
					  (lambda (insert)
					    #f)))))
    (for-each (lambda (set)
		(for-each (lambda (elt)
			    ;; success
			    (test elt (search set elt))
			    (test elt (update set elt))
			    ;; failure
			    (test #f (search set (+ elt 0.5)))
			    (test #f (update set (+ elt 0.5)))
			    (test #f (search set (- elt 0.5)))
			    (test #f (update set (- elt 0.5))))
			  (finger-tree->list set)))
	      all-sets)

    ;; finger-tree-set-update (insert functionality)
    ;; finger-tree-set-adjoin
    ;; finger-tree-set-replace
    ;; finger-tree-set-delete
    ;; (except the distinction between adjoin and replace)
    (let ((adjoin (lambda (tree elt)
		    (finger-tree->list
		     (finger-tree-set-adjoin order tree elt))))
	  (replace (lambda (tree elt)
		     (finger-tree->list
		      (finger-tree-set-replace order tree elt))))
	  (del (lambda (tree elt)
		 (finger-tree->list
		  (finger-tree-set-delete order tree elt))))
	  (update (lambda (tree elt)
		    (finger-tree->list
		     (finger-tree-set-update order
					     tree
					     elt
					     (lambda (x remove replace)
					       (replace elt))
					     (lambda (insert)
					       (insert elt)))))))
      (for-each (lambda (set)
		  ;; duplicate element
		  (for-each (lambda (elt)
			      (let ((expected (finger-tree->list set)))
				(test expected (adjoin set elt))
				(test expected (replace set elt))
				(test expected (update set elt))
				(test (delete elt expected)
				      (del set elt))))
			      
			    (finger-tree->list set))
		  ;; distinct element
		  (for-each (lambda (elt)
			      (let ((expected (sort-numbers (cons elt (finger-tree->list set)))))
				(test expected (adjoin set elt))
				(test expected (replace set elt))
				(test expected (update set elt))
				(test (finger-tree->list set)
				      (del set elt))))
			    (map (cute + <> .5) (finger-tree->list set))))
		all-sets))

    ;; finger-tree-set-predecessor
    ;; finger-tree-set-successor
    (let ((predecessor (lambda (set query)
			 (finger-tree-set-predecessor order
						      set
						      query
						      (lambda (x) x)
						      (lambda () #f))))
	  (successor (lambda (set query)
		       (finger-tree-set-successor order
						  set
						  query
						  (lambda (x) x)
						  (lambda () #f)))))
      (test #f (predecessor s9 0.5))
      (test #f (predecessor s9 1))
      (test 1 (predecessor s9 1.5))
      (test 1 (predecessor s9 2))
      (test 2 (predecessor s9 2.5))
      (test 2 (predecessor s9 3))
      (test 3 (predecessor s9 3.5))
      (test 3 (predecessor s9 4))
      (test 4 (predecessor s9 4.5))
      (test 4 (predecessor s9 5))
      (test 5 (predecessor s9 5.5))
      (test 5 (predecessor s9 6))
      (test 6 (predecessor s9 6.5))
      (test 6 (predecessor s9 7))
      (test 7 (predecessor s9 7.5))
      (test 7 (predecessor s9 8))
      (test 8 (predecessor s9 8.5))
      (test 8 (predecessor s9 9))
      (test 9 (predecessor s9 9.5))
      (test 9 (predecessor s9 10))

      (test 1 (successor s9 0.5))
      (test 2 (successor s9 1))
      (test 2 (successor s9 1.5))
      (test 3 (successor s9 2))
      (test 3 (successor s9 2.5))
      (test 4 (successor s9 3))
      (test 4 (successor s9 3.5))
      (test 5 (successor s9 4))
      (test 5 (successor s9 4.5))
      (test 6 (successor s9 5))
      (test 6 (successor s9 5.5))
      (test 7 (successor s9 6))
      (test 7 (successor s9 6.5))
      (test 8 (successor s9 7))
      (test 8 (successor s9 7.5))
      (test 9 (successor s9 8))
      (test 9 (successor s9 8.5))
      (test #f (successor s9 9))
      (test #f (successor s9 9.5))

      ;; empty
      (test #f (predecessor s0 1))
      (test #f (successor s0 1))
      
      (for-each (lambda (set)
		  (let ((lst (finger-tree->list set)))
		    (for-each (lambda (query)
				(let ((pred (let ((less (filter (cute < <> query)
								lst)))
					      (if (null? less)
						  #f
						  (last less))))
				      (succ (let ((greater (filter (cute > <> query)
								   lst)))
					      (if (null? greater)
						  #f
						  (first greater)))))
				  (test pred (predecessor set query))
				  (test succ (successor set query))))
			      lst)))
      all-sets)
      
      ))

  (let ((powerset (fold (lambda (x subsets)
			  (append subsets
				  (map (lambda (set)
					 (append set (list x)))
				       subsets)))
			'(())
			(iota 4)))
	(reduce (lambda (x y) x)))
    (for-each
     (lambda (a)
       (for-each
	(lambda (b)
	  (for-each
	   (lambda (c)
	     (let ((fa (list->finger-tree-set order reduce a))
		   (fb (list->finger-tree-set order reduce b))
		   (fc (list->finger-tree-set order reduce c)))

	       ;; <
	       (test (lset< a b c)
		     (finger-tree-set<? order fa fb fc))

	       ;; <=
	       (test (lset<= = a b c)
		     (finger-tree-set<=? order fa fb fc))

	       ;; =
	       (test (lset= = a b c)
		     (finger-tree-set=? order fa fb fc))

	       ;; >=
	       (test (lset<= = c b a)
		     (finger-tree-set>=? order fa fb fc))

	       ;; >
	       (test (lset< c b a)
		     (finger-tree-set>? order fa fb fc))

	       ;; difference
	       (test (sort-numbers (lset-difference = a b))
		     (finger-tree->list (finger-tree-set-difference order fa fb)))
	       (test (sort-numbers (lset-difference = a b c))
		     (finger-tree->list (finger-tree-set-difference order fa fb fc)))

	       ;; intersect
	       (test (sort-numbers (lset-intersection = a b))
		     (finger-tree->list (finger-tree-set-intersect order reduce fa fb)))
	       (test (sort-numbers (lset-intersection = a b c))
		     (finger-tree->list (finger-tree-set-intersect order reduce fa fb fc)))

	       ;; union
	       (test (sort-numbers (lset-union = a b))
		     (finger-tree->list (finger-tree-set-union order reduce fa fb)))
	       (test (sort-numbers (lset-union = a b c))
		     (finger-tree->list (finger-tree-set-union order reduce fa fb fc)))

	       ;; xor
	       (test (sort-numbers (lset-xor = a b))
		     (finger-tree->list (finger-tree-set-xor order fa fb)))
	       (test (sort-numbers (lset-xor = a b c))
		     (finger-tree->list (finger-tree-set-xor order fa fb fc)))


	       ))

	   powerset))
	powerset))
     powerset))

  ;; increasing-generator->finger-tree-set
  ;; increasing-list->finger-tree-set
  ;; increasing-vector->finger-tree-set
  (let* ((lst (iota 100))
	 (gen (list->generator lst))
	 (vec (list->vector lst)))
    (test lst
	  (finger-tree->list
	   (increasing-generator->finger-tree-set order gen)))
    (test lst
	  (finger-tree->list
	   (increasing-list->finger-tree-set order lst)))
    (test lst
	  (finger-tree->list
	   (increasing-vector->finger-tree-set order vec))))

  ;; generator->finger-tree-set
  ;; list->finger-tree-set
  ;; vector->finger-tree-set
  (let* ((unique '(6 5 3 1 4 9 7 0 2 8))
	 (dupes (append unique '(1 2 3)))
	 (expected (iota 10))
	 (reduce (lambda (x y) x)))
    
    (test expected
	  (finger-tree->list
	   (generator->finger-tree-set order
				       reduce
				       (list->generator unique))))
    (test expected
	  (finger-tree->list
	   (generator->finger-tree-set order
				       reduce
				       (list->generator dupes))))
    (test expected
	  (finger-tree->list
	   (list->finger-tree-set order
				  reduce
				  unique)))
    (test expected
	  (finger-tree->list
	   (list->finger-tree-set order
				  reduce
				  dupes)))

    (test expected
	  (finger-tree->list
	   (vector->finger-tree-set order
				    reduce
				    (list->vector unique))))
    (test expected
	  (finger-tree->list
	   (vector->finger-tree-set order
				    reduce
				    (list->vector dupes))))))

;; finger-tree-set-adjoin
;; finger-tree-set-replace
;; (now for the distinction between adjoin and replace)
(let* ((order (make-set-order car number-comparator))
       (set (increasing-list->finger-tree-set order '((1 2) (3 4) (5 6)))))

  ;; adjoin (3 9) should leave (3 4) in place
  (test '((1 2) (3 4) (5 6))
	(finger-tree->list
	 (finger-tree-set-adjoin order set '(3 9))))

  ;; replace (3 9) should replace (3 4) with (3 9)
  (test '((1 2) (3 9) (5 6))
	(finger-tree->list
	 (finger-tree-set-replace order set '(3 9)))))

(test-exit)

