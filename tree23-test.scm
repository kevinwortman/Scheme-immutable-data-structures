
(import
 (tree23)
 (chibi loop)
 (chibi test)
 (comparators)
 (scheme base)
 (scheme write)
 (srfi 1)
 (srfi 26)
 (srfi 27)
 (srfi 69)
 (util))

;; For a reasonably fast sanity check, define thorough #false. To
;; rigorously test correctness, define it #true.
(define thorough #true)

;; Change to #false if you want no unit tests, benchmark only.
(define do-tests #true)

(let-values (((small-n medium-n large-n)
	      (if (not thorough)
		  (values 10 100 100)
		  (values 1000 10000 (* 200 1000)))))

  (when do-tests

	(define cmp number-comparator)
	(define search (cute tree23-search cmp identity (constant-thunk #f) <> <>))
	(define insert (cute tree23-insert cmp select-left <> <>))
	(define delete (cute tree23-delete cmp (cute error "invalid state") <> <>))
	(define validate (cute tree23-validate cmp <>))

	(define (contains? tree q)
	  (if (search tree q)
	      #true
	      #false))

	(define src (make-random-source))
	(define rnd (random-source-make-integers src))
	(random-source-pseudo-randomize! src 0)
	(define random (random-source-make-integers src))

	(define perm (random-permutation src small-n))

	(define empty (make-tree23))
	(validate empty)

	(define digit-list (iota 10))
	(define digits (ordered-list->tree23 digit-list))
	(validate digits)

	(display "constructor, accessors\n")

	;; make-tree23
	(test #t (tree23? (make-tree23)))
	(test #t (tree23-empty? (make-tree23)))

	;; tree23?
	(test #t (tree23? (make-tree23)))
	(test #t (tree23? digits))
	(test #f (tree23? '()))
	(test #f (tree23? 7))
	(test #f (tree23? "this is a string not a tree"))

	;; tree23-empty?
	(test #t (tree23-empty? empty))
	(test #f (tree23-empty? digits))

	;; tree23-size
	(test 0 (tree23-size empty))
	(test 10 (tree23-size digits))

	;; tree23-min
	;; tree23-max
	(test 0 (tree23-min digits))
	(test 9 (tree23-max digits))

	(display "search\n")

	;; tree23-search
	(test #f (search empty 0))
	;; match
	(for-each (lambda (x)
		    (test x (search digits x)))
		  (iota 10))
	;; missing
	(for-each (lambda (x)
		    (test #f (search digits (+ x 0.5))))
		  (iota 10))

	(display "insert\n")

	;; tree23-insert
	(let ((one (insert empty 7)))
	  (test-not (tree23-empty? one))
	  (test 1 (tree23-size one))
	  (test '(7) (tree23->ordered-list one)))
	(test (iota small-n)
	      (tree23->ordered-list
	       (fold (lambda (x tree)
		       (validate tree)
		       (insert tree x))
		     (make-tree23)
		     perm)))
	;; should prevent duplicates
	(loop ((for i (up-from 0 (to 10))))
	      (test digit-list
		    (tree23->ordered-list (insert digits i))))

	(display "delete\n")

	;; tree23-delete
					; try to delete missing element
	(test 'missing (tree23-delete cmp (constant-thunk 'missing) digits 5.5))
					; straightforward deletion, one at a time
	(let ((tree (delete digits 3)))
	  (validate tree)
	  (test '(0 1 2 4 5 6 7 8 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 2))
	  (validate tree)
	  (test '(0 1 4 5 6 7 8 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 5))
	  (validate tree)
	  (test '(0 1 4 6 7 8 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 0))
	  (validate tree)
	  (test '(1 4 6 7 8 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 8))
	  (validate tree)
	  (test '(1 4 6 7 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 4))
	  (validate tree)
	  (test '(1 6 7 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 6))
	  (validate tree)
	  (test '(1 7 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 7))
	  (validate tree)
	  (test '(1 9) (tree23->ordered-list tree))

	  (set! tree (delete tree 9))
	  (validate tree)
	  (test '(1) (tree23->ordered-list tree))

	  (set! tree (delete tree 1))
	  (validate tree)
	  (test '() (tree23->ordered-list tree)))
					; try other deletion orders
	#|
	(loop ((for i (up-from 0 (to small-n))))
	(fold (lambda (q tree)
	(validate tree)
	(delete tree q))
	(ordered-list->tree23 (iota small-n))
	(random-permutation src small-n)))
	|#
	(display "iterator\n")

	;; make-tree23-iter
	;; tree23-iter-empty?
	;; tree23-iter-peek
	(test #t (tree23-iter-empty? (make-tree23-iter empty)))
	(test #f (tree23-iter-empty? (make-tree23-iter digits)))
	(test 0 (tree23-iter-peek (make-tree23-iter digits)))

	;; tree23-iter-pop
	(define (to-list/iter tree)
	  (let loop ((i (make-tree23-iter tree))
		     (lst '()))
	    (if (tree23-iter-empty? i)
		(reverse lst)
		(loop (tree23-iter-pop i)
		      (cons (tree23-iter-peek i) lst)))))
	(test digit-list (to-list/iter digits))

	(display "scaffold\n")

	;; make-tree23-scaffold
	;; tree23-scaffold-insert-max
	;; tree23-scaffold-finish
	(test #t (tree23-empty? (tree23-scaffold-finish (make-tree23-scaffold))))
	(test '(1)
	      (tree23->ordered-list
	       (tree23-scaffold-finish
		(tree23-scaffold-insert-max (make-tree23-scaffold) 1))))
	(test '(1 2 3 4 5)
	      (tree23->ordered-list
	       (tree23-scaffold-finish
		(tree23-scaffold-insert-max 
		 (tree23-scaffold-insert-max 
		  (tree23-scaffold-insert-max 
		   (tree23-scaffold-insert-max 
		    (tree23-scaffold-insert-max 
		     (make-tree23-scaffold) 1) 2) 3) 4) 5))))
	(loop ((for i (up-from 0 (to small-n))))
	      (test (iota i)
		    (tree23->ordered-list
		     (tree23-scaffold-finish
		      (fold (flip tree23-scaffold-insert-max)
			    (make-tree23-scaffold)
			    (iota i))))))

	(display "filter, fold, map\n")

	;; tree23-filter
	(test-assert (tree23-empty? (tree23-filter even? empty)))
	(let ((tree (tree23-filter even? digits)))
	  (validate tree)
	  (test '(0 2 4 6 8) (tree23->ordered-list tree)))
	(test-assert (tree23-empty? (tree23-filter string? digits)))

	;; tree23-fold
	(test 7 (tree23-fold + 7 empty))
	(test (+ 7 (fold + 0 (iota 10)))
	      (tree23-fold + 7 digits))

	;; tree23-map
	(test-assert (tree23-empty? (tree23-map add1 cmp select-left empty)))
	;; add1 happens to be nondecreasing
	(let ((tree (tree23-map add1 cmp select-left digits)))
	  (validate tree)
	  (test '(1 2 3 4 5 6 7 8 9 10) (tree23->ordered-list tree)))
	;; negation is non-nondecreasing
	(let ((tree (tree23-map - cmp select-left digits)))
	  (validate tree)
	  (test '(-9 -8 -7 -6 -5 -4 -3 -2 -1 0) (tree23->ordered-list tree)))

	;; tree23-map/nondecreasing
	(test-assert (tree23-empty? (tree23-map/nondecreasing add1 empty)))
	(let ((tree (tree23-map/nondecreasing add1 digits)))
	  (validate tree)
	  (test '(1 2 3 4 5 6 7 8 9 10) (tree23->ordered-list tree)))

	(display "list conversion\n")

	;; tree23->ordered-list
	;; ordered-list->tree23
	(test '() (tree23->ordered-list empty))
	(test digit-list (tree23->ordered-list digits))
	(let* ((list (iota medium-n))
	       (tree (ordered-list->tree23 list)))
	  (validate tree)
	  (test list (tree23->ordered-list tree)))

	;; list->tree23
	(test-assert (tree23-empty? (list->tree23 cmp select-left '())))
	(test '(1 2 3)
	      (tree23->ordered-list
	       (list->tree23 cmp select-left '(2 1 3))))
	;; duplicates
	(test '(1 2 3)
	      (tree23->ordered-list
	       (list->tree23 cmp select-left '(1 2 3 3 1 2 1 3 2 3 1))))

	(display "range query\n")

	;; tree23-range
	(test '()
	      (tree23->ordered-list
	       (tree23-range (cute < <> 3) (cute > <> 7) empty)))
	(define (make< x) (cute < <> x))
	(define (make> x) (cute > <> x))
	(define (digit-range mn mx)
	  (tree23->ordered-list
	   (tree23-range (make< mn) (make> mx) digits)))
	(test '(3 4 5 6 7) (digit-range 3 7))
	(test '() (digit-range -5 -1))
	(test '() (digit-range 11 12))
	(test (iota 10) (digit-range -1 11))
	(test (iota 10) (digit-range 0 10))
	(test '(1 2) (digit-range 1 2))
	(test '(7 8 9) (digit-range 7 9))

	(display "correctness gauntlet\n")
	(loop ((for i (up-from 0 (to (/ small-n 10)))))
	      (let* ((n (add1 (random small-n)))
		     (sorted (iota n))
		     (perm (random-permutation src n)))
		;; build with insert
		(let ((tree (fold (flip insert) (make-tree23) perm)))
		  ;; correctness
		  (validate tree)

		  ;; accessors
		  (test-not (tree23-empty? tree))
		  (test n (tree23-size tree))
		  (test 0 (tree23-min tree))
		  (test (sub1 n) (tree23-max tree))

		  ;; search
		  (loop ((for j (up-from -10 (to (+ n 10)))))
			(test (find (cute = j <>) sorted)
			      (search tree j)))

		  ;; iterator
		  (test sorted (to-list/iter tree))

		  ;; filter
		  (test (filter even? sorted)
			(tree23->ordered-list
			 (tree23-filter even? tree)))

		  ;; fold
		  (define (wacky-add x sum)
		    (+ x (- sum)))
		  (test (fold wacky-add 0 sorted)
			(tree23-fold wacky-add 0 tree))

		  ;; map
		  (test (map add1 sorted)
			(tree23->ordered-list
			 (tree23-map add1 cmp select-left tree)))
		  (test (map add1 sorted)
			(tree23->ordered-list
			 (tree23-map/nondecreasing add1 tree)))

		  ;; list conversion
		  (test sorted (tree23->ordered-list tree))
		  (test sorted 
			(tree23->ordered-list
			 (ordered-list->tree23 sorted)))
		  (test sorted
			(tree23->ordered-list
			 (list->tree23 cmp select-left sorted)))
		  (test sorted
			(tree23->ordered-list
			 (list->tree23 cmp select-left
				       (append perm
					       sorted
					       (random-permutation src n)
					       perm))))

		  ;; range queries
		  (loop ((for j (up-from 0 (to small-n))))
			(let* ((a (random n))
			       (b (random n))
			       (mn (min a b))
			       (mx (max a b))
			       (exp (filter (cute <= mn <> mx) sorted))
			       (got (tree23->ordered-list
				     (tree23-range (make< mn) (make> mx) tree))))
			  (test exp got)))
		  
		  ;; deletion
		  (let ((empty (fold (lambda (q tree)
				       (validate tree)
				       (delete tree q))
				     tree
				     (random-permutation src n))))
		    (test-assert (tree23-empty? empty)))

		  )))

	(display "alternating insert-delete\n")
	(let* ((n small-n)
	       (hash (alist->hash-table (map (cute cons <> #true) (iota n)))))
	  (let loop ((k medium-n)
		     (tree (ordered-list->tree23 (iota n))))
	    (validate tree)
	    (unless (zero? k)
		    (let* ((q (random n))
			   (in (hash-table-ref/default hash q #false)))
		      (test in (contains? tree q))
		      (if in
			  (begin (hash-table-delete! hash q)
				 (loop (sub1 k) (delete tree q)))
			  (begin (hash-table-set! hash q #true)
				 (loop (sub1 k) (insert tree q))))))))

	) ; end of tests

  (display "efficiency gauntlet\n")

  (let ((sorted (iota large-n))
	(unsorted (random-permutation src large-n)))
    (newline)
    (display "All trials for n = ") (display (number->string large-n))
    (newline)
    (timer "build from ordered list" (ordered-list->tree23 sorted))
    (timer "insert" (list->tree23 cmp select-left unsorted))
    (let ((tree (ordered-list->tree23 sorted)))
      (timer "delete" (fold (flip delete) tree unsorted))
      (timer "filter" (tree23-filter even? tree))
      (timer "fold" (tree23-fold + 0 tree))
      (timer "map" (tree23-map add1 cmp select-left tree))
      (timer "map/nondecreasing" (tree23-map/nondecreasing add1 tree))
      (timer "convert to list" (tree23->ordered-list tree))
      (timer "iterate"
	     (do ((iter (make-tree23-iter tree) (tree23-iter-pop iter)))
		 ((tree23-iter-empty? iter))))))

) ; let-values
