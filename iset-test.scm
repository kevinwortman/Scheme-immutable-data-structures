
(import
 (chibi loop)
 (chibi test)
 (comparators)
 (iset)
 (scheme base)
 (scheme write)
 (selector)
 (srfi 1)
 (srfi 8)
 (srfi 26)
 (srfi 95)
 (util))

(define n 8)

(define (list-set=? . lists)
  (boolean (apply lset= = lists)))
(define (list-set<=? . lists)
  (boolean (apply lset<= = lists)))
(define (list-set<? left right . rest)
  (and (list-set<=? left right)
       (not (list-set=? left right))
       (or (null? rest)
	   (apply list-set<? right (car rest) (cdr rest)))))
(define (list-set>=? . lists)
  (apply list-set<=? (reverse lists)))
(define (list-set>? . lists)
  (apply list-set<? (reverse lists)))

(define (test-list-set=? a b)
  (test-assert (list-set=? a b)))

;; iset
;; empty with default comparator and selector
(define empty (iset))
(test-assert (iset? empty))
(test-assert (iset-empty? empty))
(test-assert (eq? default-comparator (iset-comparator empty)))
(test-assert (eq? left-selector (iset-selector empty)))
;; comparator, selector overrides
(let ((set (iset number-comparator 1 2 3)))
  (test-assert (iset? set))
  (test '(1 2 3) (iset->ordered-list set))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? left-selector (iset-selector set))))
(let ((set (iset right-selector 1 2 3)))
  (test-assert (iset? set))
  (test '(1 2 3) (iset->ordered-list set))
  (test-assert (eq? default-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set))))
(let ((set (iset number-comparator right-selector 1 2 3)))
  (test-assert (iset? set))
  (test '(1 2 3) (iset->ordered-list set))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set))))
(let ((set (iset right-selector number-comparator 1 2 3)))
  (test-assert (iset? set))
  (test '(1 2 3) (iset->ordered-list set))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set))))
(let ((like (iset number-comparator right-selector)))
  (test-assert (eq? number-comparator (iset-comparator like)))
  (test-assert (eq? right-selector (iset-selector like)))
  (let ((set (iset like 1 2 3)))
    (test-assert (iset? set))
    (test '(1 2 3) (iset->ordered-list set))
    (test-assert (eq? number-comparator (iset-comparator set)))
    (test-assert (eq? right-selector (iset-selector set))))
  (let ((set (iset like integer-comparator 1 2 3)))
    (test-assert (iset? set))
    (test '(1 2 3) (iset->ordered-list set))
    (test-assert (eq? integer-comparator (iset-comparator set)))
    (test-assert (eq? right-selector (iset-selector set))))
  (let ((set (iset like left-selector 1 2 3)))
    (test-assert (iset? set))
    (test '(1 2 3) (iset->ordered-list set))
    (test-assert (eq? number-comparator (iset-comparator set)))
    (test-assert (eq? left-selector (iset-selector set)))))
;; nonempty
(define three (iset 1 2 3))
(test-assert (iset? three))
(test 3 (iset-size three))
(test '(1 2 3) (iset->ordered-list three))
;; removes duplicates
(test '(1 2 3) (iset->ordered-list (iset 1 2 2 3 3 3 3 1 2)))

;; iset?
(test-assert (iset? empty))
(test-assert (iset? three))
(test-not (iset? '(1 2 3)))

;; iset-comparator
(test-assert (comparator? (iset-comparator three)))

;; iset-selector
(test-assert (selector? (iset-selector three)))

;; iset-empty?
(test-assert (iset-empty? empty))
(test-not (iset-empty? three))

;; iset-size
(test-assert 0 (iset-size empty))
(test-assert 3 (iset-size three))

;; iset-difference
;; iset-intersection
;; iset-union
;; iset-xor
;; iset<?
;; iset<=?
;; iset=?
;; iset>=?
;; iset>?
;; tricky special cases
(test-assert (iset<? (iset) (iset 0)))
(test-assert (iset<? (iset 0) (iset 1 0)))
(test-assert (list-set<? '() '(0) '(1 0)))
(test-assert (iset<? (iset) (iset 0) (iset 1 0)))
(test (list-set<? '() '(0) '(1 0))
      (iset<? (iset) (iset 0) (iset 1 0)))
;; exhaustive tests
(let ((U (powerset n)))
  (loop ((for a (in-list U)))
    (loop ((for b (in-list U)))
      (let ((as (list->iset a))
	    (bs (list->iset b)))

	;; binary relations
	(test (list-set<?  a b) (iset<?  as bs))
	(test (list-set<=? a b) (iset<=? as bs))
	(test (list-set=?  a b) (iset=?  as bs))
	(test (list-set>=? a b) (iset>=? as bs))
	(test (list-set>?  a b) (iset>?  as bs))

	;; binary operations
	(test-list-set=? (lset-difference = a b)
			 (iset->ordered-list (iset-difference as bs)))
	(test-list-set=? (lset-intersection = a b)
			 (iset->ordered-list (iset-intersection as bs)))
	(test-list-set=? (lset-union = a b)
			 (iset->ordered-list (iset-union as bs)))
	(test-list-set=? (lset-xor = a b)
			 (iset->ordered-list (iset-xor as bs)))

	(loop ((for c (in-list U)))
	  (let ((cs (list->iset c)))
	    
	    ;; trinary relations
	    (test (list-set<?  a b c) (iset<?  as bs cs)) ;; failing
	    (test (list-set<=? a b c) (iset<=? as bs cs))
	    (test (list-set=?  a b c) (iset=?  as bs cs))
	    (test (list-set>=? a b c) (iset>=? as bs cs))
	    ;;(test (list-set>?  a b c) (iset>?  as bs cs)) ;; failing

	    (let ((x (list-set<? a b c))
		  (y (iset<? as bs cs)))
	      (unless (equal? x y)
		      (newline)
		      (display a)
		      (display b)
		      (display c)
		      (display x)
		      (display " vs ")
		      (display y)
		      (newline)))

	    ;; trinary operations
	    (test-list-set=? (lset-difference = a b c)
			     (iset->ordered-list (iset-difference as bs cs)))
	    (test-list-set=? (lset-intersection = a b c)
			     (iset->ordered-list (iset-intersection as bs cs)))
	    (test-list-set=? (lset-union = a b c)
			     (iset->ordered-list (iset-union as bs cs)))
	    (test-list-set=? (lset-xor = a b c)
			     (iset->ordered-list (iset-xor as bs cs)))))))))

(loop ((for elts (in-list (powerset n))))
      (let ((set (list->iset elts)))
	(loop ((for x (in-list elts)))
	      ;; iset-member?
	      (test (boolean (member x elts)) (iset-member? set x))

	      ;; iset-min
	      ;; iset-max
	      (test (apply min elts) (iset-min set))
	      (test (apply max elts) (iset-max set))

	      ;; iset-filter
	      (test-list-set=? (filter even? elts)
			       (iset->ordered-list (iset-filter even? set)))

	      ;; iset-fold
	      (test (fold cons '() (sort elts <))
		    (iset-fold cons '() set))

	      ;; iset-map/nondecreasing
	      (test-list-set=? (map add1 elts)
			       (iset->ordered-list
				(iset-map/nondecreasing add1 set)))

	      ;; iset-map
	      (let ((halve (cute quotient <> 2)))
		(test-list-set=? (map halve elts)
				 (iset->ordered-list
				  (iset-map halve set)))))

	(loop ((for x (up-from -1 (to (add1 n)))))
	      ;; iset-find
	      (test (find (cute = x <>) elts)
		    (iset-find set x (constant-thunk #false)))

	      ;; iset-include
	      (test-list-set=? (lset-adjoin = elts x)
			       (iset->ordered-list
				(iset-include set x)))

	      ;; iset-exclude
	      (test-list-set=? (lset-difference = elts (list x))
			       (iset->ordered-list
				(iset-exclude set x))))

	;; iset-between
	(loop ((for min (up-from -1 (to n))))
	      (loop ((for max (up-from min (to (add1 n)))))
		    ;; [min max]
		    (test-list-set=? (filter (cute <= min <> max) elts)
				     (iset->ordered-list
				      (iset-between set min #t max #t)))
		    ;; [min max)
		    (test-list-set=? (filter (cute <= min <> (sub1 max)) elts)
				     (iset->ordered-list
				      (iset-between set min #t max #f)))
		    ;; (min max]
		    (test-list-set=? (filter (cute <= (add1 min) <> max) elts)
				     (iset->ordered-list
				      (iset-between set min #f max #t)))
		    ;; (min max)
		    (test-list-set=? (filter (cute <= (add1 min) <> (sub1 max)) elts)
				     (iset->ordered-list
				      (iset-between set min #f max #f)))


		    ))))

;; iset->ordered-list
;; list->iset
;; ordered-list->iset
(loop ((for k (up-from 0 (to 100))))
  (let* ((lst (iota k))
	 (ord (ordered-list->iset lst))
	 (unord (list->iset (append lst lst))))
    (test-assert (iset? ord))
    (test-assert (iset? unord))
    (test lst (iset->ordered-list ord))
    (test lst (iset->ordered-list unord))))
;; list->iset overrides
(let ((set (list->iset '(1 2 3))))
  (test-assert (eq? default-comparator (iset-comparator set)))
  (test-assert (eq? left-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
(let ((set (list->iset number-comparator '(1 2 3))))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? left-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
(let ((set (list->iset right-selector '(1 2 3))))
  (test-assert (eq? default-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
(let ((set (list->iset (iset number-comparator right-selector) '(1 2 3))))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
;; ordered-list->iset overrides
(let ((set (ordered-list->iset '(1 2 3))))
  (test-assert (eq? default-comparator (iset-comparator set)))
  (test-assert (eq? left-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
(let ((set (ordered-list->iset number-comparator '(1 2 3))))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? left-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
(let ((set (ordered-list->iset right-selector '(1 2 3))))
  (test-assert (eq? default-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))
(let ((set (ordered-list->iset (iset number-comparator right-selector) '(1 2 3))))
  (test-assert (eq? number-comparator (iset-comparator set)))
  (test-assert (eq? right-selector (iset-selector set)))
  (test '(1 2 3) (iset->ordered-list set)))

;; iset-include and iset-exclude track min, max, and size correctly
(define (test-metadata inf lst set)
  (let ((n (length lst)))
    (test-list-set=? lst (iset->ordered-list set))
    (test n (iset-size set))
    (when (positive? n)
	  (test (fold min inf lst) (iset-min set))
	  (test (fold max -1 lst) (iset-max set)))))

(define (test-include+exclude perm)
  (let* ((n (length perm))
	 (inf (add1 n)))
    ;; insert from empty
    (let loop ((lst '())
	       (set (iset))
	       (perm perm))
      (test-metadata inf lst set)
      (unless (null? perm)
	      (receive (x perm) (car+cdr perm)
		       (loop (lset-adjoin = lst x)
			     (iset-include set x)
			     perm))))
    ;; delete from full
    (let ((everything (iota (length perm))))
      (let loop ((lst everything)
		 (set (ordered-list->iset everything))
		 (perm perm))
	(test-metadata inf lst set)
	(unless (null? perm)
		(receive (x perm) (car+cdr perm)
			 (loop (lset-difference = lst (list x))
			       (iset-exclude set x)
			       perm)))))))

;; All elements are distinct.
(for-each test-include+exclude (pseudorandom-permutations n 100 0))

;; Repeated elements.
(for-each (lambda (perm1 perm2)
	    (test-include+exclude (append perm1 perm2)))
	  (pseudorandom-permutations n 100 0)
	  (pseudorandom-permutations n 20 0))

