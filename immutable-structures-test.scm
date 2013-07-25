
(import (chibi test)
	(scheme base)
	(scheme write)
	(srfi 1))

(include "immutable-structures-impl.scm")

(define *ideque-0* (make-ideque-empty))
(define *ideque-1* (make-ideque-single 7))
(define *ideque-4* (make-ideque-deep-spineless '(1 2) '(4 3)))
(define *ideque-16* (make-ideque-deep '(0 1)
				      (make-ideque-deep-spineless '((2 3 4)
								    (5 6 7))
								  '((11 12 13)
								    (8 9 10)))
				      '(15 14)))

(test-assert (ideque? (ideque)))
(test-assert (ideque-empty? (ideque)))

(test 5 (ideque-length (ideque 1 2 3 4 5)))
(test 1 (ideque-front (ideque 1 2 3 4 5)))
(test 5 (ideque-back (ideque 1 2 3 4 5)))

(test-assert (ideque? *ideque-0*))
(test-assert (ideque? *ideque-1*))
(test-assert (ideque? *ideque-4*))

(test-assert (ideque-empty? *ideque-0*))
(test-assert (ideque-single? *ideque-1*))
(test-assert (ideque-deep? *ideque-4*))
(test-not (ideque-empty? *ideque-1*))
(test-not (ideque-empty? *ideque-4*))
(test-assert (ideque-empty? (ideque-deep-spine *ideque-4*)))

(test 0 (ideque-length *ideque-0*))
(test 1 (ideque-length *ideque-1*))
(test 4 (ideque-length *ideque-4*))
(test 1000 (ideque-length (list->ideque (iota 1000))))

(define (test-ideque-case root)
  (ideque-case (root x l s r)
	       'empty
	       'single
	       'deep))
(test 'empty (test-ideque-case *ideque-0*))
(test 'single (test-ideque-case *ideque-1*))
(test 'deep (test-ideque-case *ideque-4*))

(test '() (ideque->list *ideque-0*))
(test '(7) (ideque->list *ideque-1*))
(test '(1 2 3 4) (ideque->list *ideque-4*))
(test (iota 16) (ideque->list *ideque-16*))

(test (iota 20) (ideque->list (ideque-append-list *ideque-16* '(16 17 18 19))))

(test (iota 16) (ideque->list (list->ideque (iota 16))))
(test (iota 1000) (ideque->list (list->ideque (iota 1000))))

(test 7 (ideque-front *ideque-1*))
(test 1 (ideque-front *ideque-4*))
(test 0 (ideque-front *ideque-16*))

(test 7 (ideque-back *ideque-1*))
(test 4 (ideque-back *ideque-4*))
(test 15 (ideque-back *ideque-16*))

(test '(9) (ideque->list (ideque-push-front *ideque-0* 9)))
(test '(9 1 2 3 4) (ideque->list (ideque-push-front *ideque-4* 9)))
(test (cons 9 (iota 16)) (ideque->list (ideque-push-front *ideque-16* 9)))

(test '(9) (ideque->list (ideque-push-back *ideque-0* 9)))
(test '(1 2 3 4 9) (ideque->list (ideque-push-back *ideque-4* 9)))
(test (append (iota 16) (list 9)) (ideque->list (ideque-push-back *ideque-16* 9)))

(define *ideque-1000* (fold (flip ideque-push-back) *ideque-0* (iota 1000)))

(test (iota 1000) (ideque->list *ideque-1000*))
(test (reverse (iota 1000)) (ideque->list (fold (flip ideque-push-front) *ideque-0* (iota 1000))))

(test '() (ideque->list (ideque-pop-front *ideque-1*)))
(test '(2 3 4) (ideque->list (ideque-pop-front *ideque-4*)))
(test (cdr (iota 16)) (ideque->list (ideque-pop-front *ideque-16*)))

(test '() (ideque->list (ideque-pop-back *ideque-1*)))
(test '(1 2 3) (ideque->list (ideque-pop-back *ideque-4*)))
(test (take (iota 16) 15) (ideque->list (ideque-pop-back *ideque-16*)))

;; Test that we can pop successively without crashing.
(test-assert (ideque-empty? (fold (lambda (i deque)
				    (ideque-pop-front deque))
				  *ideque-1000*
				  (iota 1000))))
(test-assert (ideque-empty? (fold (lambda (i deque)
				    (ideque-pop-back deque))
				  *ideque-1000*
				  (iota 1000))))

(test (iota 10) (ideque->list (ideque-append-binary (list->ideque (iota 5))
						    (list->ideque (iota 5 5)))))
(test (iota 10000) (ideque->list (ideque-append-binary (list->ideque (iota 6000))
						      (list->ideque (iota 4000 6000)))))

(test (iota 36000) (ideque->list (ideque-append (list->ideque (iota 11000))
						(list->ideque (iota 12000 11000))
						(list->ideque (iota 13000 23000)))))

(test 200000 (length (ideque->list (list->ideque (iota 200000)))))

(test 500 (ideque-length (ideque-filter even? (list->ideque (iota 1000)))))

(test (fold + 0 (iota 2000))
      (ideque-fold + 0 (list->ideque (iota 2000))))

(test '(2 3 4 5) (ideque->list (ideque-map add1 (ideque 1 2 3 4))))
(test (map add1 (iota 2000))
      (ideque->list (ideque-map add1 (list->ideque (iota 2000)))))

(test '(1 2 3 4) (ordered-list-union! (list 1 2) (list 2 3 4) < iset-merger-left))
(test '(1) (ordered-list-intersection! (list 1 2) (list 1 3 4) < iset-merger-left))
(test '(1) (ordered-list-difference! (list 1 2 3) (list 2 3 4) <))
(test '(1) (ordered-list-difference! (list 1 3) (list 2 3) <))

(test 1 (iset-merger-left 1 2))
(test 2 (iset-merger-right 1 2))

(test (values '() '()) (list-split '()))
(test (values '(0) '()) (list-split (iota 1)))
(test (values '(2 0) '(1)) (list-split (iota 3)))
(test (values '(3 1) '(2 0)) (list-split (iota 4)))
(test (values '(4 2 0) '(3 1)) (list-split (iota 5)))
(test (values '(5 3 1) '(4 2 0)) (list-split (iota 6)))

(let-values (((left right) (list-split (iota 1001))))
  (test 501 (length left))
  (test 500 (length right)))

;; trace (list->ordered-list (list 1 2 3 4) <)
(test '() (list->ordered-list '() < iset-merger-left))
(test '(1) (list->ordered-list (list 1) < iset-merger-left))
(test '(2) (list->ordered-list (list 2) < iset-merger-left))
(test '(1 2) (list->ordered-list (list 1 2) < iset-merger-left))
(test '(3 4) (list->ordered-list (list 3 4) < iset-merger-left))
(test '(1 2 3 4) (ordered-list-union! (list 1 2) (list 3 4) < iset-merger-left))

(test '() (list->ordered-list '() < iset-merger-left))
(test (iota 4) (ordered-list-union! (list 0 1) (list 2 3) < iset-merger-left))
(test (iota 10) (list->ordered-list (iota 10) < iset-merger-left))
(test (iota 10) (list->ordered-list (reverse (iota 10)) < iset-merger-left))
(test '(1 2 3 4 5 6 7) (list->ordered-list (list 3 1 2 7 6 4 5) < iset-merger-left))

(test-assert (btree0? (make-btree0)))
(test-not (btree0? "foo"))

(define *btree1-fixture* (make-btree1 'leaf))
(test-assert (btree1? *btree1-fixture*))
(test 'leaf (btree1-child *btree1-fixture*))

(define *btree2-fixture* (make-btree2 'left 7 'right))

(test 'left (btree2-left *btree2-fixture*))
(test 7 (btree2-elt *btree2-fixture*))
(test 'right (btree2-right *btree2-fixture*))

(define *btree-leaf-fixture* (make-btree-leaf 7))
(test 7 (btree2-elt *btree-leaf-fixture*))
(test-assert (btree0? (btree2-left *btree-leaf-fixture*)))
(test-assert (btree0? (btree2-right *btree-leaf-fixture*)))

(define *fib-tree-3* (make-btree2 (make-btree2 (make-btree-leaf 1)
					       2
					       (make-btree1 (make-btree0)))
				  3
				  (make-btree1 (make-btree-leaf 4))))

(test '(7) (btree-fold #t cons '() (make-btree-leaf 7)))
(test '(4 3 2 1) (btree-fold #t cons '() *fib-tree-3*))
(test '(1 2 3 4) (btree-fold #f cons '() *fib-tree-3*))
(test 10 (btree-fold #t + 0 *fib-tree-3*))

(btree-fold #t cons '() (make-btree-leaf 7))

(test '(1 2 3 4) (btree-range->ordered-list *fib-tree-3* 1 #t 4 #t <))
(test '(2 3 4) (btree-range->ordered-list *fib-tree-3* 1 #f 4 #t <))
(test '(1 2 3) (btree-range->ordered-list *fib-tree-3* 1 #t 4 #f <))
(test '(2 3) (btree-range->ordered-list *fib-tree-3* 2 #t 3 #t <))

(test-assert (btree0? (btree-skip-unary (make-btree0))))
(test-assert (eq? *fib-tree-3* (btree-skip-unary *fib-tree-3*)))
(test 4 (btree2-elt (btree-skip-unary (btree2-right *fib-tree-3*))))

(test-assert (btree-brotherly? *fib-tree-3*))
(test-assert (btree-balanced? *fib-tree-3*))
(test-assert (btree-inorder? *fib-tree-3* <))
(test-assert (btree-correct? *fib-tree-3* <))
(define *btree-not-brotherly* (make-btree2 (make-btree1 (make-btree-leaf 1))
					   2
					   (make-btree1 (make-btree-leaf 3))))
(test-not (btree-brotherly? *btree-not-brotherly*))
(test-assert (btree-balanced? *btree-not-brotherly*))
(test-assert (btree-inorder? *btree-not-brotherly* <))
(test-not (btree-correct? *btree-not-brotherly* <))
(define *btree-not-balanced* (make-btree2 (make-btree-leaf 1)
					  2
					  (make-btree1 (make-btree-leaf 3))))
(test-assert (btree-brotherly? *btree-not-balanced*))
(test-not (btree-balanced? *btree-not-balanced*))
(test-assert (btree-inorder? *btree-not-balanced* <))
(test-not (btree-correct? *btree-not-balanced* <))
(define *btree-not-inorder* (make-btree2 (make-btree-leaf 1)
					 3
					 (make-btree-leaf 2)))
(test-assert (btree-brotherly? *btree-not-inorder*))
(test-assert (btree-balanced? *btree-not-inorder*))
(test-not (btree-inorder? *btree-not-inorder* <))
(test-not (btree-correct? *btree-not-inorder* <))

(define (absent-thunk)
  'absent)
(define (present-proc elt)
  'present)
(test 'absent (btree-search *fib-tree-3* 0 absent-thunk present-proc <))
(test 'present (btree-search *fib-tree-3* 1 absent-thunk present-proc <))
(test 'present (btree-search *fib-tree-3* 2 absent-thunk present-proc <))
(test 'present (btree-search *fib-tree-3* 3 absent-thunk present-proc <))
(test 'present (btree-search *fib-tree-3* 4 absent-thunk present-proc <))
(test 'absent (btree-search *fib-tree-3* 5 absent-thunk present-proc <))

(define *btree-quad* (make-trio/full 'a 1 'b 2 'c 3 'd))
(test-assert (btree2? *btree-quad*))
(test-assert (btree2? (btree2-left *btree-quad*)))
(test-assert (btree2? (btree2-right *btree-quad*)))
(test 'a (btree2-left (btree2-left *btree-quad*)))
(test 'b (btree2-right (btree2-left *btree-quad*)))
(test 'c (btree2-left (btree2-right *btree-quad*)))
(test 'd (btree2-right (btree2-right *btree-quad*)))
(test 2 (btree2-elt *btree-quad*))
(test 1 (btree2-elt (btree2-left *btree-quad*)))
(test 3 (btree2-elt (btree2-right *btree-quad*)))

(define *removed* (ordered-list->btree (iota 10)))
(test-assert (btree-correct? *removed* <))

(set! *removed* (btree-remove *removed* 5 <))
(test-assert (btree-correct? *removed* <))
(test '(0 1 2 3 4 6 7 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 2 <))
(test-assert (btree-correct? *removed* <))
(test '(0 1 3 4 6 7 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 0 <))
(test-assert (btree-correct? *removed* <))
(test '(1 3 4 6 7 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 4 <))
(test-assert (btree-correct? *removed* <))
(test '(1 3 6 7 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 6 <))
(test-assert (btree-correct? *removed* <))
(test '(1 3 7 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 3 <))
(test-assert (btree-correct? *removed* <))
(test '(1 7 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 7 <))
(test-assert (btree-correct? *removed* <))
(test '(1 8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 1 <))
(test-assert (btree-correct? *removed* <))
(test '(8 9) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 9 <))
(test-assert (btree-correct? *removed* <))
(test '(8) (btree->ordered-list *removed*))

(set! *removed* (btree-remove *removed* 8 <))
(test-assert (btree-correct? *removed* <))
(test '() (btree->ordered-list *removed*))
(test-assert (btree0? *removed*))

;; efficiency test
(test '() (fold (lambda (x root)
		  (btree-remove root x <))
		(make-btree0)
		(iota 10000)))

(define *inserted* (make-btree0))
(test-assert (btree-correct? *inserted* <))
(test '() (btree->ordered-list *inserted*))

(set! *inserted* (btree-insert *inserted* 1 <))
(test-assert (btree-correct? *inserted* <))
(test '(1) (btree->ordered-list *inserted*))

(set! *inserted* (btree-insert *inserted* 5 <))
(test-assert (btree2? *inserted*))
(test 5 (btree2-elt *inserted*))
(test-assert (btree2? (btree2-left *inserted*)))
(test 1 (btree2-elt (btree2-left *inserted*)))
(test-assert (btree0? (btree2-left (btree2-left *inserted*))))
(test-assert (btree0? (btree2-right (btree2-left *inserted*))))
(test-assert (btree1? (btree2-right *inserted*)))
(test-assert (btree0? (btree1-child (btree2-right *inserted*))))
(test-assert (btree-correct? *inserted* <))
(test '(1 5) (btree->ordered-list *inserted*))

(set! *inserted* (btree-insert *inserted* 9 <))
(test-assert (btree-correct? *inserted* <))
(test '(1 5 9) (btree->ordered-list *inserted*))

(set! *inserted* (btree-insert *inserted* 13 <))
(test-assert (btree-correct? *inserted* <))
(test '(1 5 9 13) (btree->ordered-list *inserted*))

(set! *inserted* (btree-insert *inserted* 10 <))
(test-assert (btree-correct? *inserted* <))
(test '(1 5 9 10 13) (btree->ordered-list *inserted*))

(set! *inserted* (btree-insert *inserted* 7 <))
(test-assert (btree-correct? *inserted* <))
(test '(1 5 7 9 10 13) (btree->ordered-list *inserted*))

;; insert many elements and check correctness after each
(define (test-insert-many n)
  (do ((i 0 (add1 i))
       (root (make-btree0) (btree-insert root i <)))
      ((= i n) #f)
    (test-assert (btree-correct? root <))
    (test (iota i) (btree->ordered-list root))))

(test-insert-many 3)
(test-insert-many 200)

;; efficiency test
(test (iota 10000) (btree->ordered-list (fold (lambda (x root)
						(btree-insert root x <))
					      (make-btree0)
					      (iota 10000))))

;; spine constructors
(test-assert (spine-null? (make-spine-null)))
(test-assert (spine-half? (make-spine-half 7 (make-btree0) (make-spine-null))))
(test-not (spine-half? (make-spine-full 7 (make-btree0) (make-spine-null))))

;; spine-insert-max
(define (spine->ordered-list spine)
  (btree->ordered-list (spine->btree spine)))
;; 0
(define *spine-0* (make-spine-null))
(test-assert (spine-null? *spine-0*))
(test '() (spine->ordered-list *spine-0*))
;; 1
(define *spine-1* (spine-insert-max *spine-0* 1))
(test-assert (spine? *spine-1*))
(test-assert (spine-full? *spine-1*))
(test-assert (btree0? (spine-left *spine-1*)))
(test-assert (spine-null? (spine-up *spine-1*)))
(test '(1) (spine->ordered-list *spine-1*))
;; 1 1/2
(define *spine-1-1/2* (spine-insert-max *spine-1* 2))
(test-assert (spine? *spine-1-1/2*))
(test '(1 2) (spine->ordered-list *spine-1-1/2*))
(test-assert (spine-full? *spine-1-1/2*))
(test 2 (spine-elt *spine-1-1/2*))
(test-assert (btree0? (spine-left *spine-1-1/2*)))
(test-assert (spine? (spine-up *spine-1-1/2*)))
(test-assert (spine-half? (spine-up *spine-1-1/2*)))
(test 1 (spine-elt (spine-up *spine-1-1/2*)))
(test-assert (btree0? (spine-left (spine-up *spine-1-1/2*))))
(test-assert (spine-null? (spine-up (spine-up *spine-1-1/2*))))
;; 1 1
(define *spine-1-1* (spine-insert-max *spine-1-1/2* 3))
(test-assert (spine? *spine-1-1*))
(test '(1 2 3) (spine->ordered-list *spine-1-1*))
;; 1 1/2 1/2
(define *spine-1-1/2-1/2* (spine-insert-max *spine-1-1* 4))
(test-assert (spine? *spine-1-1/2-1/2*))
(test '(1 2 3 4) (spine->ordered-list *spine-1-1/2-1/2*))
;; 1 1/2 1/2
(define *spine-1-1-1/2* (spine-insert-max *spine-1-1/2-1/2* 5))
(test-assert (spine? *spine-1-1-1/2*))
(test '(1 2 3 4 5) (spine->ordered-list *spine-1-1-1/2*))
;; 1 1/2 1
(define *spine-1-1/2-1* (spine-insert-max *spine-1-1-1/2* 6))
(test-assert (spine? *spine-1-1/2-1*))
(test '(1 2 3 4 5 6) (spine->ordered-list *spine-1-1/2-1*))
(define *spine-1-1-1* (spine-insert-max *spine-1-1/2-1* 7))
(test-assert (spine? *spine-1-1-1*))
(test '(1 2 3 4 5 6 7) (spine->ordered-list *spine-1-1-1*))

;; spine->btree
(test-assert (btree0? (spine->btree (make-spine-null))))
(define *spine-4* (make-spine-full 4 (make-btree0)
		    (make-spine-half 3 (make-btree0)
                      (make-spine-half 2 (make-btree-leaf 1)
		        (make-spine-null)))))
(define *btree-4* (spine->btree *spine-4*))
(test-assert (btree-brotherly? *btree-4*))
(test-assert (btree-balanced? *btree-4*))
(test-assert (btree-inorder? *btree-4* <))
(test-assert (btree-correct? *btree-4* <))
(test '(1 2 3 4) (btree->ordered-list *btree-4*))

(test-assert (btree0? (ordered-list->btree '())))
(define *btree1* (ordered-list->btree '(1)))
(test-assert (btree2? *btree1*))
(test 1 (btree2-elt *btree1*))

(define (btree-roundtrip ordered-list)
  (test ordered-list (btree->ordered-list (ordered-list->btree ordered-list))))
(btree-roundtrip '())
(btree-roundtrip (iota 1))
(btree-roundtrip (iota 2))
(btree-roundtrip (iota 3))
(btree-roundtrip (iota 4))
(btree-roundtrip (iota 5))
(btree-roundtrip (iota 100))
(btree-roundtrip (iota 1000))
(btree-roundtrip (iota 10000))

(test 0 (btree-size (make-btree0)))
(test 1 (btree-size (make-btree-leaf 7)))
(test 4 (btree-size *fib-tree-3*))

(test-error (btree-min (make-btree-leaf)))
(test-error (btree-max (make-btree-leaf)))
(test 7 (btree-min *btree-leaf-fixture*))
(test 7 (btree-max *btree-leaf-fixture*))
(test 1 (btree-min *fib-tree-3*))
(test 4 (btree-max *fib-tree-3*))

(test-assert (iset-empty? (make-iset-empty <)))

(test '(1 2 3) (iset->ordered-list (iset/merger < iset-merger-left 1 2 3)))
(test '(1 2 3) (iset->ordered-list (iset < 1 2 3)))

(define *set01* (iset < 0 1))
(define *set12* (iset < 1 2))

(test '(0 2) (iset->ordered-list (iset-xor *set01* *set12*)))
(test '(0) (iset->ordered-list (iset-difference *set01* *set12*)))
(test '(0 1 2) (iset->ordered-list (iset-union *set01* *set12*)))
(test '(1) (iset->ordered-list (iset-intersection *set01* *set12*)))


(test-assert (iset-member? *set01* 0))
(test-assert (iset-member? *set01* 1))
(test-not (iset-member? *set01* 2))
(test-not (iset-member? *set12* 0))
(test-assert (iset-member? *set12* 1))
(test-assert (iset-member? *set12* 2))

(test 0 (iset-min *set01*))
(test 1 (iset-max *set01*))
(test 1 (iset-min *set12*))
(test 2 (iset-max *set12*))

(test '(1 2) (iset->ordered-list (iset-include (iset-include (iset <) 2) 1)))
(iset->ordered-list (iset-include (iset-include (iset <) 2) 1))

(test 'absent (iset-update *set01* -1 (const 'absent) (const 'present)))
(test 'present (iset-update *set01* 0 (const 'absent) (const 'present)))
(test '(0 1 7) (iset->ordered-list (iset-update *set01* 7
						(lambda (insert)
						  (insert))
						(const 'present))))
(test '(0 7) (iset->ordered-list (iset-update *set01* 1
					      (const 'absent)
					      (lambda (match replace remove)
						(replace 7)))))
(test '(0) (iset->ordered-list (iset-update *set01* 1
					    (const 'absent)
					    (lambda (match replace remove)
					      (remove)))))

(test #f (iset-find *set01* 7 (const #f)))
(test 1 (iset-find *set01* 1 (const #f)))

(test '(0 1 7) (iset->ordered-list (iset-include *set01* 7)))
(test '(0 1) (iset->ordered-list (iset-include *set01* 1)))

(test '(0) (iset->ordered-list (iset-exclude *set01* 1)))
(test '(0 1) (iset->ordered-list (iset-exclude *set01* 7)))

(define *iset10* (ordered-list->iset < (iota 10)))

(test '(1 3 5 7 9) (iset->ordered-list (iset-filter odd? *iset10*)))

(test (fold * 1 (iota 10)) (iset-fold * 1 *iset10*))

(test '(1 2 3 4 5 6 7 8 9 10) (iset->ordered-list (iset-map/monotone add1 *iset10*)))

(test '(1 2 3 4 5 6 7 8 9 10) (iset->ordered-list (iset-map add1 *iset10*)))

(define *animals* (ordered-list->iset string<? '("ape" "bat" "cat" "dog")))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(test '("epa" "god" "tab" "tac") (iset->ordered-list (iset-map string-reverse *animals*)))

(test (iota 30) (iset->ordered-list (ordered-list->iset < (iota 30))))

(test '(1 2 3) (iset->ordered-list (list->iset/merger < iset-merger-right '(1 2 2 3 3 3))))

(test '(1 2 3) (iset->ordered-list (list->iset < '(1 2 2 3 3 3))))

(define *iset-empty* (iset <))
(define *iset-2* (iset < 2))
(define *iset-123* (iset < 1 2 3))

(test-assert (iset<? *iset-empty* *iset-2* *iset-123*))
(test-assert (iset<=? *iset-empty* *iset-2* *iset-123*))
(test-not (iset=? *iset-empty* *iset-2* *iset-123*))
(test-not (iset>=? *iset-empty* *iset-2* *iset-123*))
(test-not (iset>? *iset-empty* *iset-2* *iset-123*))

(test-not (iset<? *iset-123* *iset-2* *iset-empty*))
(test-not (iset<=? *iset-123* *iset-2* *iset-empty*))
(test-not (iset=? *iset-123* *iset-2* *iset-empty*))
(test-assert (iset>=? *iset-123* *iset-2* *iset-empty*))
(test-assert (iset>? *iset-123* *iset-2* *iset-empty*))

(test-not (iset<? *iset-123* *iset-123* *iset-123*))
(test-assert (iset<=? *iset-123* *iset-123* *iset-123*))
(test-assert (iset=? *iset-123* *iset-123* *iset-123*))
(test-assert (iset>=? *iset-123* *iset-123* *iset-123*))
(test-not (iset>? *iset-123* *iset-123* *iset-123*))

(define *imap0* (alist->imap < '()))
(define *squares* (alist->imap < '((0 . 0) (1 . 1) (2 . 4) (3 . 9))))

(test-assert (imap? *squares*))
(test-assert (imap-empty? *imap0*))
(test-not (imap-empty? *squares*))
(test 0 (imap-size *imap0*))
(test 4 (imap-size *squares*))

; absent-proc and present-proc
(test 'absent (imap-update *squares* -1 (const 'absent) (const 'present)))
(test 'present (imap-update *squares* 3 (const 'absent) (const 'present)))

; insert
(test 5 (imap-size (imap-update *squares* -1
				(lambda (insert)
				  (insert 1))
				(const 'absent))))

; replace
(test 7 (imap-find (imap-update *squares* 0
				(const *squares*)
				(lambda (match-key match-value replace remove)
				  (replace 7)))
		   0
		   (const 'absent)))

; remove
(test 3 (imap-size (imap-update *squares* 0
				(const *squares*)
				(lambda (match-key match-value replace remove)
				  (remove)))))

(test 0 (imap-find *squares* 0 #f))
(test 1 (imap-find *squares* 1 #f))
(test 4 (imap-find *squares* 2 #f))
(test 9 (imap-find *squares* 3 #f))
(test 'absent (imap-find *squares* -1 (const 'absent)))

(test 16 (imap-find (imap-include *squares* 4 16)
		    4
		    (const 'absent)))
(test 4 (imap-size (imap-include *squares* 1 1)))

(test 3 (imap-size (imap-exclude *squares* 0)))
(test 4 (imap-size (imap-exclude *squares* -1)))

(test-not (imap-member? *imap0* 0))
(test-not (imap-member? *imap0* 1))
(test-not (imap-member? *imap0* 2))
(test-not (imap-member? *imap0* 3))

(test-not (imap-member? *squares* -1))
(test-assert (imap-member? *squares* 0))
(test-assert (imap-member? *squares* 1))
(test-assert (imap-member? *squares* 2))
(test-assert (imap-member? *squares* 3))
(test-not (imap-member? *squares* 4))

(test 0 (imap-min-key *squares*))
(test 3 (imap-max-key *squares*))

(test 0 (imap-min-value *squares*))
(test 9 (imap-max-value *squares*))

(test '((0 . 0) (2 . 4)) (imap->ordered-alist (imap-filter (lambda (key value)
							     (even? key))
							   *squares*)))

(test (+ 0 1 4 9) (imap-fold (lambda (key value sum)
			       (+ value sum))
			     0
			     *squares*))

(test '((0 . 1) (1 . 2) (2 . 5) (3 . 10))
      (imap->ordered-alist (imap-map/monotone (lambda (key value)
						(values key (add1 value)))
					      *squares*)))

(test '((-3 . 9) (-2 . 4) (-1 . 1) (0 . 0))
      (imap->ordered-alist (imap-map (lambda (key value)
				       (values (- key) value))
				     *squares*)))

(test '((0 . 0) (1 . 2) (2 . 8) (3 . 18))
      (imap->ordered-alist (imap-map-values (lambda (key value)
					      (* 2 value))
					    *squares*)))

(test '((0 . 0) (1 . 1) (2 . 4) (3 . 9)) (imap->ordered-alist *squares*))

(test '(0 1 4 9) (imap-values *squares*))

(test-assert (iset=? (iset < 0 1 2 3 )
		     (imap-keys *squares*)))

(test '((1 . 2) (3 . 4)) (imap->ordered-alist (ordered-alist->imap < '((1 . 2) (3 . 4)))))

(test '((1 . 2) (3 . 4)) (imap->ordered-alist (alist->imap < '((1 . 2) (3 . 4)))))

(test-assert (imap=? *squares* *squares*))
(test-assert (imap=? (ordered-alist->imap < '((1 . 2) (3 . 4)))
		     (alist->imap < '((1 . 2) (3 . 4)))))

(test -3 (add1 -4))
(test 0 (add1 -1))
(test 1 (add1 0))
(test 3 (add1 2))

(test -3 (sub1 -2))
(test 0 (sub1 1))
(test 1 (sub1 2))
(test 3 (sub1 4))

(define *pair* (cons 1 2))
(test 2 (cdr *pair*))
(test-assert (eq? *pair* (set-cdr/return! *pair* 3)))
(test 3 (cdr *pair*))

(test 'apple (id 'apple))

(define *const7* (const 7))
(test 7 (*const7*))
(test 7 (*const7* 1))
(test 7 (*const7* 2))
(test 7 (*const7* 3))
(test 7 (*const7* 4))

(test 2 (- 5 3))
(test -2 ((flip -) 5 3))

