
(import
 (brother12)
 (chibi test)
 (comparators)
 (scheme base)
 (scheme time)
 (scheme write)
 (srfi 1)
 (srfi 26)
 (srfi 27)
 (srfi 39)
 (util))

;; First we have a battery of fast and simple tests to exercise the
;; basic operation of each procedure. Then we have some slow loops
;; that repeat operations thousands of times in order to cover all the
;; internal states.

(define do-slow-tests (make-parameter #t))

;; random number generator, seeded to a constant so tests are
;; deterministic
(define src (make-random-source))
(define rnd (random-source-make-integers src))
(random-source-pseudo-randomize! src 0)

;; aliases for frequently-used procedures
(define search (cute brother12-search <> number-comparator <> identity (constant #f)))
(define insert (cute brother12-insert <> number-comparator <> select-left))
(define validate (cute brother12-validate <> number-comparator))

(define (make< x)
  (cute < <> x))
(define (make> x)
  (cute > <> x))

;; Make small trees for use in sanity tests. This also tests
;; make-brother12 and brother12-insert .
(define t0 (make-brother12))
(validate t0)
(define t1 (insert t0 7))
(validate t1)
(define t2 (insert t1 2))
(validate t2)
(define t3 (insert t2 1))
(validate t3)
(define t4 (insert t3 5))
(validate t4)
(define t5 (insert t4 3))
(validate t5)
(define t6 (insert t5 6))
(validate t6)
(define t7 (insert t6 4))
(validate t7)

;; make-brother12
(test-assert (procedure? make-brother12))
(test-assert (eq? t0 (make-brother12)))

;; brother12-empty?
(test-assert (brother12-empty? t0))
(test-not (brother12-empty? t1))
(test-not (brother12-empty? t2))
(test-not (brother12-empty? t3))
(test-not (brother12-empty? t4))
(test-not (brother12-empty? t5))
(test-not (brother12-empty? t6))
(test-not (brother12-empty? t7))

;; brother12-size
(test 0 (brother12-size t0))
(test 1 (brother12-size t1))
(test 2 (brother12-size t2))
(test 3 (brother12-size t3))
(test 4 (brother12-size t4))
(test 5 (brother12-size t5))
(test 6 (brother12-size t6))
(test 7 (brother12-size t7))

;; brother12-min
(test 7 (brother12-min t1))
(test 2 (brother12-min t2))
(test 1 (brother12-min t3))
(test 1 (brother12-min t4))
(test 1 (brother12-min t5))
(test 1 (brother12-min t6))
(test 1 (brother12-min t7))

;; brother12-max
(test 7 (brother12-max t1))
(test 7 (brother12-max t2))
(test 7 (brother12-max t3))
(test 7 (brother12-max t4))
(test 7 (brother12-max t5))
(test 7 (brother12-max t6))
(test 7 (brother12-max t7))

;; brother12-search
; match
(test 1 (search t7 1))
(test 2 (search t7 2))
(test 3 (search t7 3))
(test 4 (search t7 4))
(test 5 (search t7 5))
(test 6 (search t7 6))
(test 7 (search t7 7))
; missing
(test-not (search t7 0))
(test-not (search t7 0.5))
(test-not (search t7 1.5))
(test-not (search t7 2.5))
(test-not (search t7 3.5))
(test-not (search t7 4.5))
(test-not (search t7 5.5))
(test-not (search t7 6.5))
(test-not (search t7 7.5))

;; brother12-insert was already tested in the construction of t0
;; through t7

;; brother12-delete
;; TODO

;; iterator
(test-assert (brother12-iterator-empty? (make-brother12-iterator t0)))
(test-not (brother12-iterator-empty? (make-brother12-iterator t1)))
(let ((i (make-brother12-iterator t3)))
  (test-not (brother12-iterator-empty? i))
  (test 1 (brother12-iterator-peek i))

  (set! i (brother12-iterator-pop i))
  (test-not (brother12-iterator-empty? i))
  (test 2 (brother12-iterator-peek i))

  (set! i (brother12-iterator-pop i))
  (test-not (brother12-iterator-empty? i))
  (test 7 (brother12-iterator-peek i))

  (set! i (brother12-iterator-pop i))
  (test-assert (brother12-iterator-empty? i)))

;; build
(test-assert (brother12-empty? (brother12-build-finish (make-brother12-build))))
(define t1000 (brother12-build-finish
	       (fold (flip brother12-build-push)
		     (make-brother12-build)
		     (iota 1000))))
(validate t1000)
(test (iota 1000) (brother12->ordered-list t1000))

;; brother12-fold
(test 101 (brother12-fold + 101 (make-brother12)))
(test (+ 1 2 3 4 5 6 7)
      (brother12-fold + 0 t7))

;; brother12-filter
(test '()
      (brother12->ordered-list (brother12-filter even? (make-brother12))))
(test '(2 4 6) 
      (brother12->ordered-list (brother12-filter even? t7)))

;; brother12-map/monotone
(test '(2 3 4 5 6 7 8)
      (brother12->ordered-list (brother12-map/monotone add1 t7)))

;; brother12-map
(test '(-7 -6 -5 -4 -3 -2 -1)
      (brother12->ordered-list (brother12-map - t7 number-comparator select-left)))

;; brother12->ordered-list
(test '() (brother12->ordered-list t0))
(test '(7) (brother12->ordered-list t1))
(test '(2 7) (brother12->ordered-list t2))
(test '(1 2 7) (brother12->ordered-list t3))
(test '(1 2 5 7) (brother12->ordered-list t4))
(test '(1 2 3 5 7) (brother12->ordered-list t5))
(test '(1 2 3 5 6 7) (brother12->ordered-list t6))
(test '(1 2 3 4 5 6 7) (brother12->ordered-list t7))

;; ordered-list->brother12
(test '(1 2 3 4 5 6 7)
      (brother12->ordered-list
       (ordered-list->brother12 '(1 2 3 4 5 6 7))))
(test '(1 2 3 4 5 6)
      (brother12->ordered-list
       (ordered-list->brother12 '(1 2 3 4 5 6))))

;; list->brother12
(let* ((n 1000)
       (perm (random-permutation src n))
       (root (list->brother12 perm number-comparator select-left)))
  (validate root)
  (test (iota n)
	(brother12->ordered-list root)))

;; brother12-range
(test '(1 2 3 4 5 6 7)
      (brother12->ordered-list
       (brother12-range t7 number-comparator (make< 1) (make> 7))))
(test '(2 3 4 5)
      (brother12->ordered-list
       (brother12-range t7 number-comparator (make< 2) (make> 5))))
(test '(3)
      (brother12->ordered-list
       (brother12-range t7 number-comparator (make< 3) (make> 3))))

;; correctess test
;;
;; thoroughly test all major operations, for a small collection of
;; elements, repeated over many random permutations of input
(when (do-slow-tests)
  (let ((max-n 100)
	(trials 100))
    (do ((t 0 (add1 t)))
	((= t trials))
      (let* ((n (add1 (rnd max-n)))
	     (sorted (iota n))
	     (perm (random-permutation src n)))
	;; brother12-insert
	(let ((root (fold (flip insert) (make-brother12) perm)))
	  (validate root)

	  ;; accessors
	  (test-not (brother12-empty? root))
	  (test n (brother12-size root))
	  (test 0 (brother12-min root))
	  (test (sub1 n) (brother12-max root))

	  ;; search
	  (for-each (lambda (x)
		      (test-assert (search root x))
		      (test-not (search root (+ x 0.5))))
		    sorted)

	  ;; iterator
	  (test sorted
		(let loop ((iter (make-brother12-iterator root))
			   (lst '()))
		  (if (brother12-iterator-empty? iter)
		      lst
		      (loop (brother12-iterator-pop iter)
			    (append lst (list (brother12-iterator-peek iter)))))))

	  ;; fold	
	  (test (fold + 0 perm)
		(brother12-fold + 0 root))

	  ;; filter
	  (let ((filtered (brother12-filter even? root)))
	    (validate filtered)
	    (test (filter even? sorted)
		  (brother12->ordered-list filtered)))

	  ;; map/monotone
	  (let ((mapped (brother12-map/monotone add1 root)))
	    (validate mapped)
	    (test (map add1 sorted)
		  (brother12->ordered-list mapped)))

	  ;; map
	  (let ((mapped (brother12-map - root number-comparator select-left)))
	    (validate mapped)
	    (test (reverse (map - sorted))
		  (brother12->ordered-list mapped)))

	  ;; brother12->ordered-list
	  ;; (this has already been tested several times above)
	  (test sorted (brother12->ordered-list root))

	  ;; ordered-list->brother12
	  (let ((built (ordered-list->brother12 sorted)))
	    (validate built)
	    (test sorted (brother12->ordered-list built)))

	  ;; list->brother12
	  (let ((built (list->brother12 perm number-comparator select-left)))
	    (validate built)
	    (test sorted (brother12->ordered-list built)))

	  ;; range
	  (let* ((i (rnd n))
		 (j (rnd n))
		 (least (min i j))
		 (greatest (max i j))
		 (range (brother12-range root number-comparator (make< least) (make> greatest))))
	    (validate range)
	    (test (filter (cute <= least <> greatest) sorted)
		  (brother12->ordered-list range)))

	  )))))

;; insert/delete stress test
#|
(when (do-slow-tests)
  (let* ((n 1000)
	 (trials 100))
    (let loop ((t 0)
	       (in (filter odd? iota n))
	       (root (ordered-list->brother12 in)))

      (test-assert (list-set=? in (brother12->ordered-list root)))

      (let ((x (rnd n)))
	(if (search root x)
	    ;; delete x
	    (loop (add1 t)
		  (delete x in)
		  (delete root x))

	    ;; insert
	    (loop (add1 t)
		  (cons x in)
		  (insert root x)))))))
|#

;; performance test
;;
;; test only performance-critical operations, for a large collection
;; of elements, repeated only a few times
(define (performance-test title proc)
  (define trials 3)
  (let loop ((i 0) (jiffies 0))
    (if (= i trials)
	(begin (newline)
	       (display (string-append title
				       ": averaged elapsed time of "
				       (number->string trials)
				       " trials = "
				       (number->string (inexact (/ jiffies (* (jiffies-per-second) trials))))))
	       (newline))
	(let ((start (current-jiffy)))
	  (proc)
	  (loop (add1 i) (- (current-jiffy) start))))))

(when (do-slow-tests)
  (let* ((k 200)
	 (n (* k 1000))
	 (n/string (string-append (number->string k) ",000"))
	 (ordered (iota n))
	 (unordered (random-permutation default-random-source n)))

    (performance-test (string-append "ordered-list->brother12 for n=" n/string)
		      (lambda ()
			(ordered-list->brother12 ordered)))

    (performance-test (string-append "list->brother12 for n=" n/string)
		      (lambda ()
			(list->brother12 unordered number-comparator select-left)))

    ))

