
(import
 ;(chibi loop)
 (chibi test)
 (comparators)
 (imap)
 (iset)
 (scheme base)
 (scheme write)
 (srfi 1)
 (srfi 26)
 (util))

;; imap
;; imap?
;; imap-key-comparator
;; imap-empty?
;; imap-size
(let ((m (imap)))
  (test-assert (imap? m))
  (test-assert (eq? default-comparator (imap-key-comparator m)))
  (test-assert (imap-empty? m))
  (test 0 (imap-size m)))
(let ((m (imap number-comparator)))
  (test-assert (imap? m))
  (test-assert (eq? number-comparator (imap-key-comparator m)))
  (test-assert (imap-empty? m))
  (test 0 (imap-size m)))
(let ((m (imap '(a . 2) '(b . 3) '(c . 4))))
  (test-assert (imap? m))
  (test-assert (eq? default-comparator (imap-key-comparator m)))
  (test-not (imap-empty? m))
  (test 3 (imap-size m)))
(let ((m (imap symbol-comparator '(a . 2) '(b . 3) '(c . 4))))
  (test-assert (imap? m))
  (test-assert (eq? symbol-comparator (imap-key-comparator m)))
  (test-not (imap-empty? m))
  (test 3 (imap-size m)))

;; imap?
;; positive cases already tested
(test-not (imap? '()))
(test-not (imap? (iset)))

;; imap-member?
(define abc (imap '(a . 2) '(b . 3) '(c . 4)))
(test #t (imap-member? abc 'a))
(test #t (imap-member? abc 'b))
(test #t (imap-member? abc 'c))
(test #f (imap-member? abc 2))
(test #f (imap-member? abc 3))
(test #f (imap-member? abc 4))

;; imap-min-key
;; imap-min-value
;; imap-max-key
;; imap-max-value
(test 'a (imap-min-key abc))
(test 2 (imap-min-value abc))
(test 'c (imap-max-key abc))
(test 4 (imap-max-value abc))

;; imap-ref
(let ((ref (cute imap-ref abc <> (constant-thunk #false))))
  ;; found
  (test 2 (ref 'a))
  (test 3 (ref 'b))
  (test 4 (ref 'c))
  ;; not found
  (test #f (ref 'd))
  (test #f (ref 3))
  ;; absent-proc is actually called
  (test-error (imap-ref abc 'd error)))

;; imap-set
(let ((set (lambda (key value)
	     (imap->ordered-alist (imap-set abc key value)))))
  ;; overwrite
  (test '((a . 2)
	  (b . 100)
	  (c . 4))
	(set 'b 100))
  ;; insert
  (test '((a . 2)
	  (b . 3)
	  (c . 4)
	  (d . 100))
	(set 'd 100)))

;; imap-exclude
(let ((exclude (lambda keys
		 (imap->ordered-alist
		  (apply imap-exclude abc keys)))))
  ;; exclude one key
  (test '((b . 3) (c . 4)) (exclude 'a))
  (test '((a . 2) (c . 4)) (exclude 'b))
  ;; exclude everything
  (test '() (exclude 'a 'b 'c))
  ;; no effect
  (test '((a . 2) (b . 3) (c . 4)) (exclude 'z)))

;; imap-between
(define add1mod4 (ordered-alist->imap
		  '((0 . 1)
		    (1 . 2)
		    (2 . 3)
		    (3 . 0))))
(test '((1 . 2) (2 . 3) (3 . 0))
      (imap->ordered-alist (imap-between add1mod4 1 #t 3 #t)))
(test '((2 . 3) (3 . 0))
      (imap->ordered-alist (imap-between add1mod4 1 #f 3 #t)))
(test '((1 . 2) (2 . 3))
      (imap->ordered-alist (imap-between add1mod4 1 #t 3 #f)))
(test '((2 . 3))
      (imap->ordered-alist (imap-between add1mod4 1 #f 3 #f)))

;; imap-filter
;; even keys
(test '((0 . 1) (2 . 3))
      (imap->ordered-alist (imap-filter (lambda (key value)
					  (even? key))
					add1mod4)))
;; even values
(test '((1 . 2) (3 . 0))
      (imap->ordered-alist (imap-filter (lambda (key value)
					  (even? value))
					add1mod4)))

;; imap-fold
;; sum keys
(test (+ 0 1 2 3)
      (imap-fold (lambda (key value accum)
		   (+ key accum))
		 0
		 add1mod4))
;; sum (key * (value + 1))
(test (+ (* 0 2) (* 1 3) (* 2 4) (* 3 1))
      (imap-fold (lambda (key value accum)
		   (+ accum (* key (add1 value))))
		 0
		 add1mod4))

;; imap-map-keys/nondecreasing
; TODO

;; imap-map-keys
; TODO

;; imap-map-values
; TODO

;; imap-map/nondecreasing
; TODO

;; imap-map
; TODO

;; imap->ordered-alist
(test '() (imap->ordered-alist (imap)))
(test '((a . 2) (b . 3) (c . 4)) (imap->ordered-alist abc))

;; imap-keys
(test-assert (iset-empty? (imap-keys (imap))))
(test '(a b c) (iset->ordered-list (imap-keys abc)))

;; imap-values
(test '() (imap-values (imap)))
(test '(2 3 4) (imap-values abc))

;; ordered-alist->imap
(test '() (imap->ordered-alist (ordered-alist->imap '())))
(let ((alist '((x . a) (y . 2) (z . 3))))
  (test alist (imap->ordered-alist (ordered-alist->imap alist))))

;; alist->imap
(test '() (imap->ordered-alist (alist->imap '())))
(test '((x . a) (y . 2) (z . 3))
      (imap->ordered-alist (alist->imap '((z . 3) (x . a) (y . 2)))))

