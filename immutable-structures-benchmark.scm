;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; immutable-structures-benchmark.scm
;;;;
;;;; See the file README for general remarks and LICENSE for license
;;;; information.
;;;;
;;;; This is a Scheme program that imports immutable-structures and
;;;; performs some crude benchmarks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
	(scheme time)
	(scheme write)
	(srfi 69)
	(immutable-structures))

(define (time-trial message thunk)
  (let ((start (current-jiffy)))
    (display message)
    (thunk)
    (let* ((end (current-jiffy))
	   (elapsed (inexact (/ (- end start) (jiffies-per-second)))))
      (display " ")
      (display elapsed)
      (display " seconds")
      (newline))))

(define *big-ordered-list* (iota (* 100 1000)))

(define *big-hash-table* (make-hash-table))
(time-trial "hash table mass insert"
	    (lambda ()
	      (for-each (lambda (x)
			  (hash-table-set! *big-hash-table* x #f))
			  *big-ordered-list*)))

(define *big-iset* #f)
(time-trial "iset mass insert"
	    (lambda ()
	      (set! *big-iset*
		    (ordered-list->iset < *big-ordered-list*))))

(define (benchmark first-message
		   first-proc
		   first-base
		   second-message
		   second-proc
		   second-base
		   list)
  (define (trial message proc base)
    (time-trial message
		(lambda ()
		  (fold proc base list))))

  (trial first-message first-proc first-base)
  (trial second-message second-proc second-base))

(benchmark "hash-table-exists?"
	   (lambda (x unused)
	     (hash-table-exists? *big-hash-table* x))
	   #f
	   "iset-member?"
	   (lambda (x unused)
	     (iset-member? *big-iset* x))
	   #f
	   *big-ordered-list*)

#|
(benchmark "hash table insertion"
	   (lambda (x hash)
	     (hash-table-set! hash x #f)
	     hash)
	   (make-hash-table)
	   "iset insertion"
	   (lambda (x set)
	     (iset-include set x))
	   (iset <)
	   *big-ordered-list*)
|#