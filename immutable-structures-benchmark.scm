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
	(scheme inexact)
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

(define *N* (* 200 1000))
(define *N:string* "200,000")

(define *large-list* (iota *N*))

(define *large-hash* #f)
(time-trial (string-append "O(n): insert " *N:string* " elements into a hash table...")
	    (lambda ()
	      (set! *large-hash* (make-hash-table))
	      (for-each (lambda (x)
			  (hash-table-set! *large-hash* x #f))
			  *large-list*)))

(define *large-iset* #f)
(time-trial (string-append "O(n): build an iset from an ordered list of "
			   *N:string*
			   " elements...")
	    (lambda ()
	      (set! *large-iset* (ordered-list->iset < *large-list*))))

(time-trial (string-append "O(n log n): insert " *N:string* " elements into an iset...")
	    (lambda ()
	      (fold (lambda (x iset)
		      (iset-include iset x))
		    (iset <)
		    *large-list*)))

(time-trial (string-append "O(n): query hash table " *N:string* " times...")
	    (lambda ()
	      (for-each (lambda (x)
			  (hash-table-ref *large-hash* x))
			*large-list*)))

(time-trial (string-append "O(n log n): query iset " *N:string* " times...")
	    (lambda ()
	      (for-each (lambda (x)
			  (iset-find *large-iset* x (lambda () #f)))
			*large-list*)))

(newline)
(display (string-append "Note: (log "
			(number->string *N*)
			" 2) is "
			(number->string (log *N* 2))))
(newline)
(newline)	 
