
(define-library (util)
  (import
   (comparators)
   (scheme base)
   (scheme time)
   (scheme write)
   (srfi 1)
   (srfi 26)
   (srfi 27)
   (srfi 95))

  (export
   define-syntax-rule
   define-singleton
   flip identity constant-thunk
   add1 sub1
   random-permutation
   pseudorandom-permutations
   length-at-least?
   timer
   powerset
   boolean)

  (begin
    (define-syntax define-syntax-rule
      (syntax-rules ()
	((define-syntax-rule (NAME PATTERN ...) BODY)
	 (define-syntax NAME
	   (syntax-rules ()
	     ((NAME PATTERN ...)
	      BODY))))))

    (define-syntax-rule (define-singleton IDENTIFIER PREDICATE)
      (begin
	(define IDENTIFIER (make-list 1)) ;; newly allocated object
	(define PREDICATE (cute eq? IDENTIFIER <>))))

    (define (flip procedure)
      (lambda (left right)
	(procedure right left)))

    (define (identity x)
      x)

    (define (constant-thunk x)
      (lambda ()
	x))

    (define add1 (cute + <> 1))
    (define sub1 (cute - <> 1))

    (define (random-permutation random-source n)
      ;; inside-out Fisher-Yates shuffle
      ;; http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_.22inside-out.22_algorithm
      (let ((rand (random-source-make-integers random-source))
	    (v (make-vector n)))
	(do ((i 0 (add1 i)))
	    ((= i n) (vector->list v))
	  (let ((j (rand (add1 i))))
	    (unless (= j i)
              (vector-set! v i (vector-ref v j)))
	    (vector-set! v j i)))))

    (define (pseudorandom-permutations count length-of-each seed)
      (let ((src (make-random-source)))
	(random-source-pseudo-randomize! src 0 seed)
	(list-tabulate count (lambda (i)
			       (random-permutation src length-of-each)))))

    (define (length-at-least? lst k)
      (cond
       ((<= k 0)    #true)
       ((null? lst) #false)
       (else        (length-at-least? (cdr lst) (sub1 k)))))

  (define-syntax-rule (timer message body ...)
    (let ((start (current-jiffy)))
      (display message)
      (begin body ...)
      (let* ((end (current-jiffy))
	     (elapsed (inexact (/ (- end start)
				  (jiffies-per-second)))))
	(display ": ") (display elapsed) (display " seconds") (newline))))

  (define (powerset n)
    (fold (lambda (i subsets)
	    (append subsets
		    (map (cute cons i <>) subsets)))
	  '(())
	  (iota n)))

  (define (boolean x)
    (if x #t #f))

  ;; Takes a binary procedure and returns a procedure that takes two
  ;; or more arguments, applying the binary operation in
  ;; left associative order.
  (define (left-associative binary-operation)
    (lambda (left right . rest)
      (fold (flip binary-operation) left (cons right rest))))

    ))
