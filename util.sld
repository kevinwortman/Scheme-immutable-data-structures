
(define-library (util)
  (import
   (comparators)
   (scheme base)
   (srfi 1)
   (srfi 26)
   (srfi 27)
   (srfi 95))

  (export
   define-syntax-rule
   define-singleton
   flip identity constant-thunk
   add1 sub1
   select-left select-right
   random-permutation
   length-at-least?)

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

    (define (select-left left right) left)
    (define (select-right left right) right)

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

    (define (length-at-least? lst k)
      (cond
       ((<= k 0)    #true)
       ((null? lst) #false)
       (else        (length-at-least? (cdr lst) (sub1 k)))))

    ))
