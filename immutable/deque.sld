
(define-library (ideque)
  (import
   (finger-tree)
   (scheme base)
   (srfi 1)
   (srfi 26)
   (util))

  (export
   
   ideque
   ideque?

   ideque-empty?
   ideque-length

   ideque-front ideque-back

   ideque-push-front ideque-push-back
   ideque-pop-front ideque-pop-back

   ideque-drop-front ideque-drop-back

   ideque-take-front ideque-take-back
   ideque-take-front/list ideque-take-back/list
   ideque-take-front/values ideque-take-back/values

   ideque-append

   ideque-filter
   ideque-fold
   ideque-map

   list->ideque
   ideque->list)

  (begin

    (define-record-type <ideque>
      (make-ideque length tree)
      ideque?
      (length ideque-length)
      (tree ideque-tree))

    (define (underflow-check deque k)
      (when (< (ideque-length deque) k)
	    (error "ideque underflow")))

    (define (count-check k)
      (unless (and (integer? k)
		   (not (negative? k)))
	      (error "element count must be a nonnegative integer")))

    (define (finger-tree->ideque tree)
      (make-ideque (finger-tree-length tree) tree))

    (define (list->ideque lst)
      (finger-tree->ideque (list->finger-tree lst)))

    (define (ideque . elements)
      (list->ideque elements))

    (define (ideque-empty? deque)
      (finger-tree-empty? (ideque-tree deque)))

    (define-syntax-rule (define-front+back (FRONT-IDENTIFIER BACK-IDENTIFIER) (PEEK POP PUSH) PROC)
      (begin
	(define FRONT-IDENTIFIER
	  (let ((PEEK finger-tree-front)
		(POP  finger-tree-pop-front)
		(PUSH finger-tree-push-front))
	    PROC))
	(define BACK-IDENTIFIER
	  (let ((PEEK finger-tree-back)
		(POP  finger-tree-pop-back)
		(PUSH finger-tree-push-back))
	    PROC))))

    (define-front+back (ideque-front ideque-back) (peek pop push)
      (lambda (deque)
	(underflow-check deque 1)
	(peek (ideque-tree deque))))

    (define-front+back (ideque-push-front ideque-push-back) (peek pop push)
      (left-associative
       (lambda (deque x)
	 (make-ideque (add1 (ideque-length deque))
		      (push (ideque-tree deque) x)))))

    (define-front+back (ideque-pop-front ideque-pop-back) (peek pop push)
      (lambda (deque)
	(underflow-check deque 1)
	(make-ideque (sub1 (ideque-length deque))
		     (pop (ideque-tree deque)))))
  
    (define-front+back (ideque-drop-front ideque-drop-back) (peek pop push)
      (lambda (deque k)
	(underflow-check deque k)
	(count-check k)
	(make-ideque (- (ideque-length deque) k)
		     (do ((k k (sub1 k))
			  (tree (ideque-tree deque) (pop tree)))
			 ((zero? k) tree)))))

    (define (make-take peek pop finish)
      (lambda (deque k)
	(underflow-check deque k)
	(count-check k)
	(let loop ((k k)
		   (tree (ideque-tree deque))
		   (lst '()))
	  (if (zero? k)
	      (finish lst)
	      (loop (sub1 k)
		    (pop tree)
		    (cons (peek tree) lst))))))

    (define-syntax-rule (define-take (FRONT-IDENTIFIER BACK-IDENTIFIER)
			  FINISH-FRONT
			  FINISH-BACK)
      (begin
	(define FRONT-IDENTIFIER
	  (make-take finger-tree-front finger-tree-pop-front FINISH-FRONT))
	(define BACK-IDENTIFIER
	  (make-take finger-tree-back finger-tree-pop-back FINISH-BACK))))

    (define-take (ideque-take-front/list ideque-take-back/list)
      reverse! identity)

    (define-take (ideque-take-front ideque-take-back)
      (lambda (lst)
	(list->ideque (reverse! lst)))
      list->ideque)

    (define-take (ideque-take-front/values ideque-take-back/values)
      (lambda (lst)
	(apply values (reverse! lst)))
      (cute apply values <>))

    (define ideque-append
      (left-associative
       (lambda (left right)
	 (make-ideque (+ (ideque-length left) (ideque-length right))
		      (finger-tree-append (ideque-tree left) (ideque-tree right))))))

    (define (ideque-filter f deque)
      (let ((tree (finger-tree-filter f (ideque-tree deque))))
	(make-ideque (finger-tree-length tree) tree)))

    (define (ideque-fold f knil deque)
      (finger-tree-fold f knil (ideque-tree deque)))

    (define (ideque-map f deque)
      (make-ideque (ideque-length deque)
		   (finger-tree-map f (ideque-tree deque))))

    (define (ideque->list deque)
      (finger-tree->list (ideque-tree deque)))

    ))
