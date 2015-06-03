
(define (constant obj)
  (lambda args
    obj))

(define (identity obj)
  obj)

(define (left-arg l r)
  l)

(define (right-arg l r)
  r)

(define invalid-state-error (cute error "invalid state"))

(define-syntax n-ary
  (syntax-rules ()
    ((n-ary (LEFT RIGHT OPERANDS)
       BODY)
     (fold (lambda (RIGHT LEFT)
	     BODY)
	   (car OPERANDS)
	   (cdr OPERANDS)))))

(define (finger-tree-subset? madd
			     mget
			     key
			     key-cmp
			     proper-ok
			     equal-ok
			     left
			     right)
  (let loop ((left left) (right right))
    (cond
     ((and (empty? left) (empty? right))
      equal-ok)
     ((empty? left)
      proper-ok)
     ((empty? right)
      #false)
     (else
      (let ((l (finger-tree-left left))
	    (r (finger-tree-left right)))
	(if3 (comparator-compare key-cmp (key l) (key r))
	     (and proper-ok
		  (loop (finger-tree-pop-left left)
			right))
	     (and equal-ok
		  (loop (finger-tree-pop-left left)
			(finger-tree-pop-right right)))
	     #false))))))

(define-record-type <ideque>
  (make-ideque length tree)
  ideque?
  (length ideque-length)
  (tree ideque-tree))

(define ideque-madd +)
(define (ideque-mget obj) 1)
(define ideque-mseed 0)

(define (ideque->generator deque)
  (finger-tree->generator (ideque-tree deque)))

(define (reverse-ideque->generator deque)
  (reverse-finger-tree->generator (ideque-tree deque)))

(define generator->ideque-finger-tree (cute generator->finger-tree ideque-madd ideque-mget <>))

(define (length+generator->ideque length gen)
  (make-ideque length (generator->ideque-finger-tree gen)))

(define (generator->ideque gen)
  (let ((tree (generator->ideque-finger-tree gen)))
    (make-ideque (finger-tree-length tree) tree)))

(define (ideque-check-length deque k)
  (when (> k (ideque-length deque))
	(error "ideque underflow")))

(define ideque-check-nonempty (cute ideque-check-length <> 1))

(define (ideque . elements)
  (list->ideque elements))

(define (ideque-tabulate n proc)
  (length+generator->ideque n (make-tabulation-generator n proc)))

(define (ideque-unfold stop? mapper successor seed)
  (generator->ideque (make-unfold-generator stop? mapper successor seed)))

(define (ideque-unfold-right stop? mapper successor seed)
  (ideque-reverse (ideque-unfold stop? mapper successor seed)))

(define (ideque-empty? deque)
  (zero? (ideque-length deque)))

(define (ideque-front deque)
  (ideque-check-nonempty deque)
  (finger-tree-left (ideque-tree deque)))

(define (ideque-back deque)
  (ideque-check-nonempty deque)
  (finger-tree-right (ideque-tree deque)))

(define (ideque-remove-front deque)
  (ideque-check-nonempty deque)
  (make-ideque (- (ideque-length deque) 1)
	       (finger-tree-pop-left (ideque-tree deque))))

(define (ideque-remove-back deque)
  (ideque-check-nonempty deque)
  (make-ideque (- (ideque-length deque) 1)
	       (finger-tree-pop-right (ideque-tree deque))))

(define (ideque-add-front deque obj)
  (make-ideque (+ 1 (ideque-length deque))
	       (finger-tree-push-left ideque-madd ideque-mget (ideque-tree deque) obj)))

(define (ideque-add-back deque obj)
  (make-ideque (+ 1 (ideque-length deque))
	       (finger-tree-push-right ideque-madd ideque-mget (ideque-tree deque) obj)))

(define (ideque-split-at/thunks deque n)
  (ideque-check-length deque n)
  (finger-tree-scan/context ideque-madd
			    ideque-mget
			    (cute >= <> n)
			    ideque-mseed
			    (ideque-tree deque)
			    (lambda (pre e suf)
			      (values pre
				      (cut finger-tree-push-left ideque-madd ideque-mget (suf) e)))
			    invalid-state-error))

(define (ideque-take deque n)
  (receive (pre suf) (ideque-split-at/thunks deque n)
    (make-ideque n
		 (pre))))

(define (ideque-drop deque n)
  (receive (pre suf) (ideque-split-at/thunks deque n)
    (make-ideque (- (ideque-length deque) n)
		 (suf))))

(define (ideque-take-right deque n)
  (ideque-drop deque (- n (ideque-length deque))))

(define (ideque-drop-right deque n)
  (ideque-take deque (- n (ideque-length deque))))

(define (ideque-split-at deque n)
  (values (ideque-take deque n)
	  (ideque-drop deque n)))

(define (ideque-append . deques)
  (make-ideque (apply + (map ideque-length deques))
	       (apply finger-tree-append ideque-madd ideque-mget deques)))

(define (ideque-concatenate list-of-deques)
  (length+generator->ideque (fold + 0 (map ideque-length list-of-deques))
			    (gconcatenate (map ideque->generator list-of-deques))))

(define (ideque-reverse deque)
  (length+generator->ideque (ideque-length deque)
			    (reverse-ideque->generator deque)))

(define (ideque-count pred deque)
  (generator-count pred (ideque->generator deque)))

;; TODO ideque-zip

(define (ideque-map proc deque)
  (length+generator->ideque (ideque-length deque)
			    (gmap proc (ideque->generator deque))))

(define (ideque-for-each proc deque)
  (generator-for-each proc (ideque->generator deque)))

(define (ideque-fold proc nil deque)
  (generator-fold proc nil (ideque->generator deque)))

(define (ideque-fold-right proc nil deque)
  (generator-fold proc nil (reverse-ideque->generator deque)))

;; TODO ideque-append-map

(define (ideque-filter pred deque)
  (generator->ideque (gfilter pred (ideque->generator deque))))

(define (ideque-remove pred deque)
  (ideque-filter (lambda (obj)
		   (not (pred obj)))
		 deque))

(define (ideque-partition pred deque)
  (values (ideque-filter pred deque)
	  (ideque-remove pred deque)))

;; TODO searching procedures

(define (list->ideque list)
  (generator->ideque (list->generator list)))

(define (ideque->list deque)
  (generator->list (ideque->generator deque)))



#|

(define-record-type <iset>
  (make-iset cmp size tree)
  iset?
  (cmp iset-cmp)
  (size iset-size)
  (tree iset-tree))

(define iset-madd right-arg)

(define iset-mget identity)

(define (check-iset-nonempty st)
  (when (iset-empty? st)
	(error "unexpected empty iset")))

(define iset
  (case-lambda
   ((cmp)
    (make-iset cmp 0 (finger-tree)))
   ((cmp . elements)
    (generator->iset cmp (list->generator elements)))))

(define (iset-empty? st)
  (finger-tree-empty? (iset-tree st)))

(define (iset-include . operands)
  (n-ary (st x operands)
    (let ((cmp (iset-cmp st)))
      (finger-tree-split iset-madd
			 iset-mget
			 (cute >=? cmp <> x)
			 (iset-tree left)
			 (lambda (pre e suf)
			   (if (=? cmp e x)
			       (make-iset cmp
					  (iset-size st)
					  (finger-tree-append iset-madd
							      iset-mget
							      (pre)
							      (single x)
							      (suf)))
			       (make-iset cmp
					  (+ 1 (iset-size st))
					  (finger-tree-append iset-madd
							      iset-mget
							      (pre)
							      (single x)
							      (single e)
							      (suf)))))
			 (lambda ()
			   (make-iset cmp
				      (+ 1 (iset-size st))
				      (finger-tree-push-right iset-madd
							      iset-mget
							      st
							      x)))))))

(define (iset-exclude . operands)
  (nary (st x operands)
    (let ((cmp (iset-cmp st)))
      (finger-tree-split iset-madd
			 iset-mget
			 (cute >=? cmp <> x)
			 (iset-tree st)
			 (lambda (pre e suf)
			   (if (=? cmp e x)
			       (make-iset cmp
					  (- (iset-size st) 1)
					  (finger-tree-append iset-madd
							      iset-mget
							      (pre)
							      (suf)))
			       st))
			 (lambda ()
			   st)))))

(define (iset-min st)
  (check-iset-nonempty st)
  (finger-tree-left (iset-tree st)))

(define (iset-max st)
  (check-iset-nonempty st)
  (finger-tree-right (iset-tree st)))

(define-syntax define-iset-operation
  (syntax-rules ()
    ((define-iset-operation (IDENTIFIER KEEP-UNIQUE-LEFT KEEP-UNIQUE-RIGHT MERGE-COMMON))
     (define (IDENTIFIER . operands)
       (nary (left right operands)
         (finger-tree->iset (iset-cmp left)
			    (finger-tree-merge iset-madd
					       iset-mget
					       identity
					       (iset-cmp left)
					       KEEP-UNIQUE-LEFT
					       KEEP-UNIQUE-RIGHT
					       MERGE-COMMON
					       (iset-tree left)
					       (iset-tree right))))))))

(define-iset-operation (iset-difference #t #f #f))
(define-iset-operation (iset-intersect #f #f left-arg))
(define-iset-operation (iset-union #t #t left-arg))
(define-iset-operation (iset-xor #t #t #f))

(define-syntax define-iset-relation
  (syntax-rules ()
    ((define-iset-relation (IDENTIFIER SIZE-PRED PROPER-SUBSET-OK EQUAL-OK OPERAND-ORDER))
     (define (IDENTIFIER . operands)
       (nary (left right (OPERAND-ORDER operands))
         (and left
	      (SIZE-PRED (iset-size left)
			 (iset-size right))
	      (finger-tree-subset? iset-madd
				   iset-mget
				   identity
				   (iset-cmp left)
				   PROPER-SUBSET-OK
				   EQUAL-OK
				   left
				   right)))))))

(define-iset-relation (iset<?  <  #t #f identity))
(define-iset-relation (iset<=? <= #t #t identity))
(define-iset-relation (iset=?  =  #f #t identity))
(define-iset-relation (iset>=? <= #t #t reverse!))
(define-iset-relation (iset>?  <  #t #f reverse!))
|#
