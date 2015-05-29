
(define (vector->generator vect)
  (gmap (cute vector-ref vect <>)
	(make-range-generator 0 (vector-length vect))))

(define (reverse-vector->generator vect)
  (let ((n (vector-length vect)))
    (gmap (lambda (i)
	    (vector-ref vect (- n i 1)))
	  (make-range-generator 0 n))))

(define (reverse-vector-for-each proc vect)
  (generator-for-each proc (reverse-vector->generator vect)))

(define (list->generator list)
  (let ((list list))
    (lambda ()
      (if (null? list)
	  (eof-object)
	  (let ((obj (car list)))
	    (set! list (cdr list))
	    obj)))))

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

(define *empty* (list 'empty-finger-tree))
(define *empty-promise* (delay *empty*))
(define empty? (cute eq? <> *empty*))

(define-record-type <single>
  (make-single x)
  single?
  (x single-x))

(define-record-type <deep>
  (make-deep l sp r)
  deep?
  (l deep-l)
  (sp deep-sp)
  (r deep-r))

(define digit vector)

(define list->digit list->vector)

(define digit-length vector-length)

(define (digit-length-1 dgt)
  (- (digit-length dgt) 1))

(define digit-ref vector-ref)

(define digit->generator vector->generator)

(define reverse-digit->generator reverse-vector->generator)

(define (digit-left dgt)
  (digit-ref dgt 0))

(define (digit-right dgt)
  (digit-ref dgt (digit-length-1 dgt)))

(define (digit-push-left dgt obj)
  (let ((v (make-vector (+ 1 (vector-length dgt)))))
    (vector-set! v 0 obj)
    (vector-copy! v 1 dgt)
    v))

(define (digit-push-right dgt obj)
  (let* ((k (vector-length dgt))
	 (v (make-vector (+ 1 k))))
    (vector-copy! v 0 dgt)
    (vector-set! v k obj)
    v))

(define digit-drop (cute vector-copy <> <>))

(define (digit-take dgt k)
  (vector-copy dgt 0 k))

(define digit-pop-left (cute digit-drop <> 1))

(define (digit-pop-right dgt)
  (digit-take dgt (digit-length-1 dgt)))

(define (digit-single? dgt)
  (= 1 (digit-length dgt)))

(define (digit-nonsingle? dgt)
  (> (digit-length dgt) 1))

(define (digit-nonfull? dgt)
  (< (digit-length dgt) 4))

(define (digit-full? dgt)
  (= 4 (digit-length dgt)))

(define (digit->finger-tree dgt from to)
  (case (- to from)
    ((0)
     *empty*)
    ((1)
     (make-single (digit-ref dgt from)))
    ((2)
     (make2 (digit-ref dgt from)
	    (digit-ref dgt (+ 1 from))))
    ((3)
     (make3 (digit-ref dgt from)
	    (digit-ref dgt (+ 1 from))
	    (digit-ref dgt (+ 2 from))))
    (else
     (make4 (digit-ref dgt from)
	    (digit-ref dgt (+ 1 from))
	    (digit-ref dgt (+ 2 from))
	    (digit-ref dgt (+ 3 from))))))

(define (full-digit-values dgt)
  (values (vector-ref dgt 0)
	  (vector-ref dgt 1)
	  (vector-ref dgt 2)
	  (vector-ref dgt 3)))

(define (digit-for-each proc dgt)
  (generator-for-each proc (digit->generator dgt)))

(define (digit-for-each/reverse proc dgt)
  (generator-for-each proc (reverse-digit->generator dgt)))

(define-record-type <node2>
  (make-node2-raw x y m)
  node2?
  (x node2-x)
  (y node2-y)
  (m node2-m))

(define-record-type <node3>
  (make-node3-raw x y z m)
  node3?
  (x node3-x)
  (y node3-y)
  (z node3-z)
  (m node3-m))

(define (make-node2 madd mget x y)
  (make-node2-raw x y
		  (madd (mget x)
			(mget y))))

(define (make-node3 madd mget x y z)
  (make-node3-raw x y z
		  (madd (mget x)
			(madd (mget y)
			      (mget z)))))

(define-syntax match-node
  (syntax-rules (node2 node3)
    ((match-node NODE
       ((node2 X Y M)
	NODE2-BODY)
       ((node3 X Y Z M)
	NODE3-BODY))
     (if (node2? NODE)
	 (let ((X (node2-x NODE))
	       (Y (node2-y NODE))
	       (M (node2-m NODE)))
	   NODE2-BODY)
	 (let ((X (node3-x NODE))
	       (Y (node3-y NODE))
	       (Z (node3-z NODE))
	       (M (node3-m NODE)))
	   NODE3-BODY)))))

(define (node-m node)
  (match-node node
    ((node2 x y m)
     m)
    ((node3 x y z m)
     m)))

(define (node->digit node)
  (match-node node
    ((node2 x y m)
     (digit x y))
    ((node3 x y z m)
     (digit x y z))))

(define-syntax match-nonempty-tree
  (syntax-rules (single deep)
    ((match-nonempty-tree TREE
       ((single X)
	SINGLE-BODY)
       ((deep L SP R)
	DEEP-BODY))
     (if (single? TREE)
	 (let ((X (single-x TREE)))
	   SINGLE-BODY)
	 (let ((L (deep-l TREE))
	       (SP (deep-sp TREE))
	       (R (deep-r TREE)))
	   DEEP-BODY)))))

(define-syntax match-tree
  (syntax-rules (empty single deep)
    ((match-tree TREE
      ((empty)
       EMPTY-BODY)
      ((single X)
       SINGLE-BODY)
      ((deep L SP R)
       DEEP-BODY))
     (if (empty? TREE)
	 EMPTY-BODY
	 (match-nonempty-tree TREE
	   ((single X)
	    SINGLE-BODY)
	   ((deep L SP R)
	    DEEP-BODY))))))

(define (make2 e0 e1)
  (make-deep (digit e0) *empty-promise* (digit e1)))

(define (make3 e0 e1 e2)
  (make-deep (digit e0 e1) *empty-promise* (digit e2)))

(define (make4 e0 e1 e2 e3)
  (make-deep (digit e0 e1 e2) *empty-promise* (digit e3)))

(define (make5 e0 e1 e2 e3 e4)
  (make-deep (digit e0 e1 e2 e3) *empty-promise* (digit e4)))

(define (make6 e0 e1 e2 e3 e4 e5)
  (make-deep (digit e0 e1 e2 e3) *empty-promise* (digit e4 e5)))

(define (make7 e0 e1 e2 e3 e4 e5 e6)
  (make-deep (digit e0 e1 e2 e3) *empty-promise* (digit e4 e5 e6)))

(define (make8 e0 e1 e2 e3 e4 e5 e6 e7)
  (make-deep (digit e0 e1 e2 e3) *empty-promise* (digit e4 e5 e6 e7)))

(define (finger-tree/empty)
  *empty*)

(define finger-tree
  (case-lambda
   ((madd mget)
    *empty*)
   ((madd mget e0)
    (make-single e0))
   ((madd mget e0 e1)
    (make2 e0 e1))
   ((madd mget e0 e1 e2)
    (make3 e0 e1 e2))
   ((madd mget e0 e1 e2 e3)
    (make4 e0 e1 e2 e3))
   ((madd mget e0 e1 e2 e3 e4)
    (make5 e0 e1 e2 e3 e4))
   ((madd mget e0 e1 e2 e3 e4 e5)
    (make6 e0 e1 e2 e3 e4 e5))
   ((madd mget e0 e1 e2 e3 e4 e5 e6)
    (make7 e0 e1 e2 e3 e4 e5 e6))
   ((madd mget e0 e1 e2 e3 e4 e5 e6 e7)
    (make8 e0 e1 e2 e3 e4 e5 e6 e7))
   ((madd mget . elements)
    (list->finger-tree madd mget elements))))

(define (finger-tree? x)
  (or (deep? x)
      (single? x)
      (empty? x)))

(define finger-tree-empty? empty?)

(define (finger-tree-length tree)
  (generator-fold (lambda (obj length)
		    (+ 1 length))
		  0
		  (finger-tree->generator tree)))

(define (finger-tree-left tree)
  (match-nonempty-tree tree
    ((single x)
     x)
    ((deep l sp r)
     (digit-left l))))

(define (finger-tree-right tree)
  (match-nonempty-tree tree
    ((single x)
     x)
    ((deep l sp r)
     (digit-right r))))

(define (finger-tree-push-left madd mget tree obj)
   (let recurse ((mget mget)
		 (tree tree)
		 (obj obj))
     (match-tree tree
       ((empty)
	(make-single obj))
       ((single x)
	(make2 obj x))
       ((deep l sp r)
	(if (digit-nonfull? l)
	    (make-deep (digit-push-left l obj) sp r)
	    (receive (a b c d) (full-digit-values l)
	      (make-deep (digit obj a)
			 (delay (recurse node-m
					 (force sp)
					 (make-node3 madd mget b c d)))
			 r)))))))

(define (finger-tree-push-right madd mget tree obj)
   (let recurse ((mget mget)
		 (tree tree)
		 (obj obj))
     (match-tree tree
       ((empty)
	(make-single obj))
       ((single x)
	(make2 x obj))
       ((deep l sp r)
	(if (digit-nonfull? r)
	    (make-deep l sp (digit-push-right r obj))
	    (receive (a b c d) (full-digit-values r)
	      (make-deep l
			 (delay (recurse node-m
					 (force sp)
					 (make-node3 madd mget a b c)))
			 (digit d obj))))))))

(define (deep-replenish-left s r)
  (cond
   ((not (empty? s))
    (make-deep (node->digit (finger-tree-left s))
	       (delay (finger-tree-pop-left s))
	       r))
   ((digit-nonsingle? r)
    (make-deep (digit (digit-left r))
	       *empty-promise*
	       (digit-pop-left r)))
   (else
    (make-single (digit-left r)))))

(define (deep-replenish-right l s)
  (cond
   ((not (empty? s))
    (make-deep l
	       (delay (finger-tree-pop-right s))
	       (node->digit (finger-tree-right s))))
   ((digit-nonsingle? l)
    (make-deep (digit-pop-right l)
	       *empty-promise*
	       (digit (digit-right l))))
   (else
    (make-single (digit-left l)))))

(define (finger-tree-pop-left tree)
  (let recurse ((tree tree))
    (match-nonempty-tree tree
      ((single x)
       *empty*)
      ((deep l sp r)
       (if (digit-nonsingle? l)
	   (make-deep (digit-pop-left l) sp r)
	   (deep-replenish-left (force sp) r))))))

(define (finger-tree-pop-right tree)
  (let recurse ((tree tree))
    (match-nonempty-tree tree
      ((single x)
       *empty*)
      ((deep l sp r)
       (if (digit-nonsingle? r)
	   (make-deep l sp (digit-pop-right r))
	   (deep-replenish-right l (force sp)))))))

(define (finger-tree-append madd mget . operands)
  (n-ary (left right operands)
    (let recurse ((mget mget) (left left) (right right))
      (cond
       ;; trivial cases: at most one deep operand
       ((empty? right)
	left)
       ((empty? left)
	right)
       ((single? right)
	(finger-tree-push-right madd mget left (single-x right)))
       ((single? left)
	(finger-tree-push-left madd mget right (single-x left)))
       (else
	;; both operands are deep
	(let* ((left-loose (deep-r left))
	       (right-loose (deep-l right))
	       (loose (gappend (digit->generator left-loose)
			       (digit->generator right-loose))))
	  (let loop ((k (+ (digit-length left-loose)
			   (digit-length right-loose)))
		     (s (force (deep-sp left))))
	    (case k
	      ((0)
	       (make-deep (deep-l left)
			  (delay (recurse node-m
					  s
					  (force (deep-sp right))))
			  (deep-r right)))
	      ((2 4)
	       (let* ((x (loose)) ; use let* to guarantee sequential binding
		      (y (loose)))
		 (loop (- k 2)
		       (finger-tree-push-right madd
					       node-m
					       s
					       (make-node2 madd mget x y)))))
	      (else
	       (let* ((x (loose))
		      (y (loose))
		      (z (loose)))
		 (loop (- k 3)
		       (finger-tree-push-right madd
					       node-m
					       s
					       (make-node3 madd mget x y z)))))))))))))

(define (measured madd mget m-pre obj)
  (values obj (madd m-pre (mget obj))))

(define (finger-tree-scan madd mget mpred mseed tree match absent)
  (let recurse ((mget mget)
		(m-pre mseed)
		(tree tree)
		(match/m (lambda (m-pre e)
			   (match e)))
		(absent/m (lambda (m-suf)
			    (absent))))
    (match-tree tree
      ((empty)
       (absent/m m-pre))
      ((single x)
       (receive (x m-suf) (measured madd mget m-pre x)
         (if (mpred m-suf)
	     (match/m m-pre x)
	     (absent/m m-suf))))
      ((deep l sp r)
       (let ((ln-1 (digit-length-1 l)))
	 (let lloop ((i 0) (m-pre m-pre))
	   (receive (e m-suf) (measured madd mget m-pre (digit-ref l i))
	     (cond
	      ((mpred m-suf)
	       (match/m m-pre e))
	      ((< i ln-1)
	       (lloop (+ 1 i) m-suf))
	      (else
	       (recurse node-m
			m-suf
			(force sp)
			(lambda (m-pre node)
			  (if (node2? node)
			      (receive (x m-x) (measured madd mget m-pre (node2-x node))
			        (if (mpred m-x)
				    (match/m m-pre x)
				    (match/m m-x (node2-y node))))
			      (receive (x m-x) (measured madd mget m-pre (node3-x node))
			        (if (mpred m-x)
				    (match/m m-pre x)
				    (receive (y m-y) (measured madd mget m-x (node3-y node))
				      (if (mpred m-y)
					  (match/m m-x y)
					  (match/m m-y (node3-z node))))))))
			(lambda (m-suf)
			  (let ((rn-1 (digit-length-1 r)))
			    (let rloop ((i 0) (m-pre m-suf))
			      (receive (e m-suf) (measured madd mget m-pre (digit-ref r i))
			        (cond
				 ((mpred m-suf)
				  (match/m m-pre e))
				 ((< i rn-1)
				  (rloop (+ 1 i) m-suf))
				 (else
				  (absent/m m-suf)))))))))))))))))

(define (finger-tree-scan/context madd mget mpred mseed tree match absent)
  (let recurse ((mget mget)
		(m-pre mseed)
		(tree tree)
		(match/m (lambda (pre m-pre e m-suf suf)
			   (match pre e suf)))
		(absent/m (lambda (m-suf)
			    (absent))))
    (match-tree tree
      ((empty)
       (absent/m m-pre))
      ((single x)
       (receive (x m-suf) (measured madd mget m-pre x)
         (if (mpred m-suf)
	     (match/m finger-tree/empty m-pre x m-suf finger-tree/empty)
	     (absent/m m-suf))))
      ((deep l sp r)
       (let* ((ln (digit-length l))
	      (ln-1 (- ln 1)))
	 (let lloop ((i 0) (m-pre m-pre))
	   (receive (e m-suf) (measured madd mget m-pre (digit-ref l i))
	     (cond
	      ((mpred m-suf)
	       (match/m (cute digit->finger-tree l 0 i)
			m-pre
			e
			m-suf
			(lambda ()
			  (if (= i ln-1)
			      (deep-replenish-left (force sp) r)
			      (make-deep (digit-drop l (+ 1 i)) sp r)))))
	      ((< i ln-1)
	       (lloop (+ 1 i) m-suf))
	      (else
	       (let ((s (force sp)))
		 (recurse node-m
			  m-pre
			  s
			  (lambda (pre m-pre node m-suf suf)
			    (if (node2? node)
				(receive (x m-x) (measured madd mget m-pre (node2-x node))
				  (if (mpred m-x)
				      (match/m (cut deep-replenish-right l (pre))
					       m-pre
					       x
					       m-x
					       (cut make-deep
						    (digit (node2-y node))
						    (delay (suf))
						    r))
				      (receive (y m-y) (measured madd mget m-x (node2-y node))
				        (if (mpred m-y)
					    (match/m (cut make-deep
							  l
							  (delay (pre))
							  (digit x))
						     m-x
						     y
						     m-y
						     (cut deep-replenish-left (suf) r))))))
				;; node3
				(receive (x m-x) (measured madd mget m-pre (node3-x node))
				  (if (mpred m-x)
				      (match/m (cut deep-replenish-right l (pre))
					       m-pre
					       x
					       m-x
					       (cut make-deep
						    (digit (node3-y node) (node3-z node))
						    (delay (suf))
						    r))
				      (receive (y m-y) (measured madd mget m-x (node3-y node))
				        (if (mpred m-y)
					    (match/m (cut make-deep
							  l
							  (delay (pre))
							  (digit x))
						     m-x
						     y
						     m-y
						     (cut make-deep
							  (digit (node3-z node))
							  (delay (suf))
							  r))
					    (receive (z m-z) (measured madd mget m-y (node3-z node))
					      (match/m (cut make-deep
							    l
							    (delay (pre))
							    (digit x y))
						       m-y
						       z
						       m-z
						       (cut deep-replenish-left (suf) r)))))))))
			  (lambda (m-suf)
			    (let* ((rn (digit-length r))
				   (rn-1 (- rn 1)))
			      (let rloop ((i 0) (m-pre m-suf))
				(let* ((e (digit-ref r i))
				       (m-suf (madd m-pre (mget e))))
				  (cond
				   ((mpred m-suf)
				    (match/m (lambda ()
					       (if (zero? i)
						   (deep-replenish-right l s)
						   (make-deep l sp (digit-take r i))))
					     m-pre
					     e
					     m-suf
					     (cut digit->finger-tree r (+ 1 i) (digit-length r))))
				   ((< i rn-1)
				    (rloop (+ 1 i) m-suf))
				   (else
				    (absent/m m-suf))))))))))))))))))

(define (generator->finger-tree madd mget gen)
  (let ((l (gen)))
    (if (eof-object? l)
	*empty*
	(let ((e (gen)))
	  (if (eof-object? e)
	      (make-single l)
	      (let ((buf (vector e #f #f)))
		(let loop ((k 1) (s *empty*))
		  (let ((e (gen)))
		    (cond
		     ((eof-object? e)
		      (make-deep (digit l)
				 (delay s)
				 (digit-take buf k)))
		     ((= k 3)
		      (let* ((node (make-node3 madd
					       mget
					       (vector-ref buf 0)
					       (vector-ref buf 1)
					       (vector-ref buf 2)))
			     (s (finger-tree-push-right madd node-m s node)))
			(vector-set! buf 0 e)
			(loop 1 s)))
		     (else
		      (vector-set! buf k e)
		      (loop (+ 1 k) s)))))))))))

(define (finger-tree->generator tree)
  (make-for-each-generator (lambda (proc tree)
			     (let recurse ((proc proc) (tree tree))
			       (match-tree tree
					   ((empty)
					    (begin))
					   ((single x)
					    (proc x))
					   ((deep l sp r)
					    (begin
					      (vector-for-each proc l)
					      (recurse (lambda (node)
							 (match-node node
								     ((node2 x y m)
								      (begin (proc x)
									     (proc y)))
								     ((node3 x y z m)
								      (begin (proc x)
									     (proc y)
									     (proc z)))))
						       (force sp))
					      (vector-for-each proc r))))))
			   tree))

(define (reverse-finger-tree->generator tree)
  (make-for-each-generator (lambda (proc tree)
			     (let recurse ((proc proc) (tree tree))
			       (match-tree tree
					   ((empty)
					    (begin))
					   ((single x)
					    (proc x))
					   ((deep l sp r)
					    (begin
					      (reverse-vector-for-each proc r)
					      (recurse (lambda (node)
							 (match-node node
								     ((node2 x y m)
								      (begin (proc y)
									     (proc x)))
								     ((node3 x y z m)
								      (begin (proc z)
									     (proc y)
									     (proc x)))))
						       (force sp))
					      (reverse-vector-for-each proc l))))))
			   tree))

(define (list->finger-tree madd mget list)
  (generator->finger-tree madd mget (list->generator list)))

(define (finger-tree->list tree)
  (generator->list (finger-tree->generator tree)))

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
