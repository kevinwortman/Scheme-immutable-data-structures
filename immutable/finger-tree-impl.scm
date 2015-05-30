
;;;; missing from the SRFI 121 reference implementation

(define (list->generator list)
  (let ((list list))
    (lambda ()
      (if (null? list)
	  (eof-object)
	  (let ((obj (car list)))
	    (set! list (cdr list))
	    obj)))))

(define (vector->generator vect)
  (gmap (cute vector-ref vect <>)
	(make-range-generator 0 (vector-length vect))))

(define (reverse-vector->generator vect)
  (gmap (lambda (i)
	  (vector-ref vect (- (vector-length vect) i 1)))
	(make-range-generator 0 (vector-length vect))))

;;;; data types

;;; A finger tree may have three shapes:
;;;   1) empty (zero elements),
;;;   2) single (exactly one element), or
;;;   3) deep (two or more elements).

(define-record-type <empty>
  (make-empty)
  empty?)

;; shared singleton empty object
(define *empty* (make-empty))
(define *empty-promise* (delay *empty*))

;;; A single tree is one element boxed in a record.

(define-record-type <single>
  (make-single x)
  single?
  (x single-x))

;;; A deep tree is comprised of three parts,
;;;   1) a left "digit" of 1-4 elements;
;;;   2) a "spine" holding elements between the digits, described below; and
;;;   3) a right "digit" of 1-4 elements.
;;;
;;; The spine is a promise to a nested finger tree. Each element of
;;; the inner spine tree is a "node" that contains 2-3 elements of the
;;; outer tree and a cached sum of the measurements of the
;;; elements. More on this later.

(define-record-type <deep>
  (make-deep l sp r)
  deep?
  (l deep-l)    ; digit?
  (sp deep-sp)  ; promise for a finger-tree?
  (r deep-r))   ; digit?

;;; We represent a digit by a vector of length 1-4. Our rationale is
;;; that, as compared to lists of length 1-4, vectors are likely to be
;;; faster, incur less garbage collection, and are no less space
;;; efficient than lists.

(define digit vector)
(define list->digit list->vector)
(define digit-length vector-length)
(define digit-ref vector-ref)
(define digit-set! vector-set!)
(define digit->generator vector->generator)

(define (digit-last-index dgt)
  (- (digit-length dgt) 1))

(define (digit-front dgt)
  (digit-ref dgt 0))

(define (digit-back dgt)
  (digit-ref dgt (digit-last-index dgt)))

(define (digit-add-front dgt obj)
  (let ((v (make-vector (+ 1 (vector-length dgt)))))
    (vector-set! v 0 obj)
    (vector-copy! v 1 dgt)
    v))

(define (digit-add-back dgt obj)
  (let* ((k (vector-length dgt))
	 (v (make-vector (+ 1 k))))
    (vector-copy! v 0 dgt)
    (vector-set! v k obj)
    v))

(define digit-drop (cute vector-copy <> <>))

(define digit-take (cute vector-copy <> 0 <>))

(define digit-remove-front (cute digit-drop <> 1))

(define (digit-remove-back dgt)
  (digit-take dgt (digit-last-index dgt)))

(define (digit-single? dgt)
  (= 1 (digit-length dgt)))

(define (digit-nonsingle? dgt)
  (> (digit-length dgt) 1))

(define (digit-full? dgt)
  (= 4 (digit-length dgt)))

(define (digit-nonfull? dgt)
  (< (digit-length dgt) 4))

(define (digit->finger-tree dgt from to)
  (case (- to from)
    ((0)
     *empty*)
    ((1)
     (make-single (digit-ref dgt from)))
    (else
     (let ((from+1 (+ 1 from)))
       (make-deep (vector-copy dgt from from+1)
		  *empty-promise*
		  (vector-copy dgt from+1 to))))))

(define (full-digit-values dgt)
  (values (vector-ref dgt 0)
	  (vector-ref dgt 1)
	  (vector-ref dgt 2)
	  (vector-ref dgt 3)))

;;; A node has two shapes:
;;;  1) a 2-node of a measurement and two elements; or
;;;  2) a 3-node of a measurement and three elements.
;;;
;;; We represent a node by a vector of length 3 or 4,
;;; respectively. Two distinct record types would be cleaner, but also
;;; slower since we wouldn't be able to manipulate a node until after
;;; querying its type and branching.

(define (node2? node)
  (= 3 (vector-length node)))

(define node-m (cute vector-ref <> 0))
(define node-x (cute vector-ref <> 1))
(define node-y (cute vector-ref <> 2))
(define node-z (cute vector-ref <> 3)) ; node-z must not be called on a 2-node

;; Construct a node, computing its cached measurement from its
;; elements.

(define (make-node2 madd mget x y)
  (vector (madd (mget x) (mget y))
	  x y))
(define (make-node3 madd mget x y z)
  (vector (madd (madd (mget x) (mget y))
		(mget z))
	  x y z))

(define node->digit (cute vector-copy <> 1))

;; Syntax to match a nonempty tree based on its shape.
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

;; Match a tree based on its shape.
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

;;; Special case to build a 2-element tree.
(define (make-double e0 e1)
  (make-deep (digit e0) *empty-promise* (digit e1)))

;; Call (finger-tree) to create an empty tree, or (finger-tree madd
;; mget element ...) to create a tree with elements.
(define finger-tree
  (case-lambda
   (()
    *empty*)
   ((madd mget)
    *empty*)
   ((madd mget e0)
    (make-single e0))
   ((madd mget e0 e1)
    (make-double e0 e1))
   ((madd mget . elements)
    (list->finger-tree madd mget elements))))

(define (finger-tree? x)
  (or (deep? x) ; assume deep trees are most common
      (empty? x)
      (single? x)))

(define finger-tree-empty? empty?)

(define (finger-tree-length tree)
  (generator-fold (lambda (obj length)
		    (+ 1 length))
		  0
		  (finger-tree->generator tree)))

(define (finger-tree-front tree)
  (match-nonempty-tree tree
    ((single x)
     x) ; easy
    ((deep l sp r)
     (digit-front l)))) ; easy

(define (finger-tree-back tree)
  ;; symmetric
  (match-nonempty-tree tree
    ((single x)
     x)
    ((deep l sp r)
     (digit-back r))))

(define (finger-tree-add-front madd mget tree obj)
  (let recurse ((mget mget)
		(tree tree)
		(obj obj))
    (match-tree tree
     ((empty) ; empty becomes single
      (make-single obj))
     ((single x) ; single becomes deep
      (make-double obj x))
     ((deep l sp r) ; deep gets deeper
      (if (digit-nonfull? l)
	  ;; May safely add another element to the left digit.
	  (make-deep (digit-add-front l obj) sp r)
	  
	  ;; Can't do that, so pull 3 elements out of the left
	  ;; digit, turn them into a node, and push them into the
	  ;; spine; that leaves the fourth digit element, and the
	  ;; new one, in the left digit.
	  (receive (a b c d) (full-digit-values l)
	    (make-deep (digit obj a)
		       (delay
			 (recurse node-m
				  (force sp)
				  (make-node3 madd mget b c d)))
		       r)))))))

(define (finger-tree-add-back madd mget tree obj)
  ;; symmetric
  (let recurse ((mget mget)
		(tree tree)
		(obj obj))
    (match-tree tree
      ((empty)
       (make-single obj))
      ((single x)
       (make-double x obj))
      ((deep l sp r)
       (if (digit-nonfull? r)
	   (make-deep l sp (digit-add-back r obj))
	   (receive (a b c d) (full-digit-values r)
	     (make-deep l
			(delay
			  (recurse node-m
				   (force sp)
				   (make-node3 madd mget a b c)))
			(digit d obj))))))))

;; Repair a deep node that's missing its left digit.
(define (deep-replenish-left s r)
  (cond
   ((not (empty? s))
    ;; Spine is nonempty so we can simply pull out a node and convert
    ;; it to a digit.
    (make-deep (node->digit (finger-tree-front s))
	       (delay (finger-tree-remove-front s))
	       r))
   ((digit-nonsingle? r)
    ;; Spine is empty, so no luck there, but the right digit has
    ;; multiple elements so we can safely move one over.
    (make-deep (digit (digit-front r))
	       *empty-promise*
	       (digit-remove-front r)))
   (else
    ;; There's only one element left in the entire tree, so we are
    ;; demoted to a single.
    (make-single (digit-front r)))))

(define (deep-replenish-right l s)
  ;; symmetric
  (cond
   ((not (empty? s))
    (make-deep l
	       (delay (finger-tree-remove-back s))
	       (node->digit (finger-tree-back s))))
   ((digit-nonsingle? l)
    (make-deep (digit-remove-back l)
	       *empty-promise*
	       (digit (digit-back l))))
   (else
    (make-single (digit-front l)))))

(define (deep-drop-left l sp r k)
  (if (< k (digit-length l))
      (make-deep (digit-drop l k) sp r)
      (deep-replenish-left (force sp) r)))

(define (deep-drop-right l sp r k)
  (let ((n (digit-length r)))
    (if (< k n)
	(make-deep l sp (digit-take r (- n k)))
	(deep-replenish-right l (force sp)))))

(define (finger-tree-remove-front tree)
  (let recurse ((tree tree))
    (match-nonempty-tree tree
      ((single x)
       *empty*)
      ((deep l sp r)
       (deep-drop-left l sp r 1)))))

(define (finger-tree-remove-back tree)
  (let recurse ((tree tree))
    (match-nonempty-tree tree
      ((single x)
       *empty*)
      ((deep l sp r)
       (deep-drop-right l sp r 1)))))

(define (generator->finger-tree madd mget gen)
  ;; We *could* use generator-fold and finger-tree-add-back, but that
  ;; would copy and garbage collect a right digit object at each
  ;; iteration, which would be slow. Instead we will gather triples of
  ;; elements and add them to the spine one 3-node at a time.
  (let ((front (gen)))
    (if (eof-object? front)
	;; zero elements
	*empty*
	(let ((e (gen)))
	  (if (eof-object? e)
	      ;; one element
	      (make-single front)

	      ;; 2+ elements, build a spine 3 elements at a time
	      (let ((buffer (digit e #f #f))) ; holds 1-3 elements
		(let loop ((k 1) ; number of elements in buffer
			   (s *empty*)) ; spine
		  (let ((e (gen)))
		    (cond
		     ((eof-object? e) ; done
		      (make-deep (digit front)
				 (delay s)
				 (digit-take buffer k)))
		     ((< k 3) ; buffer not full yet
		      (digit-set! buffer k e)
		      (loop (+ 1 k) s))
		     (else ; push 3 elements into the spine, leaving e in buffer
		      (let* ((node (make-node3 madd
					       mget
					       (digit-ref buffer 0)
					       (digit-ref buffer 1)
					       (digit-ref buffer 2)))
			     (s (finger-tree-add-back madd node-m s node)))
			(digit-set! buffer 0 e)
			(loop 1 s))))))))))))

(define (finger-tree->generator tree)
  (make-for-each-generator
   (lambda (proc tree) ; finger-tree-for-each
     (let recurse ((proc proc) (tree tree))
       (match-tree tree
         ((empty)
	  (begin))
	 ((single x)
	  (proc x))
	 ((deep l sp r)
	  (begin
	    (generator-for-each proc (vector->generator l))
	    (recurse (lambda (node)
		       (proc (node-x node))
		       (proc (node-y node))
		       (unless (node2? node)
			       (proc (node-z node))))
		     (force sp))
	    (generator-for-each proc (vector->generator r)))))))
   tree))

(define (reverse-finger-tree->generator tree)
  (make-for-each-generator
   (lambda (proc tree) ; finger-tree-for-each
     (let recurse ((proc proc) (tree tree))
       (match-tree tree
         ((empty)
	  (begin))
	 ((single x)
	  (proc x))
	 ((deep l sp r)
	  (begin
	    (generator-for-each proc (reverse-vector->generator r))
	    (recurse (lambda (node)
		       (unless (node2? node)
			       (proc (node-z node)))
		       (proc (node-y node))
		       (proc (node-x node)))
		     (force sp))
	    (generator-for-each proc (reverse-vector->generator l)))))))
   tree))

(define (list->finger-tree madd mget list)
  (generator->finger-tree madd mget (list->generator list)))

(define (finger-tree->list tree)
  (generator->list (finger-tree->generator tree)))

(define (append-binary madd mget left right)
  ;; This one is nontrivial, but makes sense if we proceed one step at
  ;; a time.
  (let recurse ((mget mget)
		(left left)
		(right right))
    (cond
     ;; If either operand is empty, we don't need to do anything.
     ((empty? left)
      right)
     ((empty? right)
      left)

     ;; If one operand is single, it suffices to add it to the front
     ;; or back of the other.
     ((single? left)
      (finger-tree-add-front madd mget right (single-x left)))
     ((single? right)
      (finger-tree-add-back madd mget left (single-x right)))

     (else
      ;; Both operands are deep, so we have four digits and two spines
      ;; that need to merge into two digits and one spine. Presently
      ;; we have:
      ;;
      ;; left-l left-sp left-r      right-l right-sp right-r
      ;;
      ;; We will:
      ;;  1) Reuse left-l as the left digit.
      ;;  2) Reuse right-r as the right digit.
      ;;  3) Append left-r (1-4 elements) and right-l (1-4 elements)
      ;;     into a sequence of 2-8 elements; break them into nodes;
      ;;     and push the nodes into left-sp.
      ;;  4) All that's left is left-sp and right-sp; they are finger
      ;;     trees, so we can append them recursively.
      ;;
      ;; The algorithm for (3) is greedy: if exactly 2 or 4 elements
      ;; remain, build a 2-node; otherwise build a 3-node. Observe
      ;; that, for any number of elements 2 through 8, this heuristic
      ;; splits the elements into 2-nodes and 3-nodes with nothing
      ;; left over.
      (make-deep (deep-l left)
		 (delay
		   (let ((loose (gappend (vector->generator (deep-r left))
					 (vector->generator (deep-l right)))))
		     (let loop ((loose-count (+ (digit-length (deep-r left))
						(digit-length (deep-l right))))
				(s (force (deep-sp left))))
		       (case loose-count
			 ((0)
			  (recurse node-m s (force (deep-sp right))))
			 ((2 4)
			  ;; use let* to guarantee x is bound before y
			  (let* ((x (loose))
				 (y (loose)))
			    (loop (- loose-count 2)
				  (finger-tree-add-back madd node-m s
							(make-node2 madd mget x y)))))
			 (else
			  (let* ((x (loose))
				 (y (loose))
				 (z (loose)))
			    (loop (- loose-count 3)
				  (finger-tree-add-back madd node-m s
							(make-node3 madd mget x y z)))))))))
		 (deep-r right))))))

(define finger-tree-append
  (case-lambda
   ((madd mget left right)
    (append-binary madd mget left right))
   ((madd mget first . rest)
    (fold (lambda (right left)
	    (append-binary left right))
	  first
	  rest))))

(define (finger-tree-scan madd mget mpred mseed tree match absent)
  (let recurse ((mget mget)
		(m-prefix mseed)
		(tree tree)
		(match (lambda (m-prefix e)
			 (match e)))
		(absent (lambda (m-after)
			  (absent))))
    (let ((measure (lambda (m-before e)
		     (madd m-before (mget e)))))
      (match-tree tree
        ((empty)
	 (absent m-prefix))
	((single x)
	 (let ((m-x (measure m-prefix x)))
	   (if (mpred m-x)
	       (match m-prefix x)
	       (absent m-x))))
	((deep l sp r)
	 (let lloop ((i 0) (m-prefix m-prefix))
	   (if (< i (digit-length l))
	       (let* ((e (digit-ref l i))
		      (m-e (measure m-prefix e)))
		 (if (mpred m-e)
		     (match m-prefix e)
		     (lloop (+ 1 i) m-e)))
	       (recurse node-m
			m-prefix
			(force sp)
			(lambda (m-prefix node)
			  (let* ((x (node-x node))
				 (m-x (measure m-prefix x))
				 (y (node-y node))
				 (m-y (measure m-x y)))
			    (cond
			     ((mpred m-x)
			      (match m-prefix x))
			     ((or (node2? node)
				  (mpred m-y))
			      (match m-x y))
			     (else
			      (match m-y (node-z node))))))
			(lambda (m-tree)
			  (let rloop ((i 0) (m-prefix m-tree))
			    (if (< i (digit-length r))
				(let* ((e (digit-ref r i))
				       (m-e (measure m-prefix e)))
				  (if (mpred m-e)
				      (match m-prefix e)
				      (rloop (+ 1 i) m-e)))
				(absent m-prefix))))))))))))
			  
(define (finger-tree-scan/context madd mget mpred mseed tree match absent)
  (let recurse ((mget mget)
		(m-prefix mseed)
		(tree tree)
		(match (lambda (prefix m-prefix e suffix)
			 (match prefix e suffix)))
		(absent (lambda (m-after)
			  (absent))))
    (let ((measure (lambda (m-before e)
		     (madd m-before (mget e)))))
      (match-tree tree
        ((empty)
	 (absent m-prefix))
	((single x)
	 (let ((m-x (measure m-prefix x)))
	   (if (mpred m-x)
	       (match *empty* m-prefix x *empty*)
	       (absent m-x))))
	((deep l sp r)
	 (let ((ln (digit-length l)))
	   (let lloop ((i 0) (m-prefix m-prefix))
	     (if (< i ln)
		 (let* ((e (digit-ref l i))
			(m-e (measure m-prefix e))
			(i+1 (+ 1 i)))
		   (if (mpred m-e)
		       (match (digit->finger-tree l 0 i)
			      m-prefix
			      e
			      (deep-drop-left l sp r i+1))
		       (lloop i+1 m-e)))
		 (recurse node-m
			  m-prefix
			  (force sp)
			  (lambda (prefix m-prefix node suffix)
			    (let* ((x (node-x node))
				   (m-x (measure m-prefix x))
				   (y (node-y node))
				   (m-y (measure m-x y)))
			      (cond
			       ((mpred m-x)
				(match (deep-replenish-right l prefix)
				       m-prefix
				       x
				       (make-deep (if (node2? node)
						      (digit y)
						      (digit y (node-z node)))
						  (delay suffix)
						  r)))
			       ((node2? node)
				(match (make-deep l (delay prefix) (digit x))
				       m-prefix
				       y
				       (deep-replenish-left suffix r)))
			       ((mpred m-y)
				(match (make-deep l (delay prefix) (digit x))
				       m-prefix
				       y
				       (make-deep (digit (node-z node)) (delay suffix) r)))
			       (else
				(match (make-deep l (delay prefix) (digit x y))
				       m-y
				       (node-z node)
				       (deep-replenish-left suffix r))))))
			       (lambda (m-after)
				 (let ((rn (digit-length r)))
				   (let rloop ((i 0) (m-prefix m-after))
				     (if (< i rn)
					 (let* ((e (digit-ref r i))
						(m-e (measure m-prefix e))
						(i+1 (+ 1 i)))
					   (if (mpred m-e)
					       (match (deep-drop-right l sp r (- rn i))
						      m-prefix
						      e
						      (digit->finger-tree r i+1 rn))
					       (rloop i+1 m-e)))
					 (absent m-prefix))))))))))))))
