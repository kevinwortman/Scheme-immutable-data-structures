;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outline
;;
;; This is a large, complex source file that implements a finger-tree data type
;; as well as a related data type called measure.
;;
;; The file is organized in the following order:
;;
;; 1) measure (exported)
;; 2) finger-tree (non-exported/private)
;; 3) finger-tree (exported)
;; 4) finger-tree-set (exported)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) measure (exported)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <measure>
  (make-measure get-proc add-proc)
  measure?
  (get-proc measure-get-proc)
  (add-proc measure-add-proc))

(define (measure-get meas elt)
  ((measure-get-proc meas) elt))

(define measure-add
  (case-lambda
   ((meas m1 m2)
    ((measure-add-proc meas) m1 m2))
   ((meas m1 m2 m3)
    (measure-add meas m1 (measure-add meas m2 m3)))))

(define (measure-get+add meas m1 elt)
  (measure-add meas m1 (measure-get meas elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) finger-tree (non-exported/private)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A finger tree may have three shapes:
;;;   1) empty (zero elements),
;;;   2) single (exactly one element), or
;;;   3) deep (two or more elements).

(define-record-type <empty>
  (make-empty)
  empty?)

;; shared singleton empty trees
(define *empty* (make-empty))
(define *empty-promise* (delay *empty*))

;;; A single tree is one element boxed in a record.
(define-record-type <single>
  (make-single elt)
  single?
  (elt single-ref))

;;; A deep tree is comprised of three parts,
;;;   1) a left *digit* of 1-4 elements;
;;;   2) a *spine-promise* holding elements between the digits, described below; and
;;;   3) a right *digit* of 1-4 elements.
;;;
;;; The spine is a promise to a nested finger tree. Each element of
;;; the inner spine tree is a *node* that contains 2-3 elements of the
;;; outer tree and a cached sum of the measurements of the
;;; elements. More on this later.
(define-record-type <deep>
  (make-deep left spine-promise right)
  deep?
  (left deep-left)
  (spine-promise deep-spine-promise)
  (right deep-right))

;;; A digit is a vector of length 1-4. Our rationale is that, as
;;; compared to lists of length 1-4, vectors are likely to be faster,
;;; incur less garbage collection, and are no less space efficient
;;; than lists.
(define digit            vector)
(define list->digit      list->vector)
(define digit-length     vector-length)
(define digit-ref        vector-ref)
(define digit-set!       vector-set!)
(define digit->generator vector->generator)

(define (digit-single? dgt)
  (= 1 (digit-length dgt)))

(define (digit-non-single? dgt)
  (not (digit-single? dgt)))

(define (digit-full? dgt)
  (= 4 (digit-length dgt)))

(define (digit-non-full? dgt)
  (not (digit-full? dgt)))

(define (digit-last-index dgt)
  (- (digit-length dgt) 1))

(define (digit-left dgt)
  (digit-ref dgt 0))

(define (digit-right dgt)
  (digit-ref dgt (digit-last-index dgt)))

(define (digit-add-left dgt obj)
  (let ((v (make-vector (+ 1 (vector-length dgt)))))
    (vector-set! v 0 obj)
    (vector-copy! v 1 dgt)
    v))

(define (digit-add-right dgt obj)
  (let* ((k (vector-length dgt))
	 (v (make-vector (+ 1 k))))
    (vector-copy! v 0 dgt)
    (vector-set! v k obj)
    v))

(define (digit-remove-left dgt)
  (vector-copy dgt 1))

(define (digit-remove-right dgt)
  (vector-copy dgt 0 (digit-last-index dgt)))

(define (digit->list dgt)
  (vector->list dgt))

(define (full-digit-values dgt)
  (values (digit-ref dgt 0)
	  (digit-ref dgt 1)
	  (digit-ref dgt 2)
	  (digit-ref dgt 3)))

;;; A node has two shapes:
;;;  1) a *2-node* containing a measurement m and two elements x, y; OR
;;;  2) a *3-node* containing a measurement m and three elements x, y, z.
;;;
;;; m is the sum of the measurements of all elements in the node.
;;;
;;; A node is represented by a vector of length 3 or 4. (Two distinct
;;; record types might be more type safe, but that would be slower
;;; since we couldn't manipulate a node until after querying its type
;;; and branching; and converting a node to a digit would be more
;;; complicated.)

(define (node2 meas x y)
  (vector (measure-add meas
		       (measure-get meas x)
		       (measure-get meas y))
	  x
	  y))

(define (node3 meas x y z)
  (vector (measure-add meas
		       (measure-get meas x)
		       (measure-get meas y)
		       (measure-get meas z))
	  x
	  y
	  z))

(define (node2? node)      (= 3 (vector-length node)))
(define (node-m node)      (vector-ref node 0))
(define (node-x node)      (vector-ref node 1))
(define (node-y node)      (vector-ref node 2))
(define (node-z node)      (vector-ref node 3))
(define (node->digit node) (digit-remove-left node)) ;; just drop m

;;; Helper syntax.

;; Syntax to match a nonempty tree based on its shape.
(define-syntax match-non-empty-tree
  (syntax-rules (single deep)
    ((match-non-empty-tree TREE
       ((single X)
	SINGLE-BODY)
       ((deep L SP R)
	DEEP-BODY))
     (if (single? TREE)
	 (let ((X (single-ref TREE)))
	   SINGLE-BODY)
	 (let ((L (deep-left TREE))
	       (SP (deep-spine-promise TREE))
	       (R (deep-right TREE)))
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
	 (match-non-empty-tree TREE
	   ((single X)
	    SINGLE-BODY)
	   ((deep L SP R)
	    DEEP-BODY))))))

;;; Helper procedures.

;; Construct a finger tree from two elements.
(define (finger-tree-2 x y)
  (make-deep (digit x) *empty-promise* (digit y)))

;; Convert a measure for elements into a measure for spine nodes.  The
;; returned measure uses the same add-proc, and uses node-m as the
;; get-proc.
(define (measure-spine elt-measure)
  (make-measure node-m
		(measure-add-proc elt-measure)))

;; Create a node3 from three elements, and add it to the left side of
;; the spine.
(define (spine-add-3-left meas spine x y z)
  (finger-tree-add-left (measure-spine meas)
			spine
			(node3 meas x y z)))

(define (spine-add-3-right meas spine x y z)
  (finger-tree-add-right (measure-spine meas)
			 spine
			 (node3 meas x y z)))

;; binary-append needs a spine-add-2-right. Note that there is no need
;; for spine-add-2-left.
(define (spine-add-2-right meas spine x y)
  (finger-tree-add-right (measure-spine meas)
			 spine
			 (node2 meas x y)))

;; Repair a deep tree that is missing a digit.
(define (restore-left-digit spine right)
  (cond
   ((finger-tree-non-empty? spine)
    ;; Pull one node out of the spine, and use the node's elements as
    ;; a new digit.
    (make-deep (node->digit (finger-tree-left spine))
	       (delay (finger-tree-remove-left spine))
	       right))
   ((digit-non-single? right)
    ;; Spine is empty, but we can steal an element from the right
    ;; digit.
    (make-deep (digit (digit-left right))
	       *empty-promise*
	       (digit-remove-left right)))
   (else
    ;; Only one element remains, downgrade to a single tree.
    (make-single (digit-left right)))))

(define (restore-right-digit left spine)
  ;; symmetric to restore-left-digit
  (cond
   ((finger-tree-non-empty? spine)
    (make-deep left
	       (delay (finger-tree-remove-right spine))
	       (node->digit (finger-tree-right spine))))
   ((digit-non-single? left)
    (make-deep (digit-remove-right left)
	       *empty-promise*
	       (digit (digit-right left))))
   (else
    (make-single (digit-left left)))))

;; Append two trees into one tree.
(define (append-binary meas left right)
  (cond
   ;; if either tree is empty, there's nothing to do
   ((empty? left ) right)
   ((empty? right) left )
   
   ;; if either tree is single, this reduces to add-left or add-right
   ((single? left )  (finger-tree-add-left  meas right (single-ref left )))
   ((single? right)  (finger-tree-add-right meas left  (single-ref right)))
   
   (else
    ;; Both trees are deep. We need to build a new deep tree. Reading
    ;; left-to-right, we have:
    ;;
    ;;   left-left
    ;;   left-spine
    ;;   left-right
    ;;   right-left
    ;;   right-spine
    ;;   right-right
    (let (
	  ;; left-left becomes the new left digit.
	  (new-left (deep-left left))

	  ;; right-right becomes the new right digit.
	  (new-right (deep-right right))

	  ;; Everything else is lazily smooshed together into a new
	  ;; spine.
	  (new-spine-promise
	   (delay
	     (let* (
		    ;; left-right and right-left form a list of 2-8
		    ;; *loose* elements.
		    (loose (append (digit->list (deep-right left ))
				   (digit->list (deep-left  right))))

		    ;; Merge the loose elements into left-spine
		    ;; (absorb-loose is defined next).
		    (left-spine (apply absorb-loose
				       meas
				       (force (deep-spine-promise left))
				       loose)))
	       ;; Recursively append the resulting left spine with right-spine.
	       (append-binary (measure-spine meas)
			      left-spine
			      (force (deep-spine-promise right)))))))
      
      ;; That accounts for everything!
      (make-deep new-left new-spine-promise new-right)))))

;; (absorb-loose meas spine [elt ...])
;; Partition elt... into nodes and add the nodes to the right side of spine.
;; The number of elements must be in the range [2, 8].
;; meas should be a measure for elements (not spine nodes).
(define absorb-loose
  (case-lambda
   ;; 2 elements, add a node2
   ((meas spine a b)
    (spine-add-2-right meas spine a b))
   ;; 3 elements, add a node3
   ((meas spine a b c)
    (spine-add-3-right meas spine a b c))
   ;; 4 elements, add (2) node2
   ((meas spine a b c d)
    (spine-add-2-right meas
		       (spine-add-2-right meas spine a b)
		       c d))
   ;; 5-8 elements: add a node3 and recurse to handle the rest.
   ((meas spine a b c . rest)
    (apply absorb-loose
	   meas
	   (spine-add-3-right meas spine a b c)
	   rest))))

;; Scan (search) a digit. See finger-tree-scan below.
(define (scan-digit meas mpred mzero dgt match absent)
  (let loop ((m mzero) ; accumulated measurement
	     (i 0)) ; digit index
    (let* ((x (digit-ref dgt i)) ; current element
	   (m/x (measure-get+add meas m x))) ; measurement after x
      (cond
       ((mpred m/x) ; success!
	(match m x))
       ((= i (digit-last-index dgt)) ; end of digit, failure
	(absent m))
       (else ; keep looking
	(loop m/x (+ i 1)))))))

;; Scan (search) a node. See finger-tree-scan below.
;;
;; node must be a node that certainly contains a matching
;; element. This procedure decides which of the 2-3 elements that is.
(define (scan-node meas mpred mzero node match)
  ;; add x's measurement
  (let* ((x (node-x node))
	 (m/x (measure-get+add meas mzero x)))
    (if (mpred m/x)
	(match mzero x) ; x matches
	(let ((y (node-y node)))
	  (if (node2? node)
	      (match m/x y) ; by process of elimination y must match
	      (let ((m/y (measure-get+add meas m/x y)))
		(if (mpred m/y)
		    (match m/x y) ; y matches
		    (match m/y (node-z node))))))))) ; z must match

;; Bisect (split) a digit. See finger-tree-bisect below. In the case
;; of match, the prefix and suffix are digit/vector objects.
(define (bisect-digit meas mpred mzero dgt match absent)
  (let loop ((m mzero)
	     (i 0))
    (let* ((x (digit-ref dgt i))
	   (m/x (measure-get+add meas m x)))
      (cond
       ((mpred m/x)
	(match m
	       (vector-copy dgt 0 i)
	       (vector-copy dgt i)))
       ((= i (digit-last-index dgt))
	(absent m))
       (else
	(loop m/x (+ i 1)))))))

;; Bisect (split) a node. See finger-tree-bisect below.
(define (bisect-node meas mpred mzero
		     left right
		     spine-prefix spine-suffix
		     match)
  ;; separate out the node containing the matching element
  (let* ((node (finger-tree-left spine-suffix))
	 (spine-suffix (finger-tree-remove-left spine-suffix))
	 (x (node-x node))
	 (y (node-y node))
	 (m/x (measure-get+add meas mzero x)))
    (cond
     ((mpred m/x) ;; match on x, build prefix and suffix trees
      (match mzero
	     (restore-right-digit left spine-prefix)
	     (make-deep (node->digit node) (delay spine-suffix) right)))
     ((node2? node) ;; match on y
      (match m/x
	     (make-deep left (delay spine-prefix) (digit x))
	     (make-deep (digit y) (delay spine-suffix) right)))
     (else
      (let ((m/y (measure-get+add meas m/x y))
	    (z (node-z node)))
	(if (mpred m/y)
	    ;; match on y
	    (match m/x
		   (make-deep left (delay spine-prefix) (digit x))
		   (make-deep (digit y z) (delay spine-suffix) right))
	    ;; match on z
	    (match m/y
		   (make-deep left (delay spine-prefix) (digit x y))
		   (make-deep (digit z) (delay spine-suffix) right))))))))

(define *set-mzero* #f) ; unused

(define (make-set-pred order key)
  (lambda (m)
    (>=? (set-order-comparator order)
	 m
	 key)))

;; Merge the elements of left and right in non-decreasing order. This
;; is the foundation of all the set relation and set operation
;; procedures. It supports short-circuiting (stopping the merge as
;; soon as the outcome is certain).
;;
;; This works like a fold, where
;;  - order is a set order object
;;  - zero is the initial accumulated value
;;  - left and right are tree sets to merge
;;  - tie, left-first, right-first, left-remains, right-remains, and
;;    both-empty are procedures that control the fold
;;
;; At each step in the merge, we call an appropriate procedure
;; depending on the state of the merging process.
;;
;; The procedures are defined below. In each case,
;;   - accum is the accumulated value
;;   - l is an element from left
;;   - r is an element from right
;;   - continue is a procedure; call (continue new-accum) to proceed
;;     with the rest of the merge, using the given new accumulated
;;     value.
;; The procedures, and situations when they are called, are:
;;   - (tie accum l r continue): When left and right contain two
;;     elements l and r that are =?.
;;   - (left-first accum l continue): When left contains an element l
;;      that is absent from r.
;;   - (right-first accum r continue): When right contains an element r
;;      that is absent from l.
;;   - (left-remains accum lgen): When all elements of right have been
;;     merged. lgen is a generator for the remaining elements.
;;   - (right-remains accum rgen): When all elements of left have been
;;     merged. rgen is a generator for the remaining elements.
;;   - (both-empty accum): When both trees are empty.
(define (set-merge order
		   tie
		   left-first
		   right-first
		   left-remains
		   right-remains
		   both-empty
		   zero
		   left
		   right)
  (let ((lgen (finger-tree->generator left))
	(rgen (finger-tree->generator right)))
    (let loop ((accum zero)
	       (l (lgen))
	       (r (rgen)))
      (cond
       ((eof-object? l)
	(if (eof-object? r)
	    (both-empty accum)
	    (right-remains accum (gcons* r rgen))))
       ((eof-object? r)
	(left-remains accum (gcons* l lgen)))
       (else
	(comparator-if<=>
	 (set-order-comparator order)
	 (set-order-key order l)
	 (set-order-key order r)
	 (left-first accum l (lambda (new-accum) (loop new-accum (lgen) r)))
	 (tie accum l r (lambda (new-accum) (loop new-accum (lgen) (rgen))))
	 (right-first accum r (lambda (new-accum) (loop new-accum l (rgen))))))))))
  
(define (set<=?-binary order left right)
  (let ((*unused* #f))
    (set-merge order
	       (lambda (accum l r continue) (continue *unused*))
	       (lambda (accum l continue)   #f)
	       (lambda (accum r continue)   (continue *unused*))
	       (lambda (accum lgen)         #f)
	       (lambda (accum rgen)         #t)
	       (lambda (accum)              #t)
	       *unused*
	       left
	       right)))

(define (set<?-binary order left right)
  ;; accum is #f as long as left and right seem to be equal, and
  ;; becomes #t once we see a unique element in right that makes this
  ;; a strict subset.
  (set-merge order
	     (lambda (accum l r continue) (continue accum))
	     (lambda (accum l continue)   #f)
	     (lambda (accum r continue)   (continue #t))
	     (lambda (accum lgen)         #f)
	     (lambda (accum rgen)         #t)
	     (lambda (accum)              accum)
	     #f
	     left
	     right))

(define (set=?-binary order left right)
  (let ((*unused* #f))
    (set-merge order
	       (lambda (accum l r continue) (continue *unused*))
	       (lambda (accum l continue)   #f)
	       (lambda (accum r continue)   #f)
	       (lambda (accum lgen)         #f)
	       (lambda (accum rgen)         #f)
	       (lambda (accum)              #t)
	       *unused*
	       left
	       right)))

(define (binary-relation->variadic proc)
  ;; Note: this implements short-circuiting, meaning that we stop
  ;; comparing sets as soon as a false relation is encountered.
  (lambda (order set1 set2 . rest)
    (and (proc order set1 set2)
	 (let loop ((left set2)
		    (rest rest))
	   (or (null? rest)
	       (let-values (((right rest) (car+cdr rest)))
		 (and (proc order left right)
		      (loop right rest))))))))

(define (set-difference-binary order left right)
  (let ((meas (set-order-measure order)))
    (set-merge order
	       (lambda (accum l r continue) (continue accum))
	       (lambda (accum l continue)
		 (continue (finger-tree-add-right meas accum l)))
	       (lambda (accum r continue)   (continue accum))
	       (lambda (accum lgen)
		 (append-binary meas
				accum
				(increasing-generator->finger-tree-set order lgen)))
	       (lambda (accum rgen)         accum)
	       (lambda (accum)              accum)
	       *empty*
	       left
	       right)))

(define (set-intersect-binary order reduce left right)
  (let ((meas (set-order-measure order)))
    (set-merge order
	       (lambda (accum l r continue)
		 (continue (finger-tree-add-right meas accum (reduce l r))))
	       (lambda (accum l continue)   (continue accum))
	       (lambda (accum r continue)   (continue accum))
	       (lambda (accum lgen)         accum)
	       (lambda (accum rgen)         accum)
	       (lambda (accum)              accum)
	       *empty*
	       left
	       right)))

(define (set-union-binary order reduce left right)
  (let ((meas (set-order-measure order)))
    (set-merge order
	       (lambda (accum l r continue)
		 (continue (finger-tree-add-right meas accum (reduce l r))))
	       (lambda (accum l continue)
		 (continue (finger-tree-add-right meas accum l)))
	       (lambda (accum r continue)
		 (continue (finger-tree-add-right meas accum r)))
	       (lambda (accum lgen)
		 (append-binary meas
				accum
				(increasing-generator->finger-tree-set order lgen)))
	       (lambda (accum rgen)
		 (append-binary meas
				accum
				(increasing-generator->finger-tree-set order rgen)))
	       (lambda (accum)
		 accum)
	       *empty*
	       left
	       right)))

(define (set-xor-binary order left right)
  (let ((meas (set-order-measure order)))
    (set-merge order
	       (lambda (accum l r continue)
		 (continue accum))
	       (lambda (accum l continue)
		 (continue (finger-tree-add-right meas accum l)))
	       (lambda (accum r continue)
		 (continue (finger-tree-add-right meas accum r)))
	       (lambda (accum lgen)
		 (append-binary meas
				accum
				(increasing-generator->finger-tree-set order lgen)))
	       (lambda (accum rgen)
		 (append-binary meas
				accum
				(increasing-generator->finger-tree-set order rgen)))
	       (lambda (accum)
		 accum)
	       *empty*
	       left
	       right)))

(define (binary-operation->variadic proc)
  (lambda (order set1 . sets)
    (fold (lambda (right accum)
	    (proc order accum right))
	  set1
	  sets)))

(define (binary-operation->variadic/reduce proc)
  (lambda (order reduce set1 . sets)
    (fold (lambda (right accum)
	    (proc order reduce accum right))
	  set1
	  sets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3) finger-tree (exported)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-finger-tree)
;; Construct an empty tree.
(define (make-finger-tree)
  *empty*)

;; (finger-tree meas elt ...)
;; Construct a finger tree containing elt ...
(define (finger-tree meas . elts)
  (list->finger-tree meas elts))

(define (finger-tree? x)
  (or (deep? x) ; assume deep trees are most common
      (empty? x)
      (single? x)))

(define finger-tree-empty? empty?)

(define (finger-tree-non-empty? tree)
  (not (finger-tree-empty? tree)))

(define (finger-tree-length tree)
  (generator-fold (lambda (obj length)
		    (+ 1 length))
		  0
		  (finger-tree->generator tree)))

(define (finger-tree-left tree)
  (match-non-empty-tree tree
    ((single x)     x)
    ((deep l sp r)  (digit-left l))))

(define (finger-tree-right tree)
  (match-non-empty-tree tree
    ((single x)     x)
    ((deep l sp r)  (digit-right r))))

(define (finger-tree-add-left meas tree elt)
  (match-tree tree
    ((empty) ; empty -> single
     (make-single elt))
    ((single old-elt) ; single -> deep
     (finger-tree-2 elt old-elt))
    ((deep left spine-promise right)
     (if (digit-non-full? left)
	 ;; Add one more elment to the left finger.
	 (make-deep (digit-add-left left elt)
		    spine-promise
		    right)
	 ;; Can't do that. Pull the 3 rightmost elements from the left
	 ;; digit and push them into the spine as a node. The
	 ;; remaining digit element, and the passed-in element, form
	 ;; a new left finger.
	 (let-values (((a b c d) (full-digit-values left)))
	   (make-deep (digit elt a)
		      (delay (spine-add-3-left meas (force spine-promise) b c d))
		      right))))))

(define (finger-tree-add-right meas tree elt)
  ;; symmetric to finger-tree-add-left
  (match-tree tree
    ((empty)
     (make-single elt))
    ((single old-elt)
     (finger-tree-2 old-elt elt))
    ((deep left spine-promise right)
     (if (digit-non-full? right)
	 (make-deep left
		    spine-promise
		    (digit-add-right right elt))
	 (let-values (((a b c d) (full-digit-values right)))
	   (make-deep left
		      (delay (spine-add-3-right meas (force spine-promise) a b c))
		      (digit d elt)))))))

(define (finger-tree-remove-left tree)
  (match-non-empty-tree tree
    ((single elt)
     ;; single downgrades to empty
     *empty*)
    ((deep left spine-promise right)
     (if (digit-non-single? left)
	 ;; just drop one element from the left digit
	 (make-deep (digit-remove-left left) spine-promise right)

	 ;; no digit left, repair it
	 (restore-left-digit (force spine-promise) right)))))

(define (finger-tree-remove-right tree)
  ;; symmetric to finger-tree-remove-left
  (match-non-empty-tree tree
    ((single elt)
     *empty*)
    ((deep left spine-promise right)
     (if (digit-non-single? right)
	 (make-deep left spine-promise (digit-remove-right right))
	 (restore-right-digit left (force spine-promise))))))

(define (finger-tree-append meas tree1 . trees)
  ;; Reduces to append-binary.
  (fold (lambda (tree accum)
	  (append-binary meas accum tree))
	tree1
	trees))

;; Performs a *scan* on tree. This is a very general operation whose
;; meaning depends on the measure in use. In a set-like tree, scan is
;; analogous to a search operation. In a vector-like tree, scan is
;; analogous to vector-ref.
;;
;; A scan works like a fold that accumulates a measurement value that
;; is initially mzero. Elements are visited in left-to-right
;; order. Each element is measured, and that measurement is added to
;; the accumulated measurement. The element counts as a match if
;; (mpred <accumulated-measure>) yields true.
;;
;; On success (a match is found), calls
;;   (match m elt)
;; where m is the accumulated measurement *before* elt; and elt is the
;; matching element.
;;
;; On failure (no match is found), calls
;;   (absent m)
;; where m is the final accumulated measurement *after* all elements.
;;
;; In many use cases these m values are unneeded and can be ignored.
;;
;; Time efficiency is O(log n).
(define (finger-tree-scan meas mpred mzero tree match absent)
  (match-tree tree
    ((empty)
     ;; empty tree, fail
     (absent mzero))
    
    ((single a)
     ;; one element; measure it...
     (let ((m/a (measure-get+add meas mzero a)))
       ;; test the predicate...
       (if (mpred m/a)
	   (match mzero a)   ; success!
	   (absent m/a))))   ; fail!
    
    ;; first scan the left digit
    ((deep left spine-promise right)
     (scan-digit meas mpred mzero left
		 match ; success!
		 (lambda (m/left)
		   ;; no match in the left digit, try the spine recursively
		   (finger-tree-scan
		    (measure-spine meas)
		    mpred
		    m/left
		    (force spine-promise)
		    (lambda (m/pre node)
		      ;; success in a node; find which specific element
		      (scan-node meas mpred m/pre node match))
		    (lambda (m/spine)
		      ;; no match in the spine either, finally try the
		      ;; right digit
		      (scan-digit meas mpred m/spine right match absent))))))))

;; Bisect a tree. This is similar to the scan operation implemented in
;; finger-tree-scan. The definition for meas, mpred, mzero, tree, and
;; absent are identical to finger-tree-scan. The difference is in
;; match.
;;
;; On success, calls
;;    (match m prefix suffix)
;; where
;;  - m is the accumulated measurement *before* the matching element,
;;    as in a scan.
;;  - prefix is a tree containing all elements before the match. This
;;    may be empty if the match is the left element of the original
;;    tree.
;;  - suffix is a tree containing the match and all elements
;;    after. This will never be empty since it always contains at least
;;    the matching element.
;;
;; Just like scan, this is a very general operation whose effect
;; depends on the measurement in use. Depending on the measurement, it
;; could be used for a split-at, predecessor, successor, or tree
;; removal operation.
;;
;; Time efficiency is O(log n), but more expensive than
;; finger-tree-scan, so only use finger-tree-bisect when you actually
;; need the prefix or suffix.
(define (finger-tree-bisect meas mpred mzero tree match absent)
  (match-tree tree
	    
    ((empty) ; empty tree, failure
     (absent mzero))
    
    ((single x) ; one element, check it
     (let ((m/x (measure-get+add meas mzero x)))
       (if (mpred m/x)
	   (match mzero *empty* tree) ; note prefix is empty
	   (absent m/x))))
    
    ((deep left spine-promise right)
     ;; try the left digit
     (bisect-digit meas mpred mzero left
		   (lambda (m vect-pre vect-suf)
		     ;; match in the left digit; build prefix and suffix
		     (match m
			    (vector->finger-tree meas vect-pre)
			    (make-deep vect-suf spine-promise right)))
		   ;; no match in left digit, try the spine
		   (lambda (m/left)
		     (finger-tree-bisect
		      (measure-spine meas)
		      mpred
		      m/left
		      (force spine-promise)
		      (lambda (m spine-prefix spine-suffix)
			;; found a matching node; split everything
			;; into a prefix and suffix
			(bisect-node meas mpred
				     m left right
				     spine-prefix spine-suffix
				     match))
		      (lambda (m/spine)
			;; no match in spine, finally try the right digit
			(bisect-digit meas mpred m/spine right
				      (lambda (m vect-pre vect-suf)
					;; match in the right digit;
					;; this time we might have to
					;; deal with an empty vect-pre
					(match m
					       (if (= 0 (vector-length vect-pre))
						   (restore-right-digit left (force spine-promise))
						   (make-deep left spine-promise vect-pre))
					       (vector->finger-tree meas vect-suf)))
				      absent))))))))

(define (finger-tree-reverse meas tree)
  (generator->finger-tree meas (finger-tree->reverse-generator tree)))

(define (generator->finger-tree meas gen)
  ;; This has the potential to be a bottleneck so we're careful to
  ;; minimize overhead here. In partiular we're avoiding the overhead
  ;; of all the restructuring involved in using finger-tree-add-right
  ;; on each element.
  ;;
  ;; First try to generate 5 elements to make for a full left digit
  ;; and non-empty right digit. (We have to use let*, not let, to be
  ;; certain of order of evaluation.)
  (let* ((a (gen))
	 (b (gen))
	 (c (gen))
	 (d (gen))
	 (e (gen)))
    ;; Did we get all 5?
    (cond
     ((eof-object? a) *empty*)
     ((eof-object? b) (make-single a))
     ((eof-object? c) (finger-tree-2 a b))
     ((eof-object? d) (make-deep (digit a) *empty-promise* (digit b c)))
     ((eof-object? e) (make-deep (digit a) *empty-promise* (digit b c d)))
     (else
      ;; Set aside the first 4 elements for the left digit.
      (let ((left (digit a b c d)))
	(let loop ((spine *empty*)
		   (e e)) ; reserve 1 element so we can always make a right digit
	  ;; Try to generate 3 elements for a full node3.
	  (let* ((f (gen))
		 (g (gen))
		 (h (gen)))
	    (cond
	     ;; stop if we got <3
	     ((eof-object? f) (make-deep left (delay spine) (digit e)))
	     ((eof-object? g) (make-deep left (delay spine) (digit e f)))
	     ((eof-object? h) (make-deep left (delay spine) (digit e f g)))
	     ;; otherwise push a node3 into the spine; h is the new e
	     (else (loop (spine-add-3-right meas spine e f g)
			 h))))))))))

(define (list->finger-tree meas lst)
  (generator->finger-tree meas (list->generator lst)))

(define (vector->finger-tree meas vect)
  (generator->finger-tree meas (vector->generator vect)))

(define (finger-tree->generator tree)
  ;; Like generator->finger-tree this is a potential bottleneck so
  ;; we're careful to avoid consing here.
  ;;
  ;; (gen-elt elt) returns a generator for all elements in the subtree
  ;; rooted at elt.
  (let recurse ((gen-elt (lambda (elt)
			   (generator elt)))
		(tree tree))
    ;; (gen-digit dgt) returns a generator for all elements in the
    ;; given digit.
    (let ((gen-digit
	   (lambda (dgt)
	     (case (digit-length dgt)
	       ((1)
		(gen-elt (digit-ref dgt 0)))
	       ((2)
		(gappend (gen-elt (digit-ref dgt 0))
			 (gen-elt (digit-ref dgt 1))))
	       ((3)
		(gappend (gen-elt (digit-ref dgt 0))
			 (gen-elt (digit-ref dgt 1))
			 (gen-elt (digit-ref dgt 2))))
	       ((4)
		(gappend (gen-elt (digit-ref dgt 0))
			 (gen-elt (digit-ref dgt 1))
			 (gen-elt (digit-ref dgt 2))
			 (gen-elt (digit-ref dgt 3))))))))
    (match-tree tree
     ((empty)
      (generator))
     ((single a)
      (gen-elt a))
     ((deep left spine-promise right)
      (gappend
       (gen-digit left)
       (recurse (lambda (node)
		  (if (node2? node)
		      (gappend (gen-elt (node-x node))
			       (gen-elt (node-y node)))
		      (gappend (gen-elt (node-x node))
			       (gen-elt (node-y node))
			       (gen-elt (node-z node)))))
		(force spine-promise))
       (gen-digit right)))))))

(define (finger-tree->reverse-generator tree)
  ;; Symmetric to finger-tree->generator.
  (let recurse ((gen-elt (lambda (elt)
			   (generator elt)))
		(tree tree))
    (let ((gen-digit
	   (lambda (dgt)
	     (case (digit-length dgt)
	       ((1)
		(gen-elt (digit-ref dgt 0)))
	       ((2)
		(gappend (gen-elt (digit-ref dgt 1))
			 (gen-elt (digit-ref dgt 0))))
	       ((3)
		(gappend (gen-elt (digit-ref dgt 2))
			 (gen-elt (digit-ref dgt 1))
			 (gen-elt (digit-ref dgt 0))))
	       ((4)
		(gappend (gen-elt (digit-ref dgt 3))
			 (gen-elt (digit-ref dgt 2))
			 (gen-elt (digit-ref dgt 1))
			 (gen-elt (digit-ref dgt 0))))))))
    (match-tree tree
     ((empty)
      (generator))
     ((single a)
      (gen-elt a))
     ((deep left spine-promise right)
      (gappend
       (gen-digit right)
       (recurse (lambda (node)
		  (if (node2? node)
		      (gappend (gen-elt (node-y node))
			       (gen-elt (node-x node)))
		      (gappend (gen-elt (node-z node))
			       (gen-elt (node-y node))
			       (gen-elt (node-x node)))))
		(force spine-promise))
       (gen-digit left)))))))

(define (finger-tree->list tree)
  (generator->list (finger-tree->generator tree)))

(define (finger-tree->vector tree)
  (generator->vector (finger-tree->generator tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4) finger-tree-set (exported)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <set-order>
  (make-set-order-record measure comparator)
  set-order?
  (measure set-order-measure)
  (comparator set-order-comparator))

(define (make-set-order get-key comparator)
  (make-set-order-record (make-measure get-key
				       (lambda (m1 m2)
					 m2))
			 comparator))

(define (set-order-key order elt)
  (measure-get (set-order-measure order) elt))

(define (finger-tree-set-search order tree key match absent)
  (finger-tree-scan (set-order-measure order)
		    (make-set-pred order key)
		    *set-mzero*
		    tree
		    (lambda (m elt) ; match
		      (if (=? (set-order-comparator order)
			      key
			      (set-order-key order elt))
			  (match elt)
			  (absent)))
		    (lambda (m) ; absent
		      (absent))))

;; Multipurpose insert, remove, or replace.
;;
;; If tree contains an element x that is =? to elt, calls
;;   (match x remove replace)
;; where
;;  - x is that matching element
;;  - (remove) returns a copy of tree with elt excluded
;;  - (replace new-x) returns a copy of tree with new-x in place of
;;    x. new-x must be =? to elt to preserve sorted order.
;;
;; Otherwise (no such x), calls
;;   (absent insert)
;; where
;;  - (insert new-x) returns a copy of tree with new-x included.
;;    new-x must be =? to elt to preserve sorted order.
;;
;; O(log n) time. The include, remove, and replace procedures also
;; take O(log n) time each.
(define (finger-tree-set-update order tree elt match absent)
  (let ((meas (set-order-measure order)))
    (finger-tree-bisect
     meas
     (make-set-pred order (set-order-key order elt))
     *set-mzero*
     tree
     (lambda (m pre suf) ; match
       (let ((x (finger-tree-left suf))
	     (suf-except-x (finger-tree-remove-left suf)))
	 (if (=? (set-order-comparator order)
		 (set-order-key order elt)
		 (set-order-key order x))
	     (match x
		    (lambda () ; remove
		      (append-binary meas
				     pre
				     suf-except-x))
		    (lambda (new-x) ; replace
		      (append-binary meas
				     (finger-tree-add-right meas pre new-x)
				     suf-except-x)))
	     (absent ; match is >, not =
	      (lambda (new-x) ; insert
		(append-binary meas
			       (finger-tree-add-right meas pre new-x)
			       suf))))))
     (lambda (m) ; absent, elt must be > entire tree
       (absent
	(lambda (first-x) ; insert
	  (finger-tree-add-right meas tree first-x)))))))

;; Include elt; if tree already contains an element =? to elt, leave
;; the original element.
(define (finger-tree-set-adjoin order tree elt)
  (finger-tree-set-update order
			  tree
			  elt
			  (lambda (x remove replace)
			    tree)
			  (lambda (insert)
			    (insert elt))))

;; Include elt; if tree already contains an element =? to elt, it is
;; replaced with elt.
(define (finger-tree-set-replace order tree elt)
  (finger-tree-set-update order
			  tree
			  elt
			  (lambda (x remove replace)
			    (replace elt))
			  (lambda (insert)
			    (insert elt))))

;; Include elt; if tree already contains an element =? to elt, it is
;; replaced with elt.
(define (finger-tree-set-delete order tree elt)
  (finger-tree-set-update order
			  tree
			  elt
			  (lambda (x remove replace)
			    (remove))
			  (lambda (insert)
			    tree)))

;; Predecessor query: return the greatest of the elements whose key is
;; <? key. Calls (match pred) if one exists, or (absent) otherwise
;; (i.e. when tree is empty, or key is <=? all elements of tree).
(define (finger-tree-set-predecessor order tree key match absent)
  (finger-tree-bisect (set-order-measure order)
		      (make-set-pred order key)
		      *set-mzero*
		      tree
		      (lambda (m pre suf)
			(if (finger-tree-empty? pre)
			    (absent)
			    (match (finger-tree-right pre))))
		      (lambda (m)
			(if (finger-tree-empty? tree)
			    (absent)
			    (match (finger-tree-right tree))))))

;; Successor query: return the least of the elements whose key is >?
;; key. Calls (match pred) if one exists, or (absent) otherwise
;; (i.e. when tree is empty, or key is >=? all elements of tree).
(define (finger-tree-set-successor order tree key match absent)
  (finger-tree-bisect (set-order-measure order)
		      (make-set-pred order key)
		      *set-mzero*
		      tree
		      (lambda (m pre suf) ; match
			;; suf >= key; need to find a stricly > element in suf
			(let ((x (finger-tree-left suf)))
			  (if (>? (set-order-comparator order)
				  (set-order-key order x)
				  key)
			      ;; x > key, done
			      (match x)
			      
			      ;; otherwise use the element after x, if it exists
			      (let ((after-x (finger-tree-remove-left suf)))
				(if (finger-tree-empty? after-x)
				    (absent)
				    (match (finger-tree-left after-x)))))))
		      (lambda (m)
			(absent))))

(define finger-tree-set<?  (binary-relation->variadic set<?-binary ))
(define finger-tree-set<=? (binary-relation->variadic set<=?-binary))
(define finger-tree-set=?  (binary-relation->variadic set=?-binary ))

(define (finger-tree-set>=? order . sets)
  (apply finger-tree-set<=? order (reverse sets)))

(define (finger-tree-set>? order . sets)
  (apply finger-tree-set<? order (reverse sets)))

(define finger-tree-set-difference (binary-operation->variadic set-difference-binary))

(define finger-tree-set-intersect (binary-operation->variadic/reduce set-intersect-binary))

(define finger-tree-set-union (binary-operation->variadic/reduce set-union-binary))

(define finger-tree-set-xor (binary-operation->variadic set-xor-binary))

(define (increasing-generator->finger-tree-set order gen)
  (generator->finger-tree (set-order-measure order) gen))

(define (increasing-list->finger-tree-set order lst)
  (list->finger-tree (set-order-measure order) lst))

(define (increasing-vector->finger-tree-set order vect)
  (vector->finger-tree (set-order-measure order) vect))

(define (generator->finger-tree-set order reduce gen)
  (generator-fold (lambda (elt tree)
		    (finger-tree-set-update order
					    tree
					    elt
					    (lambda (x remove replace)
					      (replace (reduce x elt)))
					    (lambda (insert)
					      (insert elt))))
		  *empty*
		  gen))

(define (list->finger-tree-set order reduce lst)
  (generator->finger-tree-set order reduce (list->generator lst)))

(define (vector->finger-tree-set order reduce vect)
  (generator->finger-tree-set order reduce (vector->generator vect)))
