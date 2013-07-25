;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; immutable-structures-impl.scm
;;;;
;;;; See the file README for general remarks and LICENSE for license
;;;; information.
;;;;
;;;; This source file contains the implementation of all exported
;;;; immutable-structures procedures, and some non-exported helper
;;;; procedures. It depends only on R7RS-small and SRFI 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
	(scheme case-lambda)
	(srfi 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IDEQUE
;;;;
;;;; This is an implementation of unlabeled 2-3 finger trees. General
;;;; finger trees are described in
;;;;
;;;; "Finger Trees: A Simple General-purpose Data Structure", Ralf
;;;; Hinze and Ross Paterson,
;;;;   http://www.soi.city.ac.uk/~ross/papers/FingerTree.html
;;;;
;;;; General finger trees can be specialized to serve as efficient
;;;; pure-functional deques, heaps, priority queues, and search
;;;; trees. We implement only the special case of unlabeled 2-3 finger
;;;; trees, which are simpler and sufficient to function as efficient
;;;; deques, but not the other data structures. Unlabeled 2-3 data
;;;; structures are described, and implemented in Java, in this blog
;;;; post by Tommy McGuire:
;;;;   http://maniagnosis.crsr.net/2010/11/finger-trees.html
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ideque . elements)
  (list->ideque elements))

;;; A 2-3 finger tree is either empty, a singleton node, or a deep
;;; node.
(define (ideque? x)
  (or (ideque-empty? x)
      (ideque-single? x)
      (ideque-deep? x)))

;;; We use a singleton list object to represent all empty ideques.
(define *ideque-empty-object* (list 'unique))

(define (make-ideque-empty)
  *ideque-empty-object*)

(define (ideque-empty? x)
  (eq? x *ideque-empty-object*))

(define (ideque-length deque)
  (ideque-fold (lambda (x accum)
		 (add1 accum))
	       0
	       deque))

;;; A singleton ideque stores one element and nothing else.
(define-record-type <ideque-single>
  (make-ideque-single elt)
  ideque-single?
  (elt ideque-single-elt))

;;; A deep node stores
;;;
;;; - A left *finger* of 1-4 elements in a list in forward order.
;;;
;;; - A right *finger* of 1-4 elements in a list in backward order
;;; (i.e. the back of the deque is the front of the right finger).
;;;
;;; - A *spine* which is another finger tree whose elements are
;;; *nodes*. A node is a list of 2-3 elements.
;;;
;;; Key point: a spine element is a list of tree elements. Stated the
;;; other way, we have to group elements into lists of 2-3 to become a
;;; single element in the spine finger tree. This works recursively,
;;; so spine elements are lists; the spine's spine's elements are
;;; lists of lists; the spine's spine's spine's elements are lists of
;;; lists of lists, etc. This is part of why finger trees are
;;; efficient; as you descend deeper into the nested spine structures,
;;; the number of deque elements in each node increases exponentially.
(define-record-type <ideque-deep>
  (make-ideque-deep left spine right)
  ideque-deep?
  (left ideque-deep-left)
  (spine ideque-deep-spine)
  (right ideque-deep-right))

(define (make-ideque-deep-spineless left right)
  (make-ideque-deep left (make-ideque-empty) right))

;;; Macro to handle the 3 cases of finger trees separately.
(define-syntax ideque-case
  (syntax-rules ()
    ((ideque-case (ROOT X L S R) EMPTY SINGLE DEEP)
     (cond
      ((ideque-empty? ROOT) EMPTY)
      ((ideque-single? ROOT)
       (let ((X (ideque-single-elt ROOT)))
	 SINGLE))
      (else
       (let ((L (ideque-deep-left ROOT))
	     (S (ideque-deep-spine ROOT))
	     (R (ideque-deep-right ROOT)))
	 DEEP))))))

(define (ideque->list root)
  (ideque-case (root x l s r)
	       '()
	       (list x)
	       (append l
		       (concatenate (ideque->list s))
		       (reverse r))))

(define (ideque-append-list deque list)
  (fold (flip ideque-push-back) deque list))

(define (list->ideque list)
  (ideque-append-list (make-ideque-empty) list))

(define (ideque-front root)
  (ideque-case (root x l s r)
	       (error "ideque-front: empty")
	       x
	       (car l)))

(define (ideque-back root)
  (ideque-case (root x l s r)
	       (error "ideque-back: empty")
	       x
	       (car r)))

(define (ideque-push-front root elt)
  (ideque-case (root x l s r)
	       ;; Empty turns into singleton.
	       (make-ideque-single elt)

	       ;; Singleton turns into a deep node with 1-element
	       ;; fingers.
	       (make-ideque-deep-spineless (list elt) (list x))

	       (if (< (length l) 4)
		   ;; A deep node with fewer than 4 elements in the
		   ;; left finger can absorb elt into that finger.
		   (make-ideque-deep (cons elt l) s r)

		   ;; Otherwise the left finger has exactly 4
		   ;; elements. We push 3 of them into the spine to
		   ;; make room, and form a new finger with elt and
		   ;; the one element left over in the finger.
		   (make-ideque-deep (list elt (car l))
				     (ideque-push-front s (cdr l))
				     r))))

(define (ideque-push-back root elt)
  ;; symmetric to ideque-push-front
  (ideque-case (root x l s r)
	       (make-ideque-single elt)
	       (make-ideque-deep-spineless (list x) (list elt))
	       (if (< (length r) 4)
		   (make-ideque-deep l s (cons elt r))
		   (make-ideque-deep l
				     (ideque-push-back s (reverse (cdr r)))
				     (list elt (car r))))))

(define (ideque-pop-front root)
  (ideque-case (root x l s r)
	       ;; Already empty, error.
	       (error "ideque-pop-front: underflow")

	       ;; Singleton becomes empty.
	       (make-ideque-empty)

	       ;; Deep node drops an element.
	       (cond
		;; The left finger has at least 2 elements, so we can
		;; safely drop the first one.
		((>= (length l) 2)
		 (make-ideque-deep (cdr l) s r))

		;; Otherwise the left finger currently has exactly 1
		;; element and becomes empty after the pop.

		;; If the spine is nonempty we can recursively pop a
		;; node's worth of elements out of the spine to
		;; replenish the left finger.
		((not (ideque-empty? s))
		 (make-ideque-deep (ideque-front s)
				   (ideque-pop-front s)
				   r))

		;; Otherwise the spine is empty; all that remains is
		;; the right finger.

		;; If the right finger contains only one element, our
		;; deep node is demoted to a single node.
		((= 1 (length r))
		 (make-ideque-single (car r)))

		;; Otherwise we can move an element from the right
		;; finger to the left.
		(else
		 (let ((k (sub1 (length r))))
		   (make-ideque-deep-spineless (drop r k) (take r k)))))))

(define (ideque-pop-back root)
  ;; symmetric to ideque-pop-front
  (ideque-case (root x l s r)
	       (error "ideque-pop-back: underflow")
	       (make-ideque-empty)
	       (cond
		((>= (length r) 2)
		 (make-ideque-deep l s (cdr r)))
		((not (ideque-empty? s))
		 (make-ideque-deep l
				   (ideque-pop-back s)
				   (ideque-back s)))
		((= 1 (length l))
		 (make-ideque-single (car l)))
		(else
		 (let ((k (sub1 (length l))))
		   (make-ideque-deep-spineless (take l k) (drop l k)))))))

(define (ideque-append left . rest)
  ;; Use ideque-append-binary to append one deque at a time.
  (fold (flip ideque-append-binary) left rest))

;;; Append right to the end of left.
(define (ideque-append-binary left right)
  (cond
   ;; Trivial cases when one or both deques are non-deep.
   ((ideque-empty? left)
    right)
   ((ideque-empty? right)
    left)
   ((ideque-single? left)
    (ideque-push-front right (ideque-single-elt left)))
   ((ideque-single? right)
    (ideque-push-back left (ideque-single-elt right)))

   ;; Otherwise build a new deep node. We reuse the exterior fingers
   ;; (i.e. left's left finger and right's right finger). The spines
   ;; and interior fingers are combined into one spine with
   ;; ideque-meld-middle.
   (else
    (make-ideque-deep (ideque-deep-left left)
		      (ideque-meld-middle left right)
		      (ideque-deep-right right)))))

;; Combine left's spine, left's right finger, right's spine, and
;; right's left finger into one spine.
(define (ideque-meld-middle left right)
  ;; We arbitrarily decide to push all of the "loose" elements in the
  ;; interior fingers into left. Then we have two spines which we
  ;; combine recursively with ideque-append-binary. Note that
  ;; ideque-append-binary and ideque-meld-middle are mutually
  ;; recursive.
  (let loop ((left-spine (ideque-deep-spine left))
	     (loose (append (reverse (ideque-deep-right left))
			    (ideque-deep-left right))))
    ;; Each loop iteration bunches up two or three loose elements
    ;; (preferring 3) into a spine node and pushes them onto the back
    ;; of left-spine. A finger has 1-4 elements so between the two
    ;; interior fingers there are 2-8 loose elements to begin
    ;; with. When exactly 2 or 4 elements remain we create 2-node;
    ;; otherwise we create a 3-node. Observe that, for 2-8 elements,
    ;; this process always succesfully partitions the list of loose
    ;; elements into 2- or 3-nodes with no leftover element.
    (if (null? loose)
	;; Every loose element has been handled and all that remains
	;; are two spines; append them.
	(ideque-append-binary left-spine (ideque-deep-spine right))

	;; Otherwise peel off a 2-node or 3-node and continue looping.
	(let ((node-width (case (length loose)
			    ((2 4) 2)
			    (else 3))))
	  (loop (ideque-push-back left-spine (take loose node-width))
		(drop loose node-width))))))

(define (ideque-filter proc deque)
  (list->ideque (filter proc (ideque->list deque))))

(define (ideque-fold proc base deque)
   (ideque-case (deque x l s r)
		base
		(proc x base)
		(fold proc
		      (ideque-fold (lambda (node accum)
				     (fold proc accum node))
				   (fold proc base l)
				   s)
		     r)))

(define (ideque-map proc deque)
  (ideque-case (deque x l s r)
	       deque
	       (make-ideque-single (proc x))
	       (make-ideque-deep (map proc l)
				 (ideque-map (lambda (node)
					       (map proc node))
					     s)
				 (map proc r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ORDERED LISTS
;;;;
;;;; An ordered list is a proper list of elements in strict increasing
;;;; order (no ties). There are exported procedures for converting
;;;; between ordered lists and isets, and we also use ordered list as
;;;; an intermediate set representation. Ordered lists have a simple
;;;; flat structure, unlike the hierarchical structure of
;;;; self-balancing trees, so they can be overhauled more easily (and
;;;; probably more efficiently) than trees. In particular, we
;;;; implement filter and the set-theoretic operations by flattening
;;;; trees to ordered lists, performing the operation on the list, and
;;;; converting the resulting list back into a tree.
;;;;
;;;; These operations form the foundation of many other higher-level
;;;; operations, so are somewhat performance critical. Please pardon
;;;; our dust, as the code in this section pays unusual attention to
;;;; low-level details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A generalized ordered list merge operation. with-unique-left,
;;; with-unique-right, and with-equal are boolean expressions that
;;; control the semantics of the merge.
;;;
;;; When with-unique-left is true, elements that are present in the
;;; left list but not the right are retained; otherwise they are
;;; dropped. With-unique-right works symmetrically.
;;;
;;; When with-equal is true, elements that are present in both lists
;;; are retained; in order to maintain element distinctness, one of
;;; the two matching objects is selected by the supplied merger
;;; procedure.
;;;
;;; This operation generalizes the merge operation found in merge sort
;;; and also all the set-theoretic operations.
;;;
;;; merge! is destructive on the lists; the returned list reuses the
;;; cons cells of left and/or right.
;;;
;;; The tight inner loop involves conditional branches over
;;; with-equal-left, with-equal-right, and with-equal, which are
;;; usually constant booleans. So we define merge! as a macro in the
;;; hopes that will help the compiler eliminate the dead branches.
(define-syntax merge!
  (syntax-rules ()
    ((merge! with-unique-left with-unique-right with-equal
	     precedes? merger left right)
     ;; Result is accumulated in LIFO order, so at the end of the loop
     ;; it is backwards and needs to be reversed.
     (let loop ((left left) (right right) (result '()))
       (cond
	;; Left is empty, so we're done. If right still has any
	;; elements they should be appended iff with-unique-right is
	;; true.
	((null? left)
	 (if with-unique-right
	     (append! (reverse! result) right)
	     (reverse! result)))

	;; symmetric case
	((null? right)
	 (if with-unique-left
	     (append! (reverse! result) left)
	     (reverse! result)))

	;; The head of left precedes the head of right. We'll skip
	;; over it and continue looping; it gets added to result iff
	;; with-unique-left is true.
	((precedes? (car left) (car right))
	 ;; Note that we need to explicitly capture (cdr left)
	 ;; *before* the call to set-cdr/return!  since, as per R7RS
	 ;; 4.1.3, function call arguments may be evaluated in any
	 ;; order. There used to be a bug here because we had
	 ;;
	 ;;  (loop (cdr left)
	 ;;        ...
	 ;;        (set-cdr/return! left result)
	 ;;
	 ;; and Chibi evaluated the set-cdr/return! before (cdr left),
	 ;; as it is perfectly entitled to do.
	 (let ((next-left (cdr left)))
	   (loop next-left
		 right
		 (if with-unique-left
		     (set-cdr/return! left result)
		     result))))

	;; symmetric case
	((precedes? (car right) (car left))
	 (let ((next-right (cdr right)))
	   (loop left
		 next-right
		 (if with-unique-right
		     (set-cdr/return! right result)
		     result))))

	;; Otherwise the head elements are tied according to our
	;; ordering. If with-equal is true we include whichever one
	;; is chosen by merger, otherwise we drop both.
	(else
	 (let ((next-left (cdr left)))
	   (loop next-left
		 (cdr right)
		 (if with-equal
		     (begin (set-car! left (merger (car left) (car right)))
			    (set-cdr/return! left result))
		     result)))))))))

;;; Now we can define set union, intersection, difference, and xor as
;;; special cases of merge!. The only significant difference is the
;;; value of the three boolean control parameters.

(define (ordered-list-union! left right precedes? merger)
  (merge! #t #t #t precedes? merger left right))

(define (ordered-list-intersection! left right precedes? merger)
  (merge! #f #f #t precedes? merger left right))

(define (ordered-list-difference! left right precedes?)
  (merge! #t #f #f precedes? iset-merger-left left right))

(define (ordered-list-xor! left right precedes?)
  (merge! #t #t #f precedes? iset-merger-left left right))

(define (iset-merger-left left right)
  left)
(define (iset-merger-right left right)
  right)

;;; Split a list nearly evenly. Returns
;;;
;;;   (values left right)
;;;
;;; such that the elements of the input list are distributed between
;;; left and right, and the lengths of left and right differ by at most
;;; one. The returned lists are freshly allocated.
;;;
;;; When the input list contains fewer than two elements, right is null
;;; and left is a copy of the input list.
(define (list-split list)
  ;; Alternate between adding an element to now and next. This way we
  ;; only need to scan the list once.
  (let loop ((list list) (now '()) (next '()))
    (if (null? list)
	;; We have to return next as left to get the short-list
	;; behavior described above.
	(values next now)

	;; Swap the identity of next and now.
	(loop (cdr list)
	      next
	      (cons (car list) now)))))

;;; This is a variant of merge sort. The difference is that classical
;;; merge sort keeps duplicated elements and usually guarantees they
;;; are kept in their original order (stability). Here we drop
;;; duplicates, so stability is irrelevant.
(define (list->ordered-list list precedes? merger)
  (let recurse ((list list))
    (let-values (((left right) (list-split list)))
      (if (null? right)
	  left

	  ;; Note that list-split returns fresh lists, so the
	  ;; destructiveness of ordered-list-union! cannot affect the
	  ;; input list.
	  (ordered-list-union! (recurse left)
			       (recurse right)
			       precedes?
			       merger)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1-2 BROTHER TREES
;;;;
;;;; 1-2 brother trees, henceforth called btrees for brevity, are
;;;; variant of self-balancing binary search trees. Their
;;;; implementation in the functional paradigm is described
;;;; exquisitely in
;;;;
;;;;   [Hinze] "FUNCTIONAL PEARLS: Purely Functional 1-2 Brother
;;;;   Trees" by Ralf Hinze .
;;;;
;;;; A preprint is available at
;;;;   http://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf
;;;;
;;;; Our implementation follows the algorithms described in that paper
;;;; closely.
;;;;
;;;; We choose 1-2 brother trees over competing structures, such as
;;;; red-black trees or labeled finger trees, for two reasons. First,
;;;; the insert and delete operations on 1-2 brother trees are
;;;; unusually simple when in an immutable context. Second, there is a
;;;; relatively straightforward algorithm for converting an ordered
;;;; list into a 1-2 brother tree in O(n) time and using only O(log-n)
;;;; additional space. We perform that operation frequently so its
;;;; performance is important.
;;;;
;;;; Like most binary search trees, a btree is composed mainly of
;;;; binary nodes, each with a single element, left subtree, and right
;;;; subtree. Perfect binary search trees may only be built when the
;;;; number n of elements is one less than a power of two, so any
;;;; practical tree structure must allow for some kind of "slack" in
;;;; its structure. The slack in btrees come from *unary* nodes, which
;;;; contain only a single child pointer and no element. Unary nodes
;;;; do contribute to the height of the tree but include zero
;;;; elements, which makes it possible to construct a correct btree
;;;; for any value of n.
;;;;
;;;; A correct btree has three properties:
;;;;
;;;; (1) Inorder: This is the usual binary search tree property. For
;;;; any binary node, all left descendants precede the node's element,
;;;; which precedes all right descendants.
;;;;
;;;; (2) Balanced: All leaves have the same depth. (A leaf is a node
;;;; with no children.)
;;;;
;;;; (3) Brotherly: Each unary node has a binary brother.
;;;;
;;;; The "brotherly" property prevents unary nodes from becoming too
;;;; prevalent. Observe that, in a brotherly btree,
;;;;
;;;; - the root is not unary;
;;;; - a binary node has at most one unary child; and
;;;; - a unary node never has a unary child.
;;;;
;;;; So fewer than half of the nodes along any path from the root to a
;;;; leaf are unary, and the height of a correct 1-2 brother tree is
;;;; O(log-n).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Nullary btree, i.e. the null child of a leaf.
(define (make-btree0)
  '())
(define btree0? null?)

;;; Unary btree node; has only a single child and no element.
(define-record-type <btree1>
  (make-btree1 child)
  btree1?
  (child btree1-child))

;;; Binary btree node; has a left subtree, right subtree, and a single
;;; element.
(define-record-type <btree2>
  (make-btree2 left elt right) ; note the left-to-right argument order
  btree2?
  (left btree2-left)
  (elt btree2-elt)
  (right btree2-right))

;;; A leaf is a binary node with both children nullary.
(define (make-btree-leaf elt)
  (make-btree2 (make-btree0) elt (make-btree0)))

;;; Convenience procedure to unpack a btree2 into its constituent
;;; parts.
(define (btree2-unpack root)
  (values (btree2-left root) (btree2-elt root) (btree2-right root)))

;;; Filter btree nodes in either order, and fold the elements of the
;;; selected nodes. If increasing-order? is true, the nodes are
;;; visited in increasing order (forwards); otherwise in decreasing
;;; order (backwards). Calls (filter-proc x) on each binary node's
;;; element x to decide whether to fold the node and its subtrees. The
;;; elements are folded by repeated application of (proc element
;;; accum) as per SRFI-1 fold.
(define (btree-filter-fold increasing-order? filter-proc fold-proc base root)
  (let-values (((fst-child snd-child) (if increasing-order?
					  (values btree2-left btree2-right)
					  (values btree2-right btree2-left))))
    (let visit ((root root) (accum base))
      (cond
       ((btree0? root)
	accum)
       ((btree1? root)
	(visit (btree1-child root)
		 accum))
       (else
	(if (filter-proc (btree2-elt root))
	    (visit (snd-child root)
		   (fold-proc (btree2-elt root)
			      (visit (fst-child root)
				     accum)))
	    accum))))))

;;; Fold all elements in either oder.
(define (btree-fold increasing-order? proc base root)
  (btree-filter-fold increasing-order? (const #t) proc base root))

;;; Range query.
(define (btree-range->ordered-list root
				   least include-least
				   most include-most
				   precedes?)
  ;; Build the list backwards with cons and fold in backwards order;
  ;; the two backwardses cancel each other out.
  (btree-filter-fold #f
		     (lambda (elt)
		       (and (or (precedes? least elt)
				(and include-least
				     (not (precedes? elt least))))
			    (or (precedes? elt most)
				(and include-most
				     (not (precedes? most elt))))))
		     cons
		     '()
		     root))

;;; Recursive walk over a btree. The first form
;;;
;;;   (btree-walk VISIT ROOT NULLARY UNARY BINARY)
;;;
;;; creats a continuation VISIT which may be called as in
;;;
;;;   (VISIT node)
;;;
;;; to recursively continue the walk in the subtree rooted at
;;; node. NULLARY, UNARY, and BINARY are evaluated with ROOT bound to
;;; the current node when ROOT is nullary, unary, or binary node,
;;; respetively.
;;;
;;; The second form
;;;
;;;   (btree-walk VISIT ROOT UNARY BINARY)
;;;
;;; always calls (VISIT (btree1-child ROOT)) for unary ROOTs. This is
;;; a convenience, as often unary nodes can be skipped entirely.
(define-syntax btree-walk
  (syntax-rules ()
    ((btree-walk visit root nullary unary binary)
     (let visit ((root root))
       (cond
	((btree0? root)
	 nullary)
	((btree1? root)
	 unary)
	(else
	 binary))))
    ((btree-walk visit root nullary binary)
     (btree-walk visit root
		    nullary
		    (visit (btree1-child root))
		    binary))))

;;; If root is unary, return its child; otherwise return root
;;; itself. This is helpful in skipping over unary nodes. Since unary
;;; nodes never have unary children, the returned node is never unary.
(define (btree-skip-unary root)
  (if (btree1? root)
      (btree1-child root)
      root))

;;; Return true if the subtree root contains no elements. This is the
;;; case if it and its descendants are all unary or nullary.
(define (btree-empty? root)
  (btree0? (btree-skip-unary root)))

;;;; Correctness checks, provided for testing. ;;;;

(define (btree-correct? root precedes?)
  (and (btree-brotherly? root)
       (btree-balanced? root)
       (btree-inorder? root precedes?)))

(define (btree-brotherly? root)
  (btree-walk visit root
	      #t
	      (and (not (btree1? (btree1-child root)))
		   (visit (btree1-child root)))
	      (and (btree2-brotherly? root)
		   (visit (btree2-left root))
		   (visit (btree2-right root)))))

(define (btree2-brotherly? root)
  (not (and (btree1? (btree2-left root))
	    (btree1? (btree2-right root)))))

(define (btree-balanced? root)
  (let-values (((min-ht max-ht) (btree-height-range root)))
    (= min-ht max-ht)))

;;; Returns (values min-height max-height).
(define (btree-height-range root)
  (btree-walk visit root
	      (values 0 0)
	      (let-values (((min max) (visit (btree1-child root))))
		(values (add1 min) (add1 max)))
	      (let-values (((l-min l-max) (visit (btree2-left root)))
			   ((r-min r-max) (visit (btree2-right root))))
		(values (add1 (min l-min r-min))
			(add1 (max l-max r-max))))))

(define (btree-inorder? root precedes?)
  (btree-walk visit root
	      #t
	      (let ((elt (btree2-elt root))
		    (left (btree-skip-unary (btree2-left root)))
		    (right (btree-skip-unary (btree2-right root))))
		(and (or (btree0? left)
			 (precedes? (btree2-elt left) elt))
		     (or (btree0? right)
			 (precedes? elt (btree2-elt right)))
		     (visit left)
		     (visit right)))))

;;; Searches for an element "match" in root. If such a match is found,
;;; returns the result of calling
;;;   (present-proc match)
;;; otherwise, returns the result of
;;;   (absent-thunk) . O(log-n) time.
(define (btree-search root x absent-thunk present-proc precedes?)
  (btree-walk visit root
	      (absent-thunk)
	      (cond
	       ((precedes? x (btree2-elt root))
		(visit (btree2-left root)))
	       ((precedes? (btree2-elt root) x)
		(visit (btree2-right root)))
	       (else
		(present-proc (btree2-elt root))))))

;;;; Deletion ;;;;

;;; Typically, deleting an element from a binary search tree is more
;;; complicated than inserting an element. The situation is actually
;;; reversed for btrees; deletion is simpler than insertion. Before we
;;; get to the actual deletion procedure, we introduce some helpers
;;; for building a "trio" of a binary node and its two children.

;;; In a "full" trio both children are binary. Note the left-to-right
;;; argument order.
(define (make-trio/full left-left
			left-elt
			left-right
			middle-elt
			right-left
			right-elt
			right-right)
  (make-btree2 (make-btree2 left-left left-elt left-right)
	       middle-elt
	       (make-btree2 right-left right-elt right-right)))

;;; We can also build a trio with one unary child.
(define (make-trio/unary-right left-left
			       left-elt
			       left-right
			       middle-elt
			       right)
  (make-btree2 (make-btree2 left-left left-elt left-right)
	       middle-elt
	       (make-btree1 right)))

(define (make-trio/unary-left left
			       middle-elt
			       right-left
			       right-elt
			       right-right)
  (make-btree2 (make-btree1 left)
	       middle-elt
	       (make-btree2 right-left right-elt right-right)))

;;; Remove element x from root. This procedure assumes that x is
;;; indeed present in the tree. O(log-n) time.
(define (btree-remove root x precedes?)
  ;; See section 5 of [Hinze]. Deletion involves a downward phase and
  ;; then an upward phase. In the downward phase we search for the
  ;; node containing x. If x's successor is in that node's right
  ;; subtree, we remove x and replace it with its successor element;
  ;; otherwise we replace the binary node with a unary node. The
  ;; latter case may introduce a violation of the brotherly
  ;; property. In the upward phase, we check each node for violations
  ;; of the brotherly property in its children, and either repair them
  ;; or propagate them upward. If the violation reaches the root, that
  ;; means the root is the sole child of a unary node, so we just drop
  ;; the unary node and that child becomes the new root.

  ;; The code in [Hinze] uses one function n_2 to rebuild binary nodes
  ;; and repair brotherly violations in either the left or right
  ;; child. Instead, we have one procedure for a node with a suspect
  ;; left child, and another for a node with a suspect right child.

  (define (repair-left left elt right)
    (cond
     ;; Both children are unary; transform into a unary node whose
     ;; child is a binary node with nonunary children. See the figure
     ;; on the bottom of p. 7 of [Hinze].
     ((and (btree1? left) (btree1? right))
      (make-btree1 (make-btree2 (btree1-child left) elt (btree1-child right))))

     ;; The left child is a chain of two unary nodes. We can rotate
     ;; the tree to break up the unary nodes, but we have to be
     ;; careful not to push the brotherly violation elsewhere. There
     ;; are three cases, and in each there is an inevitable
     ;; rearrangement that restores the brotherly property. See the
     ;; figure at the top of p. 8 of [Hinze].
     ((and (btree1? left)
	   (btree1? (btree1-child left)))
      (let*-values (((left-child) (btree1-child left))
		    ((left-child-child) (btree1-child left-child))
		    ((right-left right-elt right-right) (btree2-unpack right)))
	(cond
	 ;; The case depicted on the left: the left child of the right
	 ;; subtree is unary.
	 ((btree1? right-left)
	  (make-btree2 (make-btree2 left-child-child
				    elt
				    (btree1-child right-left))
		       right-elt
		       right-right))

	 ;; The case depicted on the right: the right child of the
	 ;; right subtree is unary.
	 ((btree1? right-right)
	  (make-trio/full left-child-child
			  elt
			  (btree2-left right-left)
			  (btree2-elt right-left)
			  (btree2-right right-left)
			  right-elt
			  (btree1-child right-right)))

	 ;; The case depicted on the bottom: neither child of the
	 ;; right subtree is unary.
	 (else
	  (make-btree2 (make-btree2 left-child
				    elt
				    right-left)
		       right-elt
		       (make-btree1 right-right))))))

     ;; Otherwise there are no violations so we simply build a new
     ;; binary node.
     (else
      (make-btree2 left elt right))))
  
  ;; symmetric to above
  (define (repair-right left elt right)
    (cond
     ((and (btree1? left) (btree1? right))
      (make-btree1 (make-btree2 (btree1-child left) elt (btree1-child right))))
     ((and (btree1? left)
	   (btree1? (btree1-child left)))
      (let*-values (((right-child) (btree1-child right))
		    ((right-child-child) (btree1-child right-child))
		    ((left-left left-elt left-right) (btree2-unpack left)))
	(cond
	 ((btree1? left-right)
	  (make-btree2 left-left
		       left-elt
		       (make-btree2 (btree1-child left-right)
				    elt
				    right-child-child)))
	 ((btree1? left-left)
	  (make-trio/full (btree1-child left-left)
			  left-elt
			  (btree2-left left-right)
			  (btree2-elt left-right)
			  (btree2-right left-right)
			  elt
			  right-child-child))
	 (else
	  (make-btree2 (make-btree1 left-left)
		       left-elt
		       (make-btree2 left-right
				    elt
				    right-child))))))
     (else
      (make-btree2 left elt right))))

  ;; Delete the minimum element of a subtree. The subtree must be
  ;; nonempty.
  (define (remove-min root)
    (btree-walk visit root
		root ; should be unreachable
		(make-btree1 (visit (btree1-child root)))
		(let-values (((left elt right) (btree2-unpack root)))
		  (if (btree-empty? left)
		      (make-btree1 right)
		      (repair-left (visit left) elt right)))))

  (btree-skip-unary ; repair a brotherly violation at the root
   (btree-walk visit root
	       root
	       (make-btree1 (visit (btree1-child root)))
	       (let-values (((left elt right) (btree2-unpack root)))
		 (cond
		  ((precedes? x elt)
		   (repair-left (visit left) elt right))
		  ((precedes? elt x)
		   (repair-right left elt (visit right)))
		  (else
		   (if (btree-empty? right)
		       ;; binary node becomes unary, possibly
		       ;; introducing a violation
		       (make-btree1 left)

		       ;; x's successor (the leftmost child of the
		       ;; right subtree) takes the place of x
		       (repair-right left 
				     (btree-min right) 
				     (remove-min right)))))))))

;;;; Insertion ;;;;

;;; At a high level, insertion is similar to deletion; it involves a
;;; search-like downward phase, inserting an element into a node at
;;; the bottom of the tree, and an upward phase that repairs property
;;; violations or propogates them upward. Insertion is trickier,
;;; though, because inserting an element into a nullary node creates a
;;; binary node that is higher than its brother, violating the balance
;;; property; and inserting an element into a binary node creates a
;;; *trinary* node with two elements and three children. So there are
;;; more special cases, and some of them involve more pieces of data,
;;; than in deletion.

;;; An unbalanced leaf is a leaf node inserted at the bottom of the
;;; tree that is deeper than its brother. The insertion step may
;;; create an unbalanced leaf, but will always eliminate it during the
;;; upward repair phase.
(define-record-type <unbalanced-leaf>
  (make-unbalanced-leaf elt)
  unbalanced-leaf?
  (elt unbalanced-leaf-elt))

;;; A trinary node has two elements and three children. Like
;;; unbalanced leaves, trinary nodes may be temporarily inserted into
;;; a btree, but will be eliminated during the upward repair phase.
(define-record-type <btree3>
  (make-btree3 left left-elt middle right-elt right)
  btree3?
  (left btree3-left)
  (left-elt btree3-left-elt)
  (middle btree3-middle)
  (right-elt btree3-right-elt)
  (right btree3-right))

(define (btree-insert root elt precedes?)
  ;; Repairing a root node or unary node is nearly the same process;
  ;; the only difference is what happens when the node is already
  ;; correct and is left unchanged.
  (define (repair-root-or-unary root unchanged-proc)
    (cond
     ((unbalanced-leaf? root)
      (make-btree-leaf (unbalanced-leaf-elt root)))
     ((btree3? root)
      (make-trio/unary-right (btree3-left root)
			     (btree3-left-elt root)
			     (btree3-middle root)
			     (btree3-right-elt root)
			     (btree3-right root)))
     (else
      (unchanged-proc root))))

  (define (repair-unary root)
    ;; Replace a unary non-root node with a new unary node.
    (repair-root-or-unary root make-btree1))

  (define (repair-root root)
    ;; Replace a unary root with its child.
    (repair-root-or-unary root id))

  ;; Rebuild a binary node with a suspect left child.
  (define (repair-left left elt right)
    (cond
     ;; Incorporate an unbalanced left leaf child into a balanced leaf
     ;; trinary node. This restores the balance property but
     ;; introduces a trinary node which will later be eliminated.
     ((unbalanced-leaf? left)
      (make-btree3 (make-btree0)
		   (unbalanced-leaf-elt left)
		   (make-btree0)
		   elt
		   (make-btree0)))

     ;; See the figure on p. 4 of [Hinze]. There is a trinary left
     ;; child which we eliminate or propagate upwards.
     ((btree3? left)
      (if (btree1? right)
	  ;; If the right child is unary we can combine the trinary
	  ;; left child, this binary node, and the unary right child
	  ;; into a trio of binary nodes, and the property violation
	  ;; is fixed.
	  (make-trio/full (btree3-left left)
			  (btree3-left-elt left)
			  (btree3-middle left)
			  (btree3-right-elt left)
			  (btree3-right left)
			  elt
			  (btree1-child right))

	  ;; Otherwise we form a trinary node with a unary middle
	  ;; child, which propagates the trinary violation one step
	  ;; upward.
	  (make-btree3 (make-btree2 (btree3-left left)
				    (btree3-left-elt left)
				    (btree3-middle left))
		       (btree3-right-elt left)
		       (make-btree1 (btree3-right left))
		       elt
		       right)))

     ;; There are no violations in the left child so we simply form a
     ;; new binary node.
     (else
      (make-btree2 left elt right))))

  ;; symmetric to above
  (define (repair-right left elt right)
    (cond
     ((unbalanced-leaf? right)
      (make-btree3 (make-btree0)
		   elt
		   (make-btree0)
		   (unbalanced-leaf-elt right)
		   (make-btree0)))
     ((btree3? right)
      (if (btree1? left)
	  (make-trio/full (btree1-child left)
			   elt
			   (btree3-left right)
			   (btree3-left-elt right)
			   (btree3-middle right)
			   (btree3-right-elt right)
			   (btree3-right right))
	  (make-btree3 left
		       elt
		       (make-btree1 (btree3-left right))
		       (btree3-left-elt right)
		       (make-btree2 (btree3-middle right)
				    (btree3-right-elt right)
				    (btree3-right right)))))

     (else
      (make-btree2 left elt right))))

  (repair-root
   (btree-walk visit root
	       (make-unbalanced-leaf elt)
	       (repair-unary (visit (btree1-child root)))
	       (if (precedes? elt (btree2-elt root))
		   (repair-left (visit (btree2-left root))
				(btree2-elt root)
				(btree2-right root))
		   (repair-right (btree2-left root)
				 (btree2-elt root)
				 (visit (btree2-right root)))))))

;;;; Construction from an ordered list ;;;;

;;; See section 3 of [Hinze]. The following algorithm builds a correct
;;; btree out of an ordered list in O(n) time. This is faster than
;;; repeatedly using btree-insert in an asymptotic sense, as the
;;; latter would take O(n-log-n) time. The construction algorithm
;;; involves involves substantially less consing and replacement of
;;; nodes than repeated insertion would, so the construction algorithm
;;; probably has a better constant factor as well.
;;;
;;; We fold elements into the tree starting at the front of the
;;; list. Since we process an ordered list in order, we will always be
;;; adding a new maximum element to the tree. The maximum element of a
;;; btree is always in the bottom-right node.
;;;
;;; The algorithm works by keeping a reference to this bottom-right
;;; node. Every time we add an element it becomes the new bottom-right
;;; node, and we push the old maximum element up toward the root. In
;;; some cases we can merge the stray element into the trunk of the
;;; tree by rotating leftward. In other cases we need to push an
;;; element further upward. Occasionally an element makes it all the
;;; way to the top, in which case we create a new root and increase
;;; the height of the tree by one.
;;;
;;; As discussed in [Hinze], inserting a maximum element may be viewed
;;; as an arithmetic increment operation in a curious number system
;;; involving the digits 1 and 1/2. To implement this properly we need
;;; to keep track of whether each node along the right spine is a "1"
;;; or a "1/2".
;;;
;;; Note that [Hinze] uses a right fold, so their algorithm inserts
;;; elements from right to left, i.e. repeatedly inserts a minimum
;;; element at the bottom-left and works along the left spine. Our
;;; algorithm is symmetric to theirs.

;;; A spine node is a binary node along the right spine of a
;;; btree. Since we insert elements bottom-up, a spine has an up
;;; pointer and left pointer instead of the downward-facing left and
;;; right pointers.
(define-record-type <spine>
  (make-spine half? elt left up)
  spine?
  (half? spine-half?)
  (elt spine-elt)
  (left spine-left)
  (up spine-up))

(define make-spine-null make-btree0)
(define spine-null? btree0?)

(define (make-spine-full elt left up)
  (make-spine #f elt left up))

(define (make-spine-half elt left up)
  (make-spine #t elt left up))

(define (spine-full? spine)
  (not (spine-half? spine)))

;;; Insert a new maximum element into the btree whose bottom-right
;;; node is spine.
(define (spine-insert-max spine elt)
  (define (carry elt left up)
    (cond
     ((spine-null? up)
      (make-spine-half elt left (make-spine-null)))
     ((spine-half? up)
      (make-spine-full elt
		       (make-btree2 (spine-left up)
				    (spine-elt up)
				    left)
		       (spine-up up)))
     (else
      (make-spine-half elt
		       left
		       (carry (spine-elt up)
			      (spine-left up)
			      (spine-up up))))))

  (make-spine-full elt
		   (make-btree0)
		   (if (spine-null? spine)
		       (make-spine-null)
		       (carry (spine-elt spine)
			      (spine-left spine)
			      (spine-up spine)))))

;;; A spine is a lower node pointing at a higher one; a btree is a
;;; higher node pointing to lower ones. So converting a spine to a
;;; btree is essentially reversing the order of the spine nodes and
;;; converting each to a btree2 node.
(define (spine->btree spine)
  (let loop ((spine spine) (root (make-btree0)))
    (if (spine-null? spine)
	root
	(loop (spine-up spine)
	      (make-btree2 (if (spine-half? spine)
			       (make-btree1 (spine-left spine))
			       (spine-left spine))
			   (spine-elt spine)
			   root)))))

;; Convert an ordered list to a btree. This uses spine-insert-max to
;; insert the list elements one at a time, then spine->btree to
;; get a proper btree. O(n) time.
(define (ordered-list->btree list)
  (spine->btree (fold (flip spine-insert-max) (make-spine-null) list)))

;;;; Miscellaneous btree operations ;;;;

(define (btree->ordered-list root)
  (btree-fold #f cons '() root))	      

(define (btree-size root)
  (btree-fold #t
	      (lambda (elt size)
		(add1 size))
	      0
	      root))

;;; Returns a procedure that maps a btree to its minimum or maximum
;;; element. Each involves following left-child or right-child
;;; pointers until we reach a leaf.
(define (make-btree-extremum child-accessor)
  (lambda (root)
    (btree-walk visit root
		(error "unexpected empty tree")
		(let ((child (btree-skip-unary (child-accessor root))))
		  (if (btree0? child)
		      (btree2-elt root)
		      (visit child))))))

(define btree-min (make-btree-extremum btree2-left))
(define btree-max (make-btree-extremum btree2-right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ISET
;;;;
;;;; This is the exported immutable set (iset) interface. It is a thin
;;;; wrapper around the lower-level btree and ordered list data
;;;; structures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An iset is defined by an order predicate over its elements and the
;;; root of a btree. We also cache the size, and the minimum and
;;; maximem element of nonempty sets, so that those things may be
;;; reported in O(1) time.
(define-record-type <iset>
  (make-iset precedes? root size min max)
  iset?
  (precedes? iset-precedes?)
  (root iset-root)
  (size iset-size)
  (min iset-min)
  (max iset-max))

;;;; Non-exported construction procedures ;;;;

;;; Build an iset from an order predicate, btree root, and precomputed
;;; size. The min and max elements are extracted from the
;;; btree. O(log-n) time.
(define (btree->iset/size precedes? root size)
  (if (btree-empty? root)
      (make-iset-empty precedes?)
      (make-iset precedes?
		 root
		 size
		 (btree-min root)
		 (btree-max root))))

;;; As with btree->iset/size, but size is computed by traversing the
;;; btree. O(n) time.
(define (btree->iset precedes? root)
  (btree->iset/size precedes? root (btree-size root)))

;;; Construct an empty iset.
(define (make-iset-empty precedes?)
  (make-iset precedes? (make-btree0) 0 #f #f))

;;;; Construction ;;;;

;;; Construct an empty iset from an order predicate, explicit merger
;;; procedure, and optional elements.
(define (iset/merger precedes? merger . elements)
  (list->iset/merger precedes? merger elements))

;;; Construct an empty iset using the default iset-merger-right.
(define (iset precedes? . elements)
  (list->iset/merger precedes? iset-merger-right elements))

;;;; Set-wide queries ;;;;

(define (iset-empty? set)
  (btree-empty? (iset-root set)))

;;; iset-size and iset-precedes? were already defined as record
;;; accessors.

;;;; Set-theoretic operations ;;;;

(define (iset-xor left right)
  ;; xor is the simplest of the set-theoretic operations, as it never
  ;; involves a merger procedure or more than two sets. Our algorithm
  ;; is
  ;;
  ;; 1) Convert both sets' btrees to ordered lists.
  ;;
  ;; 2) Compute the xor of the lists with ordered-list-xor!, defined
  ;; below.
  ;;
  ;; 3) Convert the resulting ordered list back to a btree.
  ;;
  ;; Each of the three steps takes O(n) time, so the entire process
  ;; takes O(n) time. The three subsidiary procedures do all the hard
  ;; work; this procedure just glues them together.
  (let ((precedes? (iset-precedes? left)))
    (ordered-list->iset precedes?
			(ordered-list-xor! (iset->ordered-list left)
					   (iset->ordered-list right)
					   precedes?))))

(define (iset-difference left . rest)
  ;; Set difference is essentially the same, but may involve multiple
  ;; right-operand sets; we handle them with fold. Note that the
  ;; accumulated set is kept in ordered list format, and we only
  ;; convert back to a btree once after all the set-differencing is
  ;; done.
  (let ((precedes? (iset-precedes? left)))
    (ordered-list->iset precedes?
			(fold (lambda (right accum)
				(ordered-list-difference! accum
							  (iset->ordered-list right)
							  precedes?))
			      (iset->ordered-list left)
			      rest))))

;; The remaining two set operations (union and intersection) involve
;; both an arbitrary list of right-operands, and also a merger
;; procedure. They are identical except for using different list merge
;; procedures, so we create them procedurally.
(define (make-iset-operation merge-proc)
  (lambda (left . rest)
    (let ((precedes? (iset-precedes? left)))
      (ordered-list->iset precedes?
			  (fold (lambda (right accum)
				  (merge-proc accum
					      (iset->ordered-list right)
					      precedes?
					      iset-merger-right))
				(iset->ordered-list left)
				rest)))))

(define iset-union (make-iset-operation ordered-list-union!))
(define iset-intersection (make-iset-operation ordered-list-intersection!))

;;;; Element queries ;;;;

(define (iset-member? set x)
  (btree-search (iset-root set) x
		(const #f)
		(const #t)
		(iset-precedes? set)))

;;; iset-min and iset-max were already defined as record accessors.

;;;; Element operations ;;;;

(define (iset-update set x absent-proc present-proc)
  (let ((precedes? (iset-precedes? set))
	(root (iset-root set))
	(size (iset-size set)))

    (define (insert)
      (btree->iset/size precedes?
			(btree-insert root x precedes?)
			(add1 size)))

    (define (remove->btree)
      (btree-remove root x precedes?))
	
    (define (remove->iset)
      (btree->iset/size precedes?
			(remove->btree)
			(sub1 size)))


    (define (replace y)
      (btree->iset/size precedes?
			(btree-insert (remove->btree) y precedes?)
			size))

    (btree-search root x
		  (lambda ()
		    (absent-proc insert))
		  (lambda (match)
		    (present-proc match replace remove->iset))
		  precedes?)))

(define (iset-find set x absent-thunk)
  (iset-update set x
	       (lambda (insert)
		 (absent-thunk))
	       (lambda (match replace remove)
		 match)))

(define (iset-include set x)
  (iset-update set x
	       (lambda (insert)
		 (insert))
	       (const set)))

(define (iset-exclude set x)
  (iset-update set x
	       (const set)
	       (lambda (match replace remove)
		 (remove))))

;;;; Filter, fold, map ;;;;

(define (iset-filter predicate? set)
  (let* ((everything (btree->ordered-list (iset-root set)))
	 (filtered (filter predicate? everything))
	 (root (ordered-list->btree filtered)))
    (btree->iset (iset-precedes? set) root)))

(define (iset-fold proc base set)
  (btree-fold #t proc base (iset-root set)))

(define (iset-map/monotone proc set)
  (ordered-list->iset (iset-precedes? set)
		      (map proc (iset->ordered-list set))))

(define (iset-map proc set)
  (list->iset (iset-precedes? set)
	      (map proc (iset->ordered-list set))))

;;;; Conversion ;;;;

(define (iset->ordered-list set)
  (btree->ordered-list (iset-root set)))

(define (ordered-list->iset precedes? list)
  (btree->iset precedes? (ordered-list->btree list)))

(define (list->iset/merger precedes? merger list)
  (ordered-list->iset precedes? (list->ordered-list list precedes? merger)))

(define (list->iset precedes? list)
  (list->iset/merger precedes? iset-merger-right list))

;;;; Equality and subsets ;;;;

(define (make-transitive-relation binary-predicate)
  (lambda (left . rest)
    (and (fold (lambda (right left)
		 (and left
		      (binary-predicate left right)
		      right))
	       left
	       rest)
	 #t)))

(define (make-subset-predicate size-condition)
  (make-transitive-relation
   (lambda (left right)
     (and (size-condition (iset-size left) (iset-size right))
	  (null? (ordered-list-difference! (iset->ordered-list left)
					   (iset->ordered-list right)
					   (iset-precedes? left)))))))
  
(define iset<? (make-subset-predicate <))
(define iset<=? (make-subset-predicate <=))
(define iset=? (make-subset-predicate =))

(define iset>?
  (lambda args
    (not (apply iset<=? args))))

(define iset>=?
  (lambda args
    (not (apply iset<? args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IMAP
;;;;
;;;; We implement the map structure on top of iset in the same way
;;;; that association lists (alists) implement maps on top of
;;;; lists. That is, we store a set of pairs, where each pair stores
;;;; an association key in the car slot and the corresponding value in
;;;; the cdr slot. Then many imap operations are thin wrappers around
;;;; iset or list operations.
;;;;
;;;; One wrinkle is that the underlying iset's order predicate needs
;;;; to compare pairs according to their car slots (keys) only, but
;;;; clients of iset expect to provide predicates that operate on key
;;;; objects directly. We need to exercise some care in keeping these
;;;; two related procedures straight.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <imap>
  (make-imap precedes? pairs)
  imap?
  (precedes? imap-key-precedes?) ; takes key objects as arguments
  (pairs imap-pairs))

(define (make-pair-precedes? key-precedes?)
  (lambda (left-pair right-pair)
    (key-precedes? (car left-pair) (car right-pair))))

;;; Returns the procedure that takes two key-value pair objects as
;;; arguments.
(define (imap-pair-precedes? map)
  (iset-precedes? (imap-pairs map)))

(define (imap-replace-pairs map new-pairs)
  (make-imap (imap-key-precedes? map) new-pairs))

;;; A sentinel pair is a key-value pair that is equal to key for the
;;; purposes of the imap's order predicate, but has an undefined cdr
;;; (value). We use them as "dummy" objects to search for.
(define (sentinel-pair key)
  (cons key #f))

(define (make-imap/pairs map pairs)
  (make-imap (imap-key-precedes? map) pairs))

;;;; Map-wide queries ;;;;

;;; imap? was defined above

(define (imap-empty? map)
  (iset-empty? (imap-pairs map)))

(define (imap-size map)
  (iset-size (imap-pairs map)))

;;;; Element queries ;;;;

(define (imap-member? map key)
  (iset-member? (imap-pairs map) (sentinel-pair key)))

(define (make-map-map-iset pair-proc iset-proc)
  (lambda (map)
    (pair-proc (iset-proc (imap-pairs map)))))

(define imap-min-key (make-map-map-iset car iset-min))
(define imap-max-key (make-map-map-iset car iset-max))
(define imap-min-value (make-map-map-iset cdr iset-min))
(define imap-max-value (make-map-map-iset cdr iset-max))

;;;; Element operations ;;;;

(define (imap-update map key absent-proc present-proc)
  (let ((pairs (imap-pairs map)))
    (iset-update pairs
		 (sentinel-pair key)
		 (lambda (insert-sentinel-pair)
		   (absent-proc (lambda (new-value)
				  (make-imap/pairs map
						   (iset-include pairs (cons key new-value))))))
		 (lambda (match-pair replace-pair remove-pair)
		   (present-proc (car match-pair)
				 (cdr match-pair)
				 (lambda (new-value) ; replace
				   (make-imap/pairs map
						    (replace-pair (cons key new-value))))
				 (lambda () ; remove
				   (make-imap/pairs map (remove-pair))))))))

(define (imap-find map key absent-thunk)
  (imap-update map key
	       (lambda (insert)
		 (absent-thunk))
	       (lambda (key-match match-value replace remove)
		 match-value)))

(define (imap-include map key value)
  (imap-update map key
	       (lambda (insert)
		 (insert value))
	       (lambda (key-match match-value replace remove)
		 (replace value))))

(define (imap-exclude map key)
  (imap-update map key
	       (lambda (insert)
		 map)
	       (lambda (key-match match-value replace remove)
		 (remove))))

;;;; Range queries ;;;;

;;;; Filter, fold, map ;;;;

(define (imap-filter proc map)
  (imap-replace-pairs map
		      (iset-filter (lambda (pair)
				     (proc (car pair) (cdr pair)))
				   (imap-pairs map))))

(define (imap-fold proc base map)
  (iset-fold (lambda (pair accum)
	       (proc (car pair) (cdr pair) accum))
	     base
	     (imap-pairs map)))

(define (make-imap-map iset-map-proc)
  (lambda (proc map)
    (imap-replace-pairs map
			(iset-map-proc (lambda (pair)
					 (let-values (((key value) (proc (car pair) (cdr pair))))
					   (cons key value)))
				       (imap-pairs map)))))

(define imap-map/monotone (make-imap-map iset-map/monotone))
(define imap-map (make-imap-map iset-map))

(define (imap-map-values proc map)
  (imap-map/monotone (lambda (key value)
		       (values key (proc key value)))
		     map))

;;;; Conversion ;;;;

(define (imap->ordered-alist map)
  (iset->ordered-list (imap-pairs map)))

(define (imap-values imap)
  (map cdr (imap->ordered-alist imap)))

(define (imap-keys imap)
  (ordered-list->iset (imap-key-precedes? imap)
		      (map car (imap->ordered-alist imap))))

(define (ordered-alist->imap precedes? alist)
  (make-imap precedes?
	     (ordered-list->iset (make-pair-precedes? precedes?) alist)))

(define (alist->imap precedes? alist)
  (make-imap precedes?
	     (list->iset (make-pair-precedes? precedes?) alist)))

;;;; Equality ;;;;

(define imap=? 
  (make-transitive-relation 
   (lambda (left right)
     (and (= (imap-size left) (imap-size right))
	  (equal? (imap->ordered-alist left)
		  (imap->ordered-alist right))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MISCELLANEOUS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

;;; A variant of set-cdr! that also returns the pair that was
;;; modified.
(define (set-cdr/return! pair cdr)
  (set-cdr! pair cdr)
  pair)

;;; Identity function.
(define (id x)
  x)

;;; Returns a function that always returns x regardless of its
;;; arguments.
(define (const x)
  (lambda args
    x))

(define (flip proc)
  (lambda (left right)
    (proc right left)))
