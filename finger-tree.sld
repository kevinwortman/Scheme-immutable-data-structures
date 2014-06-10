;; finger-tree.sld
;;
;; This Scheme library implements a persistent, pure-functional
;; unlabled 2-3 finger tree. The data structure is intended to serve
;; as a queue or sequence, but since it is unlabeled cannot perform
;; efficient search tree or priority queue operations.
;;
;; For background on 2-3 finger trees, see
;;
;; [1] "Finger Trees: a simple general-purpose data structure", by
;;     Ralf Hinze and Ross Paterson
;;     https://www.cs.ox.ac.uk/people/ralf.hinze/publications/FingerTrees.pdf
;;
;; [3] Wikipedia page
;;     http://en.wikipedia.org/wiki/Finger_tree
;;
;; The library depends on R7RS-small, SRFIs 1, and 26, and the util
;; library in this repository.
;;
;; See the file README for general remarks and LICENSE for license
;; information.

(define-library (finger-tree)
  (import
   (scheme base)
   (srfi 1)
   (srfi 26)
   (util))
  (export
   
   make-finger-tree
   finger-tree?
   finger-tree-empty?
   finger-tree-length

   finger-tree-front
   finger-tree-back

   finger-tree-push-front
   finger-tree-push-back

   finger-tree-pop-front
   finger-tree-pop-back

   finger-tree-append

   finger-tree-filter
   finger-tree-fold
   finger-tree-map

   finger-tree->list
   list->finger-tree)

  (begin

    ;; A finger tree may be either
    ;;
    ;; - empty;
    ;; - a single element; or
    ;; - a *deep node* comprised of
    ;;   left: a list of 1-4 elements at the front of the tree,
    ;;   spine: a nested finger tree, and
    ;;   right: a list of 1-4 elements at the back of the tree.
    ;;
    ;; left and right are called *fingers*, since they may be thought
    ;; of as pointers inside the structure of a conventional search
    ;; tree, hence the name *finger tree.*
    ;;
    ;; The spine is another finger tree; the type of each element of
    ;; the spine tree is a *node*, and a node is a *list of 2-3*
    ;; elements of the outer tree. So if the element type of a
    ;; first-level tree is a number, then the element type of a nested
    ;; second-level tree is a list of 2-3 numbers; the element type of
    ;; a nested third-level tree is a list of 2-3 lists of 2-3
    ;; numbers; and so on. This is why finger trees are so efficient:
    ;; every time you go one level deeper, a push/pop operation
    ;; affects exponentially more elements from the outermost
    ;; list. This is also where the "2-3" comes in.
    ;;
    ;; We store the right finger in reverse order, so the back element
    ;; may be found quickly with (car r).

    (define-singleton empty empty?)

    (define-record-type <single>
      (make-single x)
      single?
      (x single-x))

    (define-record-type <deep>
      (make-deep l s r)
      deep?
      (l deep-l)
      (s deep-s)
      (r deep-r))

    ;; Dispatch based on the shape of a tree.
    (define-syntax case/tree
      (syntax-rules (empty single deep)
	((case/tree TREE
	   (empty        EMPTY-BODY)
	   (single (X)   SINGLE-BODY)
	   (deep (L S R) DEEP-BODY))
	 (let ((t TREE))
	   (cond
	    ((deep? t)
	     (let ((L (deep-l t))
		   (S (deep-s t))
		   (R (deep-r t)))
	       DEEP-BODY))
	    ((single? t)
	     (let ((X (single-x t)))
	       SINGLE-BODY))
	    (else
	     EMPTY-BODY))))))

    ;; Create an empty finger tree. O(1) time.
    (define make-finger-tree (constant-thunk empty))

    ;; Type predicate. O(1) time.
    (define (finger-tree? x)
      (or (deep? x)
	  (single? x)
	  (empty? x)))

    ;; Empty predicate. O(1) time.
    (define finger-tree-empty? empty?)

    ;; Return the number of elements in the tree. O(n) time.
    (define (finger-tree-length tree)
      (finger-tree-fold (lambda (_ sum)
			  (add1 sum))
			0
			tree))

    ;; Return the element at the front of the tree. It is an error for
    ;; the tree to be empty. O(1) time.
    (define (finger-tree-front tree)
      (case/tree tree
        (empty        (error "finger-tree-front: empty tree"))
	(single (x)   x)
	(deep (l s r) (first l))))

    ;; Return the element at the back of the tree. It is an error for
    ;; the tree to be empty. O(1) time.
    (define (finger-tree-back tree)
      (case/tree tree
        (empty        (error "finger-tree-front: empty tree"))
	(single (x)   x)
	(deep (l s r) (first r))))

    ;; Insert q at the front of tree and return the result as a new
    ;; tree object. O(1) amortized and O(log n) worst case time.
    (define (finger-tree-push-front tree q)
      (case/tree tree
	;; empty becomes single
        (empty      (make-single q))

	;; single becomes deep with 1-element fingers and no spine
	(single (x) (make-deep (list q) empty (list x)))

	(deep (l s r)
	 (if (< (length l) 4)
	     ;; if there's room, just insert q at the front of l
	     (make-deep (cons q l) s r)

	     ;; otherwise l has exactly 4 elements; consider the last
	     ;; 3 a node and push them into the spine, and form a new
	     ;; finger with q and the 1 leftover element
	     (make-deep (list q (first l))
			(finger-tree-push-front s (cdr l))
			r)))))

    ;; Insert q at the back of tree and return the result as a new
    ;; tree object. O(1) amortized and O(log n) worst case time.
    (define (finger-tree-push-back tree q)
      (case/tree tree
        (empty      (make-single q))
	(single (x) (make-deep (list x) empty (list q)))
	(deep (l s r)
	 (if (< (length r) 4)
	     (make-deep l s (cons q r))
	     (make-deep l
			(finger-tree-push-back s (reverse (cdr r)))
			(list q (first r)))))))

    ;; Remove the front element of tree and return the result as a new
    ;; tree object. It is an error for tree to be empty. O(1)
    ;; amortized and O(log n) worst case time.
    (define (finger-tree-pop-front tree)
      (case/tree tree
	;; underflow
        (empty      (error "finger-tree-pop-front: empty tree"))

	;; single becomes empty
	(single (x) empty)

	(deep (l s r)
          (cond
	   ;; If l has at least 2 elements, we can just drop the
	   ;; first.
	   ((length-at-least? l 2)
	    (make-deep (cdr l) s r))

	   ;; Otherwise l is effectively empty after dropping its only
	   ;; element. If the spine is nonempty we can pull out a node
	   ;; and use it as a new left finger.
	   ((not (finger-tree-empty? s))
	    (make-deep (finger-tree-front s)
		       (finger-tree-pop-front s)
		       r))

	   ;; l and s are both empty; if r has at least 2 elements,
	   ;; move the last one over to l.
	   ((length-at-least? r 2)
	    (let-values (((r l) (split-at r (sub1 (length r)))))
	      (make-deep l empty r)))
	    
	   ;; If we got here, l and s are empty, and r has only 1
	   ;; element; we shrink down to a single tree.
	   (else
	    (make-single (first r)))))))

    ;; Remove the back element of tree and return the result as a new
    ;; tree object. It is an error for tree to be empty. O(1)
    ;; amortized and O(log n) worst case time.
    (define (finger-tree-pop-back tree)
      (case/tree tree
        (empty      (error "finger-tree-pop-back: empty tree"))
	(single (x) empty)
	(deep (l s r)
          (cond
	   ((length-at-least? r 2)
	    (make-deep l s (cdr r)))
	   ((not (finger-tree-empty? s))
	    (make-deep l
		       (finger-tree-pop-back s)
		       (reverse (finger-tree-back s))))
	   ((length-at-least? l 2)
	    (let-values (((l r) (split-at l (sub1 (length l)))))
	      (make-deep l empty r)))
	   (else
	    (make-single (first l)))))))

    ;; Return a new tree containing the elements of left followed by
    ;; the elements of right. O(log n) time.
    (define (finger-tree-append left right)
      (cond
       ;; Trivial cases when one or both trees are non-deep.
       ((empty? left)
	right)
       ((empty? right)
	left)
       ((single? left)
	(finger-tree-push-front right (single-x left)))
       ((single? right)
	(finger-tree-push-back left (single-x right)))
       (else
	;; We have two deep nodes that need to be smooshed into one:
	;;
	;; left-l left-s left-r    right-l right-s right-r
	;;
	;; We will form a new node, reusing left-l and right-r and
	;; *melding* everything else into one unified spine.
	;;
	;; left-l (left-s left-r right-l right-s) right-r
	;;
	;; To do that, we combine left-r and right-l into a list of
	;; *loose* elements, group them into 2-nodes and 3-nodes, and
	;; push these nodes into left-s.
	;;
	;; This is why fingers have 1-4 elements: between left-r and
	;; right-l we have two fingers, for a total of 2-8
	;; elements. Any length between 2 and 8 may be split into a
	;; combination of one, two, or three 2-nodes and 3-nodes.
	;;
	;; After dispensing with all the loose elements, we have
	;;
	;; left-l left-s right-s right-r .
	;;
	;; We recursively append left-s with right-s, producing a
	;; melded spine, and we're done:
	;;
	;; left-l (finger-tree-append left-s right-s) right-r .
	(make-deep (deep-l left)

		   (let meld ((spine (deep-s left))
			      (loose (append (reverse (deep-r left))
					     (deep-l right))))
		     (case (length loose)
                       ((0) ; no loose elements; recursively append
			    ; the two spines into one
			(finger-tree-append spine (deep-s right)))
		       ((2 4) ; peel of a 2-node
			(meld (finger-tree-push-back spine (take loose 2))
			      (drop loose 2)))
		       (else ; peel off a 3-node
			(meld (finger-tree-push-back spine (take loose 3))
			      (drop loose 3)))))

		   (deep-r right)))))

    ;; Conventional filter. O(n) time.
    (define (finger-tree-filter f tree)
      (finger-tree-fold (lambda (x result)
			  (if (f x)
			      (finger-tree-push-back result x)
			      result))
			(make-finger-tree)
			tree))

    ;; Conventional fold. O(n) time.
    (define (finger-tree-fold f knil tree)
      (case/tree tree
        (empty      knil)
	(single (x) (f x knil))
	(deep (l d r)
	  (fold f
		(finger-tree-fold (lambda (node knil)
				    (fold f knil node))
				  (fold f knil l)
				  d)
		(reverse r)))))

    ;; Conventional map. O(n) time.
    (define (finger-tree-map f tree)
      (case/tree tree
        (empty      empty)
	(single (x) (make-single (f x)))
	(deep (l s r)
          (make-deep (map f l)
		     (finger-tree-map (lambda (node)
					(map f node))
				      s)
		     (map f r)))))

    ;; Convert to a list. O(n) time.
    (define (finger-tree->list tree)
      (reverse! (finger-tree-fold cons '() tree)))

    ;; Convert from a list. O(n) time.
    (define (list->finger-tree lst)
      (fold (flip finger-tree-push-back) (make-finger-tree) lst))

    ))
