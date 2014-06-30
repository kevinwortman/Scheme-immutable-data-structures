;; tree23.sld
;;
;; This Scheme library implements a persistent, pure-functional 2-3
;; search tree. For background on 2-3 trees, see
;;
;; [1] Lyn Turbak's lecture notes
;;     http://cs.wellesley.edu/~cs230/spring07/2-3-trees.pdf
;;
;; [2] Wikipedia page
;;     http://en.wikipedia.org/wiki/2%E2%80%933_tree
;;
;; The algorithm for building a tree from an ordered list in O(n) time
;; uses ideas from
;;
;; [3] "Purely functional 1-2 brother trees" by Ralf Hinze
;;      http://dl.acm.org/citation.cfm?id=1735547
;;
;; The library depends on R7RS-small, SRFIs 1, 26, and 114, and the
;; selector and util libraries in this repository.
;;
;; See the file README for general remarks and LICENSE for license
;; information.

(define-library (tree23)
  (import
   (comparators)
   (scheme base)
   (scheme write)
   (selector)
   (srfi 1)
   (srfi 26)
   (util))
  
  (export

   make-tree23
   tree23?

   tree23-empty?
   tree23-size
   tree23-min
   tree23-max

   tree23-search
   tree23-insert
   tree23-delete

   make-tree23-iter
   tree23-iter-empty?
   tree23-iter-peek
   tree23-iter-pop

   make-tree23-scaffold
   tree23-scaffold-insert-max
   tree23-scaffold-finish

   tree23-filter
   tree23-fold
   tree23-map
   tree23-map/nondecreasing

   tree23->ordered-list
   ordered-list->tree23
   list->tree23

   tree23-range

   tree23-display
   tree23-validate)

  (begin

    ;; A 2-3 tree is either
    ;;
    ;; - empty;
    ;;
    ;; - a *binary* node with a left child l, element x, and right
    ;;   child r, in binary search tree order; or
    ;;
    ;; - a *trinary* node with a left child l, left element x, middle
    ;;   child m, right element y, and right child r, all in binary
    ;;   search tree order.
    ;;
    ;; Further, a 2-3 tree has the property that all empty nodes are
    ;; at the same depth. This guarantees that the height of any 2-3
    ;; tree is O(log n).

    (define-singleton empty empty?)

    (define-record-type <binary>
      (make-binary l x r)
      binary?
      (l binary-l)
      (x binary-x)
      (r binary-r))

    (define-record-type <trinary>
      (make-trinary l x m y r)
      trinary?
      (l trinary-l)
      (x trinary-x)
      (m trinary-m)
      (y trinary-y)
      (r trinary-r))

    ;; Create an empty 2-3 tree. O(1) time.
    (define make-tree23 (constant-thunk empty))

    ;; Create a leaf. A leaf is a binary or trinary node with all
    ;; empty children. O(1) time.
    (define make-binary/leaf (cute make-binary empty <> empty))
    (define make-trinary/leaf (cute make-binary empty <> empty <> empty))

    ;; Type predicate. O(1) time.
    (define (tree23? x)
      (or (binary? x)
	  (trinary? x)
	  (empty? x)))

    ;; Is the given tree empty? O(1) time.
    (define tree23-empty? empty?)

    ;; Syntax to bind node fields.
    (define-syntax let/binary
      (syntax-rules ()
	((let/binary ((L X R) TREE)
           BODY ...)
	 (let* ((t TREE)
		(L (binary-l t))
		(X (binary-x t))
		(R (binary-r t)))
	   BODY ...))))
    (define-syntax let/trinary
      (syntax-rules ()
	((let/trinary ((L X M Y R) TREE)
           BODY ...)
	 (let* ((t TREE)
		(L (trinary-l t))
		(X (trinary-x t))
		(M (trinary-m t))
		(Y (trinary-y t))
		(R (trinary-r t)))
	   BODY ...))))

    ;; Dispatch based on the type of a tree. L, X, and R are bound in
    ;; BINARY-BODY; L, X, M, Y, R are bound in TRINARY-BODY.
    (define-syntax case/tree
      (syntax-rules (empty binary trinary)
	((case/tree ((L X M Y R) TREE)
	   (empty    EMPTY-BODY ...)
	   (binary   BINARY-BODY ...)
	   (trinary  TRINARY-BODY ...))

	 (let ((t TREE))
	   (cond
	    ((binary? t)
	     (let/binary ((L X R) t)
	       BINARY-BODY ...))
	    ((trinary? t)
	     (let/trinary ((L X M Y R) t)
	       TRINARY-BODY ...))
	    (else
	     EMPTY-BODY ...))))))

    ;; Return the number of elements in tree. O(n) time.
    (define (tree23-size tree)
      (case/tree ((l x m y r) tree)
        (empty   0)
	(binary  (+ 1
		    (tree23-size l)
		    (tree23-size r)))
	(trinary (+ 2
		    (tree23-size l)
		    (tree23-size m)
		    (tree23-size r)))))

    ;; Return the minimum/maximum element in the tree. It is an error
    ;; for the tree to be empty. O(log n) time.
    (define-syntax-rule (define-extreme IDENTIFIER L X Y R CHILD TRINARY-ELEMENT)
      ;; minimim/maximum are symmetric so we use one macro to define
      ;; either procedure.
      (define (IDENTIFIER tree)
	(case/tree ((L X _ Y R) tree)
	  (empty   (error "tree23-min/max: unexpected empty tree"))
	  (binary  (if (empty? CHILD)
		       x
		       (IDENTIFIER CHILD)))
	  (trinary (if (empty? CHILD)
		       TRINARY-ELEMENT
		       (IDENTIFIER CHILD))))))

    (define-extreme tree23-min l x y r l x)
    (define-extreme tree23-max l x y r r y)

    ;; Dispatch based on the type of a tree *and* the order of a query
    ;; value relative to the tree's stored elements.
    (define-syntax case/tree-query
      (syntax-rules (empty
		     binary:l binary:x binary:r
		     trinary:l trinary:x trinary:m trinary:y trinary:r)
        ((case/tree-query ((L X M Y R) TREE COMPARATOR QUERY)
	   (empty     EMPTY-BODY)
	   (binary:l  B:L)
	   (binary:x  B:X)
	   (binary:r  B:R)
	   (trinary:l T:L)
	   (trinary:x T:X)
	   (trinary:m T:M)
	   (trinary:y T:Y)
	   (trinary:r T:R))

	 (let ((compare (cute comparator-compare COMPARATOR QUERY <>)))
	   (case/tree ((L X M Y R) TREE)
	     (empty   EMPTY-BODY)
	     (binary  (if3 (compare X)
			   B:L
			   B:X
			   B:R))
	     (trinary (if3 (compare X)
			   T:L
			   T:X
			   (if3 (compare Y)
				T:M
				T:Y
				T:R))))))))

    ;; Search for element q in tree; calls (match-proc match), where
    ;; match is an element in tree that is equal to q according to
    ;; comparator; or calls (missing-proc) if no such match
    ;; exists. O(log n) time.
    (define (tree23-search comparator match-proc missing-proc tree q)
      ;; alias
      (define match? (cute =? comparator q <>))

      (let recurse ((tree tree))
	(case/tree-query ((l x m y r) tree comparator q)
	  (empty     (missing-proc))
	  (binary:l  (recurse l))
	  (binary:x  (match-proc x))
	  (binary:r  (recurse r))
	  (trinary:l (recurse l))
	  (trinary:x (match-proc x))
	  (trinary:m (recurse m))
	  (trinary:y (match-proc y))
	  (trinary:r (recurse r)))))

    ;; Return a tree that contains q. In the event that tree already
    ;; contains an element match equal to q according to comparator,
    ;; the result of (selector match q), which must be equal to match
    ;; and q, is inserted. O(log n) time.
    (define (tree23-insert comparator selector tree q)
      ;; aliases
      (define compare (cute comparator-compare comparator q <>))
      (define select (cute selector-select selector <> q))

      ;; We do this with recursion and continuation passing. down must
      ;; insert q into tree, then call either
      ;;
      ;;  (balanced node)
      ;;
      ;; where node is a new tree of any type, containing q, otherwise
      ;; equivalent to tree, and obeying the height invariant; or call
      ;;
      ;;  (unbalanced node)
      ;;
      ;; where node is a binary tree that contains q but is 1 level
      ;; higher than any of its siblings.
      ;;
      ;; This reduces to several straightforward, perhaps tedious
      ;; special cases. See pp. 3-6 of Turbak's notes.
      ;;
      ;; Using continuations this way allows us to avoid consing
      ;; temporary nodes that get garbage collected immediately. It
      ;; also allows us to avoid the need for any dispatching during
      ;; the upward phase. We do all the decision-making on the way
      ;; down, and the way back up is a chain of fast tail calls with
      ;; no branches, that reconstruct and repairs the tree.
      (let down ((tree tree)
		 (balanced identity)
		 (unbalanced identity))
	(case/tree-query ((l x m y r) tree comparator q)
          (empty     (unbalanced (make-binary/leaf q)))
	  (binary:l  (down l
			   (lambda (l)
			     (balanced (make-binary l x r)))
			   (lambda (l)
			     (let/binary ((l w m) l)
			       (balanced (make-trinary l w m x r))))))
	  (binary:x  (balanced (make-binary l (select x) r)))
	  (binary:r  (down r
			   (lambda (r)
			     (balanced (make-binary l x r)))
			   (lambda (r)
			     (let/binary ((m w r) r)
			       (balanced (make-trinary l x m w r))))))
	  (trinary:l (down l
			   (lambda (l)
			     (balanced (make-trinary l x m y r)))
			   (lambda (l)
			     (unbalanced (make-binary l x (make-binary m y r))))))
	  (trinary:x (balanced (make-trinary l (select x) m y r)))
	  (trinary:m (down m
			   (lambda (m)
			     (balanced (make-trinary l x m y r)))
			   (lambda (m)
			     (let/binary ((b w c) m)
			       (unbalanced (make-binary (make-binary l x b)
							w
							(make-binary c y r)))))))
	  (trinary:y (balanced (make-trinary l x m (select y) r)))
	  (trinary:r (down r
			   (lambda (r)
			     (balanced (make-trinary l x m y r)))
			   (lambda (r)
			     (unbalanced (make-binary (make-binary l x m) y r))))))))

    ;; Return a tree that does not contain q. In the event that q is
    ;; not an element of tree, calls (missing-proc). O(log n) time.
    (define (tree23-delete comparator missing-proc tree q)
      ;; We use the same approach here as in tree23-insert. down must
      ;; remove q from tree, then call either
      ;;
      ;;  (balanced node)
      ;;
      ;; where node is equivalent to tree, does not contain q, and
      ;; honors the height invariant; or call
      ;;
      ;;  (unbalanced node)
      ;;
      ;; where node does not contain q but is one level lower than any
      ;; of its siblings.
      ;;
      ;; Note that this time q is one down's arguments; this is
      ;; necessary to handle the case of deleting an element out of a
      ;; non-terminal node (i.e. a node with non-empty children). In
      ;; this case we use the standard trick of replacing the
      ;; offending element with its inorder successor, then deleting
      ;; the redundant copy of the successor out of the right (or
      ;; middle) subtree. That means that the value of q may need to
      ;; be rebound on the way down.
      ;;
      ;; There are more cases to contend with than in insertion; see
      ;; pp. 8-10 of Turbak's notes.
      (let down ((tree tree)
		 (q q)
		 (balanced identity)
		 (unbalanced identity))

	;; Some of the cases are repeated, so we define a procedure
	;; for each.

	(define (binary-left l x r q)
	  (down l q
		(lambda (l)
		  (balanced (make-binary l x r)))
		(lambda (l)
		  (if (binary? r)
		      (let/binary ((rl rx rr) r) ; case 1, left
		        (unbalanced (make-trinary l x rl rx rr)))
		      (let/trinary ((rl rx rm ry rr) r) ; case 2, left
			(balanced (make-binary (make-binary l x rl)
					       rx
					       (make-binary rm ry rr))))))))

	(define (binary-right l x r q)
	  (down r q
		(lambda (r)
		  (balanced (make-binary l x r)))
		(lambda (r)
		  (if (binary? l)
		      (let/binary ((ll lx lr) l) ; case 1, right
		        (unbalanced (make-trinary ll lx lr x r)))
		      (let/trinary ((ll lx lm ly lr) l) ; case 2, right
		        (balanced (make-binary (make-binary ll lx lm)
					       ly
					       (make-binary lr x r))))))))

	(define (trinary-left l x m y r q)
	  (down l q
		(lambda (l)
		  (balanced (make-trinary l x m y r)))
		(lambda (l)
		  (if (binary? m)
		      (let/binary ((ml mx mr) m) ; case 3a, left
		        (balanced (make-binary (make-trinary l x ml mx mr) y r)))
		      (let/trinary ((ml mx mm my mr) m) ; case 4a, left
                        (balanced (make-trinary (make-binary l x ml)
						mx
						(make-binary mm my mr)
						y
						r)))))))

	(define (trinary-middle l x m y r q)
	  (down m q
		(lambda (m)
		  (balanced (make-trinary l x m y r)))
		(lambda (m)
		  (if (binary? r)
		      (let/binary ((rl rx rr) r)  ; case 3b, left
                        (balanced (make-binary l x (make-trinary m y rl rx rr))))
		      (let/trinary ((rl rx rm ry rr) r) ; case 4b, left
                        (balanced (make-trinary l
						x
						(make-binary m y rl)
						rx
						(make-binary rm ry rr))))))))

	(define (trinary-right l x m y r q)
	  (down r q
		(lambda (r)
		  (balanced (make-trinary l x m y r)))
		(lambda (r)
		  (if (binary? m)
		      (let/binary ((ml mx mr) m) ; case 3b, right
		        (balanced (make-binary l x (make-trinary ml mx mr y r))))
		      (let/trinary ((ml mx mm my mr) m) ; case 4b, right
                        (balanced (make-trinary l
						x
						(make-binary ml mx mm)
						my
						(make-binary mr y r))))))))

	(case/tree-query ((l x m y r) tree comparator q)
	  (empty     (missing-proc))
	  (binary:l  (binary-left l x r q))
	  (binary:x  (if (empty? r)
			 (unbalanced empty)
			 (let ((successor (tree23-min r)))
			   (binary-right l successor r successor))))
	  (binary:r  (binary-right l x r q))
	  (trinary:l (trinary-left l x m y r q))
	  (trinary:x (if (empty? m)
			 (balanced (make-binary/leaf y))
			 (let ((successor (tree23-min m)))
			   (trinary-middle l successor m y r successor))))
	  (trinary:m (trinary-middle l x m y r q))
	  (trinary:y (if (empty? r)
			 (balanced (make-binary/leaf x))
			 (let ((successor (tree23-min r)))
			   (trinary-right l x m successor r successor))))
	  (trinary:r (trinary-right l x m y r q)))))
					     
    ;; tree23-iter
    ;;
    ;; Tree element iterator.
    ;;
    ;; A tree23-iter is represented as a stack data structure, stored
    ;; as a list where each element is one of
    ;;
    ;; - a binary node, whose x is unconsumed;
    ;;
    ;; - at-x, containing a trinary node x and y are both unconsumed;
    ;;   or
    ;;
    ;; - at-y, containing a trinary node whose x is consumed and y is
    ;;   unconsumed.

    (define-record-type <at-x>
      (make-at-x tree)
      at-x?
      (tree at-x-tree))

    (define-record-type <at-y>
      (make-at-y tree)
      at-y?
      (tree at-y-tree))

    ;; Helper procedure to push all the leftmost descendants of tree
    ;; onto iter.
    (define (push-left iter tree)
      (case/tree ((l x m y r) tree)
	(empty   iter)
	(binary  (push-left (cons tree iter) l))
	(trinary (push-left (cons (make-at-x tree) iter) l))))
                     
    ;; Return a new iterator at the beginning of the tree. O(log n)
    ;; time.
    (define (make-tree23-iter tree)
      (push-left '() tree))

    ;; Return true iff iter is empty. O(1) time.
    (define tree23-iter-empty? null?)

    ;; Return the element that iter points to. It is an error for iter
    ;; to be empty. O(1) time.
    (define (tree23-iter-peek iter)
      (let ((top (car iter)))
	(cond
	 ((binary? top) (binary-x top))
	 ((at-x? top)   (trinary-x (at-x-tree top)))
	 (else          (trinary-y (at-y-tree top))))))

    ;; Move iter to the next element. It is an error for iter to be
    ;; empty. O(1) amortized and O(log n) worst case time.
    (define (tree23-iter-pop iter)
      (let-values (((top rest) (car+cdr iter)))
	(cond
	 ((binary? top) (push-left rest (binary-r top)))
	 ((at-x? top)   (let ((tree (at-x-tree top)))
			  (push-left (cons (make-at-y tree) rest)
				     (trinary-m tree))))
	 (else          (let ((tree (at-y-tree top)))
			  (push-left rest
				     (trinary-r tree)))))))

    ;; tree23-scaffold
    ;;
    ;; A *scaffold* is a structure that may be used to construct a
    ;; tree23 from an ordered (e.g. sorted) sequence of elements in
    ;; O(n) time. This is significantly faster than calling
    ;; tree23-insert n times, which has O(n log n) asymptotic
    ;; complexity and significantly worse constant factors. A scaffold
    ;; is a partially formed tree23, and may be *finished* to convert
    ;; it into a fully functional tree23 object.
    ;;
    ;; A scaffold is a 2-3 tree, but rather than being represented by
    ;; a reference to the root node, it is represented by a reference
    ;; to the *rightmost leaf*. The nodes along the right *spine*,
    ;; from the rightmost leaf up to the root, have *up* pointers but
    ;; do not have *right* pointers as usual. This allows us to insert
    ;; a new maximum element very quickly, since this element must be
    ;; the new rightmost leaf; we need only rebalance along the
    ;; spine. Finishing a scaffold involves flipping all the nodes
    ;; along the right spine to have proper right pointers instead of
    ;; up pointers.

    ;; It would be cleaner to define new record types for binary and
    ;; trinary right-spine nodes; but they are almost identical to
    ;; ordinary nodes, so we cut corners and reuse <binary> and
    ;; <trinary> as spine nodes, with the convention that the r field
    ;; really means a u (up) field.
    (define binary-u binary-r)
    (define trinary-u trinary-r)

    ;; Return a new empty scaffold. O(1) time.
    (define make-tree23-scaffold (constant-thunk empty))

    ;; Return a new scaffold that contains q. q must be greater than
    ;; any element already in scaf. Note that this procedure does not
    ;; take a comparator argument so has no way of checking. O(1)
    ;; amortized and O(log n) worst case time.
    (define (tree23-scaffold-insert-max scaf q)
      ;; These are the same cases as used in the more general
      ;; insertion algorithm. However they are much simpler since we
      ;; start out knowing where to insert q (the rightmost leaf), and
      ;; there are fewer cases since unbalanced subtrees are only ever
      ;; a right child of their parent. An element only goes as high
      ;; as its height in the completed tree, so a standard argument
      ;; shows that the amortized cost of each insertion is O(1).
      (let up ((l empty)
	       (x q)
	       (u scaf))
	(case/tree ((ul ux um uy uu) u)
	  (empty   (make-binary l x empty))
	  (binary  (make-trinary ul ux l x uu))
	  (trinary (make-binary l x (up (make-binary ul ux um) uy uu))))))

    ;; Return a tree23 object storing the elements of scaf. O(log n)
    ;; time.
    (define (tree23-scaffold-finish scaf)
      ;; We run up the right spine of the tree, replacing each spine
      ;; node with an up pointer, with an ordinary node with a right
      ;; pointer.
      (let up ((scaf scaf)
	       (tree empty))
	(case/tree ((l x m y u) scaf)
	  (empty   tree)
	  (binary  (up u (make-binary l x tree)))
	  (trinary (up u (make-trinary l x m y tree))))))

    ;; Conventional fold. O(n) time.
    (define (tree23-fold f knil tree)
      (let recurse ((tree tree)
		    (knil knil))
	(case/tree ((l x m y r) tree)
          (empty   knil)
	  (binary  (recurse r (f x (recurse l knil))))
	  (trinary (recurse r (f y (recurse m (f x (recurse l knil)))))))))

    ;; Return a tree with only those elements for which f returns
    ;; true. O(n) time.
    (define (tree23-filter f tree)
      (tree23-scaffold-finish
       (tree23-fold (lambda (x scaf)
		      (if (f x)
			  (tree23-scaffold-insert-max scaf x)
			  scaf))
		    (make-tree23-scaffold)
		    tree)))

    ;; Return a tree containing (f x) for all x in tree. f must be
    ;; nondecreasing, which guarantees that f does not change the
    ;; relative ordering of elements, so the structure of the original
    ;; tree may be reused. O(n) time.
    (define (tree23-map/nondecreasing f tree)
      (let recurse ((tree tree))
        (case/tree ((l x m y r) tree)
	  (empty   empty)
	  (binary  (make-binary (recurse l)
				(f x) 
				(recurse r)))
	  (trinary (make-trinary (recurse l)
				 (f x)
				 (recurse m)
				 (f y)
				 (recurse r))))))

    ;; Return a tree containing (f x) for all x in tree. There is no
    ;; constraint on f's behavior, so a new tree structure must be
    ;; built from scratch. O(n log n) time.
    (define (tree23-map f comparator selector tree)
      (tree23-fold (lambda (x result)
		     (tree23-insert comparator selector result (f x)))
		   (make-tree23)
		   tree))

    ;; Return a list containing all elements of tree in order. O(n)
    ;; time.
    (define (tree23->ordered-list tree)
      ;; Note: we traverse the tree backwards in right-to-left order,
      ;; consing elements to the start of lst; the two reversals
      ;; cancel each other out.
      (let recurse ((tree tree)
		    (lst '()))
	(case/tree ((l x m y r) tree)
	  (empty   lst)
	  (binary  (recurse l (cons x (recurse r lst))))
	  (trinary (recurse l (cons x (recurse m (cons y (recurse r lst)))))))))

    ;; Return a tree23 containing the elements of lst. It is an error
    ;; for lst to be out of order. Note that this procedure does not
    ;; take a comparator argument so has no way of checking. O(n)
    ;; time.
    (define (ordered-list->tree23 lst)
      (tree23-scaffold-finish
       (fold (flip tree23-scaffold-insert-max)
	     (make-tree23-scaffold)
	     lst)))

    ;; Return a tree23 containing the elements of lst. O(n log n)
    ;; time.
    (define (list->tree23 comparator selector lst)
      (fold (flip (cute tree23-insert comparator selector <> <>))
	    (make-tree23)
	    lst))

    ;; Return a tree23 containing all elements x of tree that are not
    ;; too-small? nor too-large? . too-small? and too-large? must
    ;; define an interval in the set of tree elements. O(k + log n)
    ;; time, where k is the number of elements returned.
    (define (tree23-range too-small? too-large? tree)
      (tree23-scaffold-finish
       (let recurse ((tree tree)
		     (scaf (make-tree23-scaffold)))

	 (define insert (flip tree23-scaffold-insert-max))

	 (define (insert-binary l x r scaf)
	   (recurse r (insert x (recurse l scaf))))

	 (define (insert-trinary l x m y r scaf)
	   (recurse r (insert y (insert-binary l x m scaf))))

	 (case/tree ((l x m y r) tree)
	   (empty   scaf)
	   (binary  (cond
		     ((too-small? x) (recurse r scaf))
		     ((too-large? x) (recurse l scaf))
		     (else           (insert-binary l x r scaf))))
	   (trinary (cond
		     ((too-large? x) (recurse l scaf))
		     ((too-small? y) (recurse r scaf))
		     ((too-small? x) (if (too-large? y)
					 (recurse m scaf)
					 (insert-binary m y r scaf)))
		     ((too-large? y) (insert-binary l x m scaf))
		     (else           (insert-trinary l x m y r scaf))))))))

    ;; Display tree in an ad-hoc human-readable format. This is
    ;; intended for debugging and testing and should not be used in
    ;; production code. O(n) time.
    (define (tree23-display tree)
      (display "tree23 with ")
      (display (tree23-size tree))
      (display " elements:")
      (newline)

      (define indent-increment "  ")
      (let recurse ((tree tree)
		    (indent ""))
	(let ((next-indent (string-append indent indent-increment)))
	  (case/tree ((l x m y r) tree)
            (empty   #false)
	    (binary  (recurse l next-indent)
		     (display indent)
		     (display "binary node x=")
		     (display x)
		     (newline)
		     (recurse r next-indent))
	    (trinary (recurse l next-indent)
		     (display indent)
		     (display "trinary node x=")
		     (display x)
		     (newline)
		     (recurse m next-indent)
		     (display indent)
		     (display "y=")
		     (display y)
		     (newline)
		     (recurse r next-indent))))))

    ;; Raise an error if any 2-3 tree property is violated in tree. No
    ;; effect otherwise. This is intended for debugging and testing
    ;; and should not be used in production code. O(n^2) time.
    (define (tree23-validate comparator tree)
      (define pass (constant-thunk #f))

      (define (assert-in-order a b)
	(unless (<? comparator a b)
		(error "order constraint violation")))

      (define (assert-for-all proc node)
	(for-each proc (tree23->ordered-list node)))

      (unless (empty? tree)

        (let inorder ((tree tree))
	  (case/tree ((l x m y r) tree)
	    (empty   #false)
	    (binary  (begin (assert-for-all (cute assert-in-order <> x) l)
		     (assert-for-all (cute assert-in-order x <>) r)
		     (inorder l)
		     (inorder r)))
	    (trinary (begin (assert-in-order x y)
		     (assert-for-all (cute assert-in-order <> x) l)
		     (assert-for-all (cute assert-in-order x <>) m)
		     (assert-for-all (cute assert-in-order <> y) m)
		     (assert-for-all (cute assert-in-order y <>) r)
		     (inorder l)
		     (inorder m)
		     (inorder r)))))

	(let ((ds (let leaf-depths ((tree tree)
				    (d 0)
				    (lst '()))
		    (let ((d+1 (add1 d)))
		      (case/tree ((l x m y r) tree)
  		        (empty   (cons d lst))
			(binary  (leaf-depths r d+1 (leaf-depths l d+1 lst)))
			(trinary (leaf-depths r d+1 (leaf-depths m d+1 (leaf-depths l d+1 lst)))))))))
	  (unless (every (cute = <> (first ds)) ds)
		  (error "balance constraint violation")))

	))
	
    ))
