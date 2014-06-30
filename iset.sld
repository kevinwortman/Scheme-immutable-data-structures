
(define-library (iset)
  (import
   (comparators)
   (scheme base)
   (scheme write)
   (selector)
   (srfi 1)
   (srfi 8)
   (srfi 26)
   (tree23)
   (util))

  (export

   iset
   iset?

   iset-comparator
   iset-selector
   iset-empty?
   iset-size

   iset-difference
   iset-intersection
   iset-union
   iset-xor

   iset-member?
   iset-min
   iset-max

   iset-find
   iset-include
   iset-exclude

   iset<?
   iset<=?
   iset=?
   iset>=?
   iset>?

   iset-between

   iset-filter
   iset-fold
   iset-map/nondecreasing
   iset-map

   iset->ordered-list
   ordered-list->iset
   list->iset)

  (begin

    (define-record-type <iset>
      (make-iset-record comparator selector min max size tree)
      iset?
      (comparator iset-comparator)
      (selector iset-selector)
      (min iset-min/unchecked)
      (max iset-max/unchecked)
      (size iset-size)
      (tree iset-tree))

    ;; Private procedures.

    (define (make-empty comparator selector)
      (make-iset-record comparator selector #f #f 0 (make-tree23)))

    (define (from-tree/size comparator selector size tree)
      (if (zero? size)
	  (make-empty comparator selector)
	  (make-iset-record comparator
			    selector
			    (tree23-min tree)
			    (tree23-max tree)
			    size
			    tree)))

    (define (from-tree comparator selector tree)
      (from-tree/size comparator selector (tree23-size tree) tree))

    (define (from-list comparator selector lst)
      (from-tree comparator selector (list->tree23 comparator selector lst)))

    (define (replace-tree like tree)
      (from-tree (iset-comparator like) (iset-selector like) tree))

    ;; Compare x and y using set's comparator.
    (define (compare set x y)
      (comparator-compare (iset-comparator set) x y))

    ;; Select between x and y using set's selector.
    (define (select set x y)
      (selector-select (iset-selector set) x y))

    ;; Constructors.

    (define-syntax define-constructor
      (syntax-rules ()
	((define-constructor IDENTIFIER FORMALS BODY)
	 (define (IDENTIFIER . args)
	   (let ((proc (lambda FORMALS BODY)))
	     (let loop ((args args)
			(k 2)
			(comparator default-comparator)
			(selector left-selector))

	       (define (done) (apply proc comparator selector args))

	       (if (or (zero? k) (null? args))
		   (done)
		   (let ((a (first args)))
		     (cond
		      ((comparator? a)
		       (loop (cdr args) (sub1 k) a selector))
		      ((selector? a)
		       (loop (cdr args) (sub1 k) comparator a))
		      ((iset? a)
		       (loop (cdr args) (sub1 k) (iset-comparator a) (iset-selector a)))
		      (else
		       (done)))))))))))
    
    (define-constructor list->iset (comparator selector list)
      (from-list comparator selector list))

    (define-constructor iset (comparator selector . elements)
      (from-list comparator selector elements))

    (define-constructor ordered-list->iset (comparator selector list)
      (from-tree comparator selector (ordered-list->tree23 list)))

    ;;;; set operations

    ;; Takes a binary procedure and returns a procedure that takes two
    ;; or more arguments, applying the binary operation in
    ;; left associative order.
    (define (left-associative binary-operation)
      (lambda (left right . rest)
	(fold (flip binary-operation) left (cons right rest))))

    ;; Defines an abstract binary set operation,
    ;; e.g. union. INCL-LEFT, INCL-RIGHT, and INCL-COMMON are Booleans
    ;; that control whether distinct left, distinct right, and common
    ;; elements are included in the result, respectively.
    ;;
    ;; This is defined as syntax because some branches are dead
    ;; depending on the values of INCL-LEFT, INCL-RIGHT, and
    ;; INCL-COMMON. Having them be macro arguments instead of
    ;; procedure arguments makes it easier for a compiler to eliminate
    ;; the dead branches.
    (define-syntax-rule (define-operation IDENTIFIER INCL-LEFT INCL-RIGHT INCL-COMMON)
      (define IDENTIFIER
	(left-associative
	 (lambda (left right)
	   (let ((cmp (iset-comparator left))
		 (sel (iset-selector left)))
	     (let loop ((li (make-tree23-iter (iset-tree left)))
			(ri (make-tree23-iter (iset-tree right)))
			(scaf (make-tree23-scaffold)))
	       
	       (define ins tree23-scaffold-insert-max)

	       (define (finish scaf)
		 (from-tree cmp sel (tree23-scaffold-finish scaf)))

	       (define (take-all iter)
		 (let loop ((iter iter)
			    (scaf scaf))
		   (if (tree23-iter-empty? iter)
		       (finish scaf)
		       (loop (tree23-iter-pop iter)
			     (ins scaf (tree23-iter-peek iter))))))

	       (cond
		((and (tree23-iter-empty? li) (tree23-iter-empty? ri))
		 (finish scaf))
		((tree23-iter-empty? li)
		 (if INCL-RIGHT
		     (take-all ri)
		     (finish scaf)))
		((tree23-iter-empty? ri)
		 (if INCL-LEFT
		     (take-all li)
		     (finish scaf)))
		(else
		 (let ((l (tree23-iter-peek li))
		       (r (tree23-iter-peek ri)))
		 (if3 (compare left l r)
		      (loop (tree23-iter-pop li)
			    ri
			    (if INCL-LEFT
				(ins scaf l)
				scaf))
		      (loop (tree23-iter-pop li)
			    (tree23-iter-pop ri)
			    (if INCL-COMMON
				(ins scaf (selector-select sel l r))
				scaf))
		      (loop li
			    (tree23-iter-pop ri)
			    (if INCL-RIGHT
				(ins scaf r)
				scaf))))))))))))

    ;; For each of the following set operations, (<operation> set1
    ;; set2 ...)  performs the operation on set1 set2... in
    ;; left-associative order. For example (iset-union a b c) returns
    ;; the set-theoretic union of isets a, b, and c. O(kn) time, where
    ;; k is the number of operations performed and n is the maximum
    ;; size of any set.
    (define-operation iset-difference   #t #f #f)
    (define-operation iset-intersection #f #f #t)
    (define-operation iset-union        #t #t #t)
    (define-operation iset-xor          #t #t #f)

    ;;;; iset-include and iset-exclude must be careful to keep min,
    ;;;; max, and size updated.

    ;; (iset-include set x1 ...) returns an iset obtained by inserting
    ;; x1, ... into set in left-associative order.
    (define iset-include
      ;; This almost reduces to a simple tree23 operation, except that
      ;; updating the new set's min, max, and size in O(1) takes some
      ;; care.
      (left-associative
       (lambda (set x)
	 (let ((cmp (iset-comparator set))
	       (sel (iset-selector set)))
	   (if (iset-empty? set)
	       (make-iset-record cmp sel x x 1
				 (tree23-insert cmp sel (make-tree23) x))
	       (let ((cur-min (iset-min set))
		     (cur-max (iset-max set))
		     (cur-size (iset-size set)))
		 (make-iset-record cmp
				   sel
				   (if3 (compare set cur-min x)
					cur-min
					(select set cur-min x)
					x)
				   (if3 (compare set cur-max x)
					x
					(select set cur-max x)
					cur-max)
				   (if (iset-member? set x)
				       cur-size
				       (add1 cur-size))
				   (tree23-insert cmp sel (iset-tree set) x))))))))
	    
    ;; (iset-exclude set x1 ...) returns an iset obtained by removing,
    ;; if present, x1, ... from set in left-associative order.
    (define iset-exclude
      ;; Again, pretty straightforward except for updating min, max,
      ;; and size. When the min or max element are deleted we have to
      ;; traverse the new tree to find the new min/max, which takes
      ;; O(log n) time.
      (left-associative
       (lambda (set x)
	 (if (not (iset-member? set x))
	     set
	     (let ((cmp (iset-comparator set))
		   (sel (iset-selector set))
		   (size (iset-size set)))
	       (if (= 1 size)
		   (make-empty cmp sel)
		   (let ((tree (tree23-delete cmp
					      (cute error "unreachable state")
					      (iset-tree set)
					      x))
			 (cur-min (iset-min set))
			 (cur-max (iset-max set)))
		     (make-iset-record cmp
				       sel
				       (if (=? cmp cur-min x)
					   (tree23-min tree)
					   cur-min)
				       (if (=? cmp cur-max x)
					   (tree23-max tree)
					   cur-max)
				       (sub1 size)
				       tree))))))))

    ;;;; set relations

    ;; Wrapper around call/cc to support a short-circuit return from a
    ;; procedure.
    (define-syntax-rule (with-exit (RETURN) BODY ...)
      (call/cc
       (lambda (RETURN)
	 BODY ...)))

    (define (set-equal? left right)
      (with-exit (return)
	;; Fast path for when cardinalities differ.
        (unless (= (iset-size left) (iset-size right))
	  (return #false))

	;; When both sets are the same size, we must test
	;; whether their contents are identical.
	(iset-fold (lambda (l ri)
		     (unless (=? (iset-comparator left) l (tree23-iter-peek ri))
		       (return #false))
		     (tree23-iter-pop ri))
		   (make-tree23-iter (iset-tree right))
		   left)

	;; If we got here, the sets are equal in size and contents.
	#true))

    ;; Takes a binary predicate that is transitive, and returns an
    ;; n-ary procedure that tests the predicate on adjacent
    ;; arguments. The procedure uses short-circuit logic, in other
    ;; words stops evaluating as soon as the result is certain.
    (define (transitive-relation binary-relation)
      (lambda (left first . rest)
	(with-exit (return)
	  (fold (lambda (right left)
		  (unless (binary-relation left right)
			  (return #false))
		  right)
		left
		(cons first rest))
	  #true)))

    (define iset=? (transitive-relation set-equal?))

    ;; Define either of the two binary subset relations, ⊂ or ⊆. First
    ;; try to disprove the relation based on set cardinalities in O(1)
    ;; time. If that cannot be done, iterate through the sets,
    ;; searching for an element of the left set that does not exit in
    ;; the right set, and return #false as soon as one is found. If
    ;; none is found, return #true. O(n) worst case time, but faster
    ;; for common use cases.
    (define-syntax-rule (define-subset IDENTIFIER SIZE-RELATION)
      (define (IDENTIFIER left right)
	(and (SIZE-RELATION (iset-size left) (iset-size right))
	     (let loop ((li (make-tree23-iter (iset-tree left)))
			(ri (make-tree23-iter (iset-tree right))))
	       (cond
		((tree23-iter-empty? li) #true)
		((tree23-iter-empty? ri) #false)
		(else
		 (if3 (compare left (tree23-iter-peek li) (tree23-iter-peek ri))
		      #false
		      (loop (tree23-iter-pop li) (tree23-iter-pop ri))
		      (loop li (tree23-iter-pop ri)))))))))

    (define-subset subset? <=)
    (define-subset proper-subset? <)

    (define iset<=? (transitive-relation subset?))
    (define iset>=? (transitive-relation (flip subset?)))
    (define iset<?  (transitive-relation proper-subset?))
    (define iset>?  (transitive-relation (flip proper-subset?)))

    ;; The remaining procedures are thin wrappers around tree23
    ;; operations.

    ;; Return true iff set is empty. O(1) time.
    (define (iset-empty? set)
      (tree23-empty? (iset-tree set)))

    ;; Return true iff set contains x. O(log n) time.
    (define (iset-member? set x)
      (tree23-search (iset-comparator set)
		     (lambda (match)
		       #true)
		     (constant-thunk #false)
		     (iset-tree set)
		     x))

    (define (iset-min set)
      (when (iset-empty? set)
	    (error "iset-min: empty set"))
      (iset-min/unchecked set))

    (define (iset-max set)
      (when (iset-empty? set)
	    (error "iset-max: empty set"))
      (iset-max/unchecked set))

    ;; Return the element of set equal to x, or the result of
    ;; (absent-proc) if no such element exists.
    (define (iset-find set x absent-proc)
      (tree23-search (iset-comparator set)
		     identity
		     absent-proc
		     (iset-tree set)
		     x))

    (define (iset-between set min min-inclusive max max-inclusive)
      (let ((cmp (iset-comparator set)))
	(replace-tree set
		      (tree23-range (cute (if min-inclusive <? <=?) cmp <> min)
				    (cute (if max-inclusive >? >=?) cmp <> max)
				    (iset-tree set)))))

    (define (iset-filter f set)
      (replace-tree set (tree23-filter f (iset-tree set))))

    (define (iset-fold f knil set)
      (tree23-fold f knil (iset-tree set)))

    (define (iset-map/nondecreasing f set)
      (from-tree/size (iset-comparator set)
		      (iset-selector set)
		      (iset-size set)
		      (tree23-map/nondecreasing f (iset-tree set))))

    (define (iset-map f set)
      (replace-tree set (tree23-map f
				    (iset-comparator set)
				    (iset-selector set)
				    (iset-tree set))))

    (define (iset->ordered-list set)
      (tree23->ordered-list (iset-tree set)))

     ))
