
(define-library (brother12)
  (import
   (comparators)
   (scheme base)
   (srfi 1)
   (srfi 26)
   (util))
  (export
   make-brother12

   brother12-empty?
   brother12-size
   brother12-min
   brother12-max

   brother12-search
   brother12-insert
   ;;brother12-delete

   make-brother12-iterator
   brother12-iterator-empty?
   brother12-iterator-peek
   brother12-iterator-pop

   make-brother12-build
   brother12-build-push
   brother12-build-finish

   brother12-fold
   brother12-filter
   brother12-map/monotone
   brother12-map

   brother12->ordered-list
   ordered-list->brother12
   list->brother12

   brother12-range

   brother12-validate)

  (begin

    (define-singleton empty empty?)

    (define-syntax-rule (define-node-type TYPE CONSTRUCTOR PREDICATE L X R)
      (define-record-type TYPE
	(CONSTRUCTOR l x r)
	PREDICATE
	(l L)
	(x X)
	(r R)))
    
    (define-node-type <level> make-level level? level-l level-x level-r)
    (define-node-type <lefty> make-lefty lefty? lefty-l lefty-x lefty-r)
    (define-node-type <righty> make-righty righty? righty-l righty-x righty-r)

    #|
    (define (internal? x)
      (or (level? x) (lefty? x) (righty? x)))

    (define (leaf? x)
      (not (or (empty? x) 
	       (internal? x))))
    |#

    (define make-brother12 (constant empty))

    (define brother12-empty? empty?)

    (define-syntax case/node
      (syntax-rules (empty leaf level lefty righty)
	((case/node ((L X R) ROOT)
		    (empty EMPTY-BODY)
		    (leaf LEAF-BODY)
		    (level LEVEL-BODY)
		    (lefty LEFTY-BODY)
		    (righty RIGHTY-BODY))
	 (let ((root ROOT))
	   (cond
	    ((level? root)
	     (let ((L (level-l root))
		   (X (level-x root))
		   (R (level-r root)))
	       LEVEL-BODY))
	    ((lefty? root)
	     (let ((L (lefty-l root))
		   (X (lefty-x root))
		   (R (lefty-r root)))
	       LEFTY-BODY))
	    ((righty? root)
	     (let ((L (righty-l root))
		   (X (righty-x root))
		   (R (righty-r root)))
	       RIGHTY-BODY))
	    ((empty? root)
	     EMPTY-BODY)
	    (else
	     (let ((X root))
	       LEAF-BODY)))))))

    (define-syntax case/symmetric
      (syntax-rules (empty leaf internal)
	((case/symmetric ((L X R) ROOT)
			 (empty EMPTY-BODY)
			 (leaf LEAF-BODY)
			 (internal INTERNAL-BODY))
	 (case/node ((L X R) ROOT)
	   (empty EMPTY-BODY)
	   (leaf LEAF-BODY)
	   (level INTERNAL-BODY)
	   (lefty INTERNAL-BODY)
	   (righty INTERNAL-BODY)))))

    (define (brother12-size root)
      (case/symmetric ((l x r) root)
	(empty    0)
	(leaf     1)
	(internal (+ (brother12-size l)
		     1
		     (brother12-size r)))))

    (define-syntax-rule (define-brother12-extreme IDENTIFIER L R DIRECTION)
      (define (IDENTIFIER root)
	(case/symmetric ((L x R) root)
	  (empty    (error "brother12-extreme: unexpected empty tree"))
	  (leaf     x)
	  (internal (if (empty? DIRECTION)
			x
			(IDENTIFIER DIRECTION))))))

    (define-brother12-extreme brother12-min l r l)
    (define-brother12-extreme brother12-max l r r)

    (define (brother12-search root comparator q match-proc missing-proc)
      (let recurse ((root root))
	(case/symmetric ((l x r) root)
          (empty
	   (missing-proc))
	  (leaf
	   (if (=? comparator q x)
	       (match-proc x)
	       (missing-proc)))
	  (internal
	   (if3 (comparator-compare comparator q x)
		(recurse l)
		(match-proc x)
		(recurse r))))))

    (define (brother12-insert root comparator q selector)
      ;; compare x with q
      (define (compare x)
	(comparator-compare comparator q x))

      ;; convert a trinary node with unary parent into two binary
      ;; nodes (figure p. 3)
      (define (trinary-internal->binaries cl cx cm cy cr) 
	(make-righty (make-righty cl cx cm) cy cr))
      (define (trinary-terminal->binaries cx cy)
	(make-righty cx cy empty))

      (let down ((root root)
		 (up-binary identity)
		 (up-trinary-internal trinary-internal->binaries)
		 (up-trinary-terminal trinary-terminal->binaries))
	(case/node ((l x r) root)
	  (empty
	   (up-binary q))
	  (leaf
	   (if3 (compare x)
		(up-trinary-terminal q x)
		(up-binary (selector x q))
		(up-trinary-terminal x q)))
	  (level
	   (if3 (compare x)
		(down l
		      (lambda (child)
			(up-binary (make-level child x r)))
		      (lambda (cl cx cm cy cr) ;; figure p. 4 bottom left
			(up-trinary-internal (make-righty cl cx cm)
					     cy cr x r))
		      (lambda (cx cy)
			(up-trinary-internal cx cy empty x r)))
		(up-binary (make-level l (selector x q) r))
		(down r
		      (lambda (child)
			(up-binary (make-level l x child)))
		      (lambda (cl cx cm cy cr) ;; figure p. 4 bottom right
			(up-trinary-internal l x cl cx
					     (make-lefty cm cy cr)))
		      (lambda (cx cy)
			(up-trinary-internal l x empty cx cy)))))
	  (lefty
	   (if3 (compare x)
		(if (empty? l)
		    (up-binary (make-level q x r))
		    (down l
			  (lambda (child)
			    (up-binary (make-lefty child x r)))
			  (lambda (cl cx cm cy cr) ;; figure p. 3
			    (up-binary (make-level (trinary-internal->binaries cl cx cm cy cr)
						   x r)))
			  (lambda (cx cy)
			    (up-binary (make-level (trinary-terminal->binaries cx cy)
						   x r)))))
		(up-binary (make-lefty l (selector x q) r))
		(down r
		      (lambda (child)
			(up-binary (make-lefty l x child)))
		      (lambda (cl cx cm cy cr) ;; figure p. 4 top right
			(up-binary (make-level (make-level l x cl)
					       cx
					       (make-lefty cm cy cr))))
		      (lambda (cx cy)
			(up-binary (make-level x cx cy))))))
	  (righty
	   (if3 (compare x)
		(down l
		      (lambda (child)
			(up-binary (make-righty child x r)))
		      (lambda (cl cx cm cy cr) ;; figure p. 4 top left
			(up-binary (make-level (make-righty cl cx cm)
					       cy
					       (make-level cr x r))))
		      (lambda (cx cy)
			(up-binary (make-level cx cy x))))
		(up-binary (make-righty l (selector x q) r))
		(if (empty? r)
		    (up-binary (make-level l x q))
		    (down r
			  (lambda (child)
			    (up-binary (make-righty l x child)))
			  (lambda (cl cx cm cy cr) ;; figure p. 3
			    (up-binary (make-level l x
						   (trinary-internal->binaries cl cx cm cy cr))))
			  (lambda (cx cy)
			    (up-binary (make-level l x
						   (trinary-terminal->binaries cx cy)))))))))))
#|
    (define (brother12-delete root comparator q missing-proc)
      (define (compare q x)
	(comparator-compare comparator q x))

      (let down ((root root)
		 (q q)
		 (up-binary identity)
		 (up-unary identity))
	(case/node ((l x r) root)
          (empty
	   (missing-proc))
	  (leaf
	   (if (=? comparator q x)
	       (up-unary empty)
	       (missing-proc)))
	  (level
	   (if3 (compare q x)
		(down l q
		      (lambda (child)
			(up-binary (make-level child x r)))
		      (lambda (child)
			(up-binary (make-lefty child x r))))
		(down root (brother12-min r) up-binary up-unary)
		(down r q
		      (lambda (child)
			(up-binary (make-level l x child)))
		      (lambda (child)
			(up-binary (make-righty l x child))))))
	  (lefty
	   (if3 (compare q x)
		(down l q
		      (lambda (child) ;; figure p. 8
			(case/node ((rl rx rr) r)
			  (empty (up-binary x))
			  (leaf (up-binary (make-lefty empty x r)))
			  (level (up-binary (make-righty (make-lefty child x rl) rx rr)))
			  (lefty (up-unary (make-level (make-level child x rl) rx rr)))
			  (righty (up-unary (make-level (make-level child x rl) rx rr)))))
		      (lambda (child) ;; figure p. 7
			(up-unary (make-level child x r))))
		(let ((successor (brother12-min r)))
		  (
							 rx
							 rr)
			   (
			   
			(cond
			 ((lefty? r)
			  (up-unary (make-level (make-level child x (lefty-l r))
						(lefty-x r)
						(lefty-r r))))
			 ((righty? r)
			  (up-unary (make-level (make-level child x (lefty-l r))
						(lefty-x r)
						(lefty-r r))))
			(if (level? r)
			    (up-binary (make-righty (make-lefty child x (lefty-l r)
      )
|#
    (define (push-left-spine iter root)
      (case/symmetric ((l x r) root)
        (empty    iter)
	(leaf     (cons x iter))
	(internal (push-left-spine (cons root iter) l))))

    (define make-brother12-iterator (cute push-left-spine '() <>))

    (define brother12-iterator-empty? null?)

    (define (brother12-iterator-peek iter)
      (case/symmetric ((l x r) (car iter))
        (empty    (error "unreachable state"))
	(leaf     x)
	(internal x)))

    (define (brother12-iterator-pop iter)
      (case/symmetric ((l x r) (car iter))
        (empty    (error "unreachable state"))
	(leaf     (cdr iter))
	(internal (push-left-spine (cdr iter) r))))

    #|

    (define-record-type <spine>
      (make-spine full? l x u)
      spine?
      (full? spine-full?)
      (l spine-l)
      (x spine-x)
      (u spine-u))

    (define make-brother12-builder (constant empty))

    (define (brother12-builder-push build q)
      (define full (cute make-spine #t <> <> <>))
      (define half (cute make-spine #f <> <> <>))

      (full empty
	    q
	    (if (empty? build)
		empty
		(let carry ((spine build))
		  (let ((l (spine-l spine))
			(x (spine-x spine))
			(u (spine-u spine)))
		    (cond
		     ((empty? u)
		      (half l x empty))
		     ((spine-full? spine)
		      (half l x (carry u)))
		     (else
		      (full (make-level (spine-l u) (spine-x u) l)
			    x
			    (spine-u u)))))))))

    (define (brother12-builder-finalize build)
      (let loop ((spine build)
		 (root empty))
	(if (empty? spine)
	    root
	    (loop (spine-u spine)
		  (if (spine-full? spine)
		      (make-level (spine-l spine)
				  (spine-x spine)
				  root)
		      (make-lefty (spine-l spine)
				  (spine-x spine)
				  root))))))
|#

    (define-record-type <half>
      (make-half x l u)
      half?
      (x half-x)
      (l half-l)
      (u half-u))

    (define-record-type <full>
      (make-full x l u)
      full?
      (x full-x)
      (l full-l)
      (u full-u))
    
    (define make-brother12-build (constant empty))

    (define (brother12-build-push build q)
      (define (half x l u)
	(cond
	 ((empty? u)
	  (make-half x l u))
	 ((half? u)
;	  (make-full x (make-level l (half-x u) (half-l u)) (half-u u)))

;	  (make-full x (make-level (half-l u) (half-x u) l) (half-u u)))

	  (make-full x
		     (if (empty? l)
			 (half-x u)
			 (make-level (half-l u) (half-x u) l))
		     (half-u u)))
	      
	 (else
	  (make-half x l (half (full-x u) (full-l u) (full-u u))))))

      (make-full q
		 empty
		 (if (empty? build)
		     empty
		     (half (full-x build) (full-l build) (full-u build)))))

    (define (brother12-build-finish build)
      (let loop ((t empty) (build build))
	(cond
	 ((empty? build)
	  t)
	 ((half? build)
;	  (loop (make-righty t (half-x build) (half-l build))

	  (loop (make-lefty (half-l build) (half-x build) t)
		(half-u build)))

	 (else
;	  (loop (make-level t (full-x build) (full-l build))

;	  (loop (make-level (full-l build) (full-x build) t)
;		(full-u build))))))

	  (loop (if (empty? (full-l build))
		    (full-x build)
		    (make-level (full-l build) (full-x build) t))
		(full-u build))))))

    (define (brother12-fold f knil root)
      (let recurse ((knil knil) (root root))
        (case/symmetric ((l x r) root)
          (empty    knil)
	  (leaf     (f x knil))
	  (internal (recurse (f x (recurse knil l))
			     r)))))

    (define (brother12-filter f root)
      (brother12-build-finish
       (brother12-fold (lambda (x build)
			 (if (f x)
			     (brother12-build-push build x)
			     build))
		       (make-brother12-build)
		       root)))

    (define (brother12-map/monotone f root)
      (let recurse ((root root))
        (case/node ((l x r) root)
          (empty  empty)
	  (leaf   (f x))
	  (level  (make-level  (recurse l) (f x) (recurse r)))
	  (lefty  (make-lefty  (recurse l) (f x) (recurse r)))
	  (righty (make-righty (recurse l) (f x) (recurse r))))))

    (define (brother12-map f root comparator selector)
      (brother12-fold (lambda (x root)
			(brother12-insert root comparator (f x) selector))
		      (make-brother12)
		      root))

    (define (brother12->ordered-list root)
      ;; build the list right-to-left so it doesn't need to be
      ;; reversed
      (let recurse ((root root) (successors '()))
	(case/symmetric ((l x r) root)
          (empty    successors)
	  (leaf     (cons x successors))
	  (internal (recurse l (cons x (recurse r successors)))))))

    (define (ordered-list->brother12 lst)
      (brother12-build-finish
       (fold (flip brother12-build-push) 
	     (make-brother12-build)
	     lst)))

    (define (list->brother12 lst comparator selector)
      (fold (lambda (x root)
	      (brother12-insert root comparator x selector))
	    (make-brother12)
	    lst))

    (define (brother12-range root comparator <least? >greatest?)
      (brother12-build-finish
       (let recurse ((root root) (build (make-brother12-build)))
	 (case/symmetric ((l x r) root)
           (empty build)
	   (leaf
	    (if (or (<least? x) (>greatest? x))
		build
		(brother12-build-push build x)))
	   (internal
	    (let ((lt (<least? x))
		  (gt (>greatest? x)))
	      (cond
	       ((and lt gt) build)
	       (lt (recurse r build))
	       (gt (recurse l build))
	       (else
		(recurse r
			 (brother12-build-push (recurse l build)
					       x))))))))))

    (define (brother12-validate root comparator)

      (define (check-heights hl hr)
	(unless (= hl hr)
	  (error (string-append "depth constraint violation: "
				(number->string hl)
				" versus "
				(number->string hr))))
	hl)
		
      (let height ((root root))
	(case/node ((l x r) root)
          (empty 0)
	  (leaf 1)
	  (level (add1 (check-heights (height l)
				      (height r))))
	  (lefty (add1 (check-heights (add1 (height l))
				      (height r))))
	  (righty (add1 (check-heights (height l)
				       (add1 (height r)))))))

      (let ordered ((root root))
	(case/symmetric ((l x r) root)
          (empty #f)
	  (leaf  #f)
	  (internal
	   (begin
	     (when (or (any (lambda (cx)
			      (>=? comparator cx x))
			    (brother12->ordered-list l))
		       (any (lambda (cx)
			      (<=? comparator cx x))
			    (brother12->ordered-list r)))
		   (error "order violation"))
	     (ordered l)
	     (ordered r))))))
    ))
