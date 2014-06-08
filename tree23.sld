
(define-library (tree23)
  (import
   (comparators)
   (scheme base)
   (scheme write)
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
   tree23-validate
   )

  (begin

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

    #|
    (define-record-type <bileaf>
      (make-bileaf x y)
      bileaf?
      (x bileaf-x)
      (y bileaf-y))

    (define (internal? node)
      (or (binary? node) (trinary? node)))

    (define (leaf? node)
      (not (internal? node)))

    (define (binary-terminal? node)
      (leaf? (binary-l node)))

    (define (trinary-terminal? node)
      (leaf? (trinary-l node)))
    |#

    (define make-tree23 (constant empty))

    (define make-binary/leaf (cute make-binary empty <> empty))
    (define make-trinary/leaf (cute make-binary empty <> empty <> empty))

    (define (tree23? x)
      (or (binary? x)
	  (trinary? x)
	  (empty? x)))

    (define tree23-empty? empty?)

    #|
    (define-syntax case/node
      (syntax-rules (unileaf bileaf binary trinary)
	((case/node ((L X M Y R) NODE)
	   (unileaf  UNILEAF-BODY ...)
	   (bileaf   BILEAF-BODY ...)
	   (binary   BINARY-BODY ...)
	   (trinary  TRINARY-BODY ...))

	 (let ((n NODE))
	   (cond
	    ((binary? n)
	     (let ((L (binary-l n))
		   (X (binary-x n))
		   (R (binary-r n)))
	       BINARY-BODY ...))
	    ((trinary? n)
	     (let ((L (trinary-l n))
		   (X (trinary-x n))
		   (M (trinary-m n))
		   (Y (trinary-y n))
		   (R (trinary-r n)))
	       TRINARY-BODY ...))
	    ((bileaf? n)
	     (let ((X (bileaf-x n))
		   (Y (bileaf-y n)))
	       BILEAF-BODY ...))
	    (else
	     (let ((X n))
	       UNILEAF-BODY ...)))))))
    |#

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

    #|
    (define (tree23-size tree)
      (if (empty? tree)
	  0
	  (let recurse ((node tree))
	    (case/node ((l x m y r) node)
	      (unileaf  1)
	      (bileaf   2)
	      (binary   (+ 1 
			   (recurse l)
			   (recurse r)))
	      (trinary  (+ 2
			   (recurse l)
			   (recurse m)
			   (recurse r)))))))
    |#
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

    #|
    (define-syntax-rule (define-extreme IDENTIFIER L X Y R CHILD BILEAF-ELT)
      (define (IDENTIFIER tree)
	(if (empty? tree)
	    (error (string-append (symbol->string IDENTIFIER)
				  ": unexpected empty tree"))
	    (let recurse ((node tree))
	      (case/node ((L X m Y R) node)
                (unileaf x)
		(bileaf  BILEAF-ELT)
		(binary  (recurse CHILD))
		(trinary (recurse CHILD)))))))
    |#

    (define-syntax-rule (define-extreme IDENTIFIER L X Y R CHILD TRINARY-ELEMENT)
      (define (IDENTIFIER tree)
	(case/tree ((L X m Y R) tree)
	  (empty   (error "tree23-min/max: unexpected empty tree"))
	  (binary  (if (empty? CHILD)
		       x
		       (IDENTIFIER CHILD)))
	  (trinary (if (empty? CHILD)
		       TRINARY-ELEMENT
		       (IDENTIFIER CHILD))))))

    #|
    (define-extreme tree23-min l x y r l x)
    (define-extreme tree23-max l x y r r y)
    |#
    (define-extreme tree23-min l x y r l x)
    (define-extreme tree23-max l x y r r y)

    #|
    (define-syntax case/node-query
      (syntax-rules (unileaf bileaf
		     binary:l binary:x binary:r
		     trinary:l trinary:x trinary:m trinary:y trinary:r)
        ((case/node-query ((L X M Y R) NODE) COMPARATOR QUERY
	   (unileaf   UNILEAF-BODY)
	   (bileaf    BILEAF-BODY)
	   (binary:l  B:L)
	   (binary:x  B:X)
	   (binary:r  B:R)
	   (trinary:l T:L)
	   (trinary:x T:X)
	   (trinary:m T:M)
	   (trinary:y T:Y)
	   (trinary:r T:R))

	 (let ((compare (cute comparator-compare COMPARATOR QUERY <>)))
	   (case/node ((L X M Y R) NODE)
	     (unileaf UNILEAF-BODY)
	     (bileaf  BILEAF-BODY)
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

    |#
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

    #|
    (define (tree23-search comparator match-proc missing-proc tree q)
      (let ((match? (cute =? comparator q <>)))
       (if (empty? tree)
	   (missing-proc)
	   (let recurse ((tree tree))
	     (case/node-query ((l x m y r) tree) comparator q
	       (unileaf    (if (match? x)
			       (match-proc x)
			       (missing-proc)))
	       (bileaf     (cond
			    ((match? x) (match-proc x))
			    ((match? y) (match-proc y))
			    (else       (missing-proc))))
	       (binary:l  (recurse l))
	       (binary:x  (match-proc x))
	       (binary:r  (recurse r))
	       (trinary:l (recurse l))
	       (trinary:x (match-proc x))
	       (trinary:m (recurse m))
	       (trinary:y (match-proc y))
	       (trinary:r (recurse r)))))))
    |#

    (define (tree23-search comparator match-proc missing-proc tree q)
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

    #|
    (define (tree23-insert comparator selector tree q)
      (define compare (cute comparator-compare comparator q <>))

      (if (empty? tree)
	  q
	  (let down ((tree tree)
		     (return identity)
		     (kickup identity))
 	    (case/node-query ((l x m y r) tree) comparator q
	      (unileaf   (if3 (compare x)
			      (return (make-bileaf q x))
			      (return (selector x q))
			      (return (make-bileaf x q))))
	      (bileaf    (if3 (compare x)
			      (kickup (make-binary q x y))
			      (return (make-bileaf (selector x q) y))
			      (if3 (compare y)
				   (kickup (make-binary x q y))
				   (return (make-bileaf x (selector y q)))
				   (kickup (make-binary x y q)))))
	      (binary:l  (down l
			       (lambda (w)
				 (return (make-binary w x r)))
			       (lambda (w)
				 (return (make-trinary (binary-l w)
						       (binary-x w)
						       (binary-r w)
						       x
						       r)))))
	      (binary:x  (return (make-binary l (selector x q) r)))
	      (binary:r  (down r
			       (lambda (w)
				 (return (make-binary l x w)))
			       (lambda (w)
				 (return (make-trinary l
						       x
						       (binary-l w)
						       (binary-x w)
						       (binary-r w))))))
	      (trinary:l (down l
			       (lambda (w)
				 (return (make-trinary w x m y r)))
			       (lambda (w)
				 (kickup (make-binary w
						      x
						      (make-binary m y r))))))
	      (trinary:x (return (make-trinary l (selector x q) m y r)))
	      (trinary:m (down m
			       (lambda (w)
				 (return (make-trinary l x w y r)))
			       (lambda (w)
				 (kickup (make-binary (make-binary l x (binary-l w))
						      (binary-x w)
						      (make-binary (binary-r w) y r))))))
	      (trinary:y (return (make-trinary l x m (selector y q) r)))
	      (trinary:r (down r
			       (lambda (w)
				 (return (make-trinary l x m y w)))
			       (lambda (w)
				 (kickup (make-binary (make-binary l x m)
						      y
						      w)))))))))
    |#

    (define (tree23-insert comparator selector tree q)
      (define compare (cute comparator-compare comparator q <>))
      (define select (cute selector <> q))

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

    (define (tree23-delete comparator missing-proc tree q)
      (let down ((tree tree)
		 (q q)
		 (balanced identity)
		 (unbalanced identity))

	(define (display-binary prefix x)
	  (when #f
	    (display prefix) (display " binary x=") (display x) (newline)))

	(define (display-trinary prefix x y)
	  (when #f
 	    (display prefix) (display " trinary x=") (display x) (display ", y=") (display y) (newline)))

	(define (binary-left l x r q)
	  (display-binary "descending left" x)
	  (down l q
		(lambda (l)
		  (display-binary "replacing" x)
		  (balanced (make-binary l x r)))
		(lambda (l)
		  (if (binary? r)
		      (let/binary ((rl rx rr) r) ; case 1, left
			(display-binary "case 1 left" x)
		        (unbalanced (make-trinary l x rl rx rr)))
		      (let/trinary ((rl rx rm ry rr) r) ; case 2, left
			(display-binary "case 2 left" x)
			(balanced (make-binary (make-binary l x rl)
					       rx
					       (make-binary rm ry rr))))))))

	(define (binary-right l x r q)
	  (display-binary "descending right" x)
	  (down r q
		(lambda (r)
		  (display-binary "replacing" x)
		  (balanced (make-binary l x r)))
		(lambda (r)
		  (if (binary? l)
		      (let/binary ((ll lx lr) l) ; case 1, right
			(display-binary "case 1 right" x)
		        (unbalanced (make-trinary ll lx lr x r)))
		      (let/trinary ((ll lx lm ly lr) l) ; case 2, right
			(display-binary "case 2 right" x)
		        (balanced (make-binary (make-binary ll lx lm)
					       ly
					       (make-binary lr x r))))))))

	(define (trinary-left l x m y r q)
	  (display-trinary "descending left" x y)
	  (down l q
		(lambda (l)
		  (display-trinary "replacing" x y)
		  (balanced (make-trinary l x m y r)))
		(lambda (l)
		  (if (binary? m)
		      (let/binary ((ml mx mr) m) ; case 3a, left
			(display-trinary "case 3a left" x y)
		        (balanced (make-binary (make-trinary l x ml mx mr) y r)))
		      (let/trinary ((ml mx mm my mr) m) ; case 4a, left
			(display-trinary "case 4a left" x y)
                        (balanced (make-trinary (make-binary l x ml)
						mx
						(make-binary mm my mr)
						y
						r)))))))

	(define (trinary-middle l x m y r q)
	  (display-trinary "descending middle" x y)
	  (down m q
		(lambda (m)
		  (display-trinary "replacing" x y)
		  (balanced (make-trinary l x m y r)))
		(lambda (m)
		  (if (binary? r)
		      (let/binary ((rl rx rr) r)  ; case 3b, left
			(display-trinary "case 3b left" x y)
                        (balanced (make-binary l x (make-trinary m y rl rx rr))))
		      (let/trinary ((rl rx rm ry rr) r) ; case 4b, left
			(display-trinary "case 4b left" x y)
                        (balanced (make-trinary l
						x
						(make-binary m y rl)
						rx
						(make-binary rm ry rr))))))))

	(define (trinary-right l x m y r q)
	  (display-trinary "descending right" x y)
	  (down r q
		(lambda (r)
		  (display-trinary "replacing" x y)
		  (balanced (make-trinary l x m y r)))
		(lambda (r)
		  (if (binary? m)
		      (let/binary ((ml mx mr) m) ; case 3b, right
			(display-trinary "case 3b right" x y)
		        (balanced (make-binary l x (make-trinary ml mx mr y r))))
		      (let/trinary ((ml mx mm my mr) m) ; case 4b, right
			(display-trinary "case 4b right" x y)
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
					     
    #|
    (define (tree23-delete comparator missing-proc tree q)
      (if (empty? tree)
	  (missing-proc)
	  (let down ((tree tree)
		     (q q)
		     (balanced identity)
		     (hole/empty (constant empty))
		     (hole/node identity))

	    (define (binary-left l x r q)
	      (down l q
		    (lambda (w)
		      (make-binary w x r))
		    (lambda ()
		      (if (bileaf? r)
			  (balanced (make-binary x (bileaf-x r) (bileaf-y r)))
			  (hole/node (make-bileaf x r))))
		    (lambda (w)
		      (case/node ((rl rx rm ry rr) r)
                        (unileaf (hole/node (make-bileaf x r)))
			(bileaf  (balanced (make-binary x rx ry)))
			(binary  (hole/node (make-trinary w x rl rx rr)))
			(trinary (balanced (make-binary (make-binary w x rl)
							rx
							(make-binary rm ry rr))))))))

	    (define (binary-right l x r q)
	      (down r q
		    (lambda (w)
		      (balanced (make-binary l x w)))
		    (lambda ()
		      (if (bileaf? l)
			  (balanced (make-binary (bileaf-x l) (bileaf-y l) r))
			  (hole/node (make-bileaf l x))))
		    (lambda (w)
		      (case/node ((ll lx lm ly lr) l)
		        (unileaf (hole/node (make-bileaf l x)))
			(bileaf  (balanced (make-binary lx ly x)))
			(binary  (hole/node (make-trinary ll lx lr x w)))
			(trinary (balanced (make-binary (make-binary ll lx lm)
							ly
							(make-binary ll x w))))))))

	    (define (trinary-left l x m y r q)
	      (down l q
		    (lambda (w)
		      (balanced (make-trinary w x m y r)))
		    (lambda ()
		      (case/node ((ml mx mm my myr) m)
                        (unileaf (make-binary (make-bileaf x mx)))
			(bileaf  (make-trinary x mx my y r))
			(binary  (make-binary (make-trinary w x ml mx mr) y r))
			(trinary (make-trinary (make-binary w x ml)
					       mx
					       (make-binary ml my mr)
					       y
					       r))))
		    (lambda (w)
		      (case/node ((ml mx mm my myr) m)
		        (unileaf (make-binary (make-trinary 

	    (define (trinary-middle l x m y r q)
	      (down m q
		    (lambda (w)
		      (balanced (make-trinary l x w y r)))
		    (lambda ()
		      (balanced (if (bileaf? r)
				    (make-trinary l x y (bileaf-x r) (bileaf-y r))
				    (make-binary l x (make-bileaf y r)))))
		    (lambda (w)
		      (balanced (if (trinary? r)
				    (make-trinary l
						  x
						  (make-binary w
							       y
							       (trinary-l r))
						  (trinary-x r)
						  (make-binary (trinary-m r)
							       (trinary-y r)
							       (trinary-r r)))
				    (make-binary l
						 x
						 (make-trinary w
							       y
							       (binary-l r)
							       (binary-x r)
							       (binary-r r))))))))

	    (define (trinary-right l x m y r q)
	      (down r q
		    (lambda (w)
		      (balanced (make-trinary l x m y w)))
		    (lambda ()
		      (balanced (if (bileaf? m)
				    (make-trinary l
						  x
						  (bileaf-x m)
						  (bileaf-y m)
						  r)
				    (make-binary l
						 x
						 (make-bileaf m y)))))
		    (lambda (w)
		      (balanced (if (trinary? m)
				    (make-trinary l
						  x
						  (make-binary (trinary-l m)
							       (trinary-x m)
							       (trinary-m m))
						  (trinary-y m)
						  (make-binary (trinary-r m)
							       y
							       w))
				    (make-binary l
						 x
						 (make-trinary (binary-l m)
							       (binary-x m)
							       (binary-r m)
							       y
							       w)))))))

	    (case/node-query ((l x m y r) tree) comparator q
              (unileaf   (if (=? comparator q x)
			     (hole/empty)
			     (missing-proc)))
	      (bileaf    (cond
			  ((=? comparator q x) (hole/node y))
			  ((=? comparator q y) (hole/node x))
			  (else                (missing-proc))))
	      (binary:l  (down l q
			       (lambda (w)
				 (balanced (make-binary w x r)))
			       (lambda ()
				 (if (bileaf? r)
				     (balanced (make-binary x
							    (bileaf-x r)
							    (bileaf-y r)))
				     (hole/node (make-bileaf x r))))
			       (lambda (w)
				 (if (trinary? r)
				     (balanced (make-binary (make-binary w
									 x
									 (trinary-l r))
							    (trinary-x r)
							    (make-binary (trinary-m r)
									 (trinary-y r)
									 (trinary-r r))))
				     (hole/node (make-trinary w
							      x
							      (binary-l r)
							      (binary-x r)
							      (binary-r r)))))))
	      (binary:x  (let ((successor (tree23-min r)))
			   (binary-right l successor r successor)))
	      (binary:r  (binary-right l x r q))
	      (trinary:l (down l q
			       (lambda (w)
				 (balanced (make-trinary w x m y r)))
			       (lambda ()
				 (balanced (if (bileaf? m)
					       (make-trinary x
							     (bileaf-x m)
							     (bileaf-y m)
							     y
							     r)
					       (make-binary (make-bileaf x m)
							    y
							    r))))
			       (lambda (w)
				 (balanced (if (trinary? m)
					       (make-trinary (make-binary w
									  x
									  (trinary-l m))
							     (trinary-x m)
							     (make-binary (trinary-m m)
									  (trinary-y m)
									  (trinary-r m))
							     y
							     r)
					       (make-binary (make-trinary w
									  x
									  (binary-l m)
									  (binary-x m)
									  (binary-r m))
							    y
							    r))))))
	      (trinary:x (let ((successor (tree23-min m)))
			   (trinary-middle l successor m y r successor)))
	      (trinary:m (trinary-middle l x m y r q))
	      (trinary:y (let ((successor (tree23-min r)))
			   (trinary-right l x m successor r successor)))
	      (trinary:r (trinary-right l m x y r q))))))
|#
    #|
    (define (tree23-delete comparator missing-proc root q)
      (let down ((root root)
		 (q q)
		 (return identity)
		 (kickup identity))
	(case/tree-query (l x m y r) root
          (leaf (missing-proc))
	  (terminal (if (=? comparator q root)
			(kickup leaf)
			(missing-proc)))
	  (binary:l
	   (down l q
		 (lambda (ch)
		   (return (make-binary ch x r)))
		 (lambda (ch)
		   (cond
		    ((node2? r) ;; case 1, left
		     (kickup (make-trinary ch x (node2-l r) (node2-x r) (node2-r r))))
		    ((node3? r) ;; case 2, left
		     (return (make-binary (make-binary ch x (nodee-l r))
					 (node3-y r)
					 (make-binary (node3-m r) (node3-y r) (node3-r r)))))
		    (else ;; case 1, left, terminal
		     (return (make-trinary x r)))))))
	  (binary:x
	   (let ((successor (tree23-min r)))
	     (down (make-binary l successor r) successor return kickup)))
	  (binary:r
	   (down r q
		 (lambda (ch)
		   (return (make-binary l x ch)))
		 (lambda (ch)
		   (cond
		    ((node2? l) ;; case 1, right
		     (kickup (make-trinary (node2-l l) (node2-x l) (node2-r l) x ch)))
		    ((node3? l) ;; case 2, right
		     (return (make-binary (make-binary (node3-l l) (node3-x l) (node3-m l))
					 (node3-y l)
					 (make-binary (node3-r l) x ch))))
		    (else ;; case 1, right, terminal
		     (return (make-trinary l x)))))))
	  (trinary:l
	   (down l q
		 (lambda (ch)
		   (return (make-trinary ch x m y r)))
		 (lambda (ch)
		   (cond
		    ((node2? m) ;; case 3a, left
		     (return (make-binary (make-trinary ch x (node2-l m) (node2-x m) (node2-r m))
					 y
					 r)))
		    ((node3? m) ;; case 4a, left
		     (return (make-trinary (make-binary ch x (node3-l m))
					 (node3-x m)
					 (make-binary (node3-m m) (node3-y m) (node3-r m))
					 y
					 r)))
		    (else ;; case 3a, left, terminal
		     (return (make-binary (make-trinary/terminal x m) y r)))))))
	  (trinary:x
	   (if (leaf? l)
	       (return y)
	       (let ((successor (tree23-min m)))
		 (down (make-trinary l successor m y r) successor return kickup))))
	  (trinary:m
	   (down m q
		 (lambda (ch)
		   (return (make-trinary l x ch y r)))
		 (lambda (ch)
		   (cond
		    ((node2? r) ;; case 3b, left
		     (return (make-binary l
					 x
					 (make-trinary ch y (node2-l r) (node2-x r) (node2-r r)))))
		    ((node3? r) ;; case 4b, left
		     (return (make-trinary l
					 x
					 (make-binary ch y (node3-l r))
					 (node3-x r)
					 (make-binary (node3-m r) (node3-y r) (node3-r r)))))
		    (else ;; case 3b, left, terminal
		     (return (make-binary l x (make-trinary/terminal y r))))))))
	  (trinary:y
	   (if (leaf? l)
	       (return x)
	       (let ((successor (tree23-min r)))
		 (down (make-trinary l x m successor r) successor return kickup))))
	  (trinary:r
	   (down r q
		 (lambda (ch)
		   (return (make-trinary l x m y ch)))
		 (lambda (ch)
		   (cond
		    ((node2? m) ;; case 3b, right
		     (return (make-binary l
					 x
					 (make-trinary (node2-l m) (node2-x m) (node2-r m) y ch))))
		    ((node3? m) ;; case 4b, right
		     (return (make-trinary l
					 x
					 (make-binary (node3-l m) (node3-x m) (node3-m m))
					 (node3-y m)
					 (make-binary (node3-r m) y ch))))
		    (else ;; case 3b, right, terminal
		     (return (make-binary l
					 x
					 (make-trinary/terminal m ch)))))))))))

    |#

    ;; tree23-iter
    ;;
    ;; A tree23-iter is a stack data structure, represented as a list
    ;; where each element is one of
    ;;
    ;; (1) a binary node, whose x is unconsumed;
    ;; (2) at-x, containing a trinary node x and y are both unconsumed; or
    ;; (3) at-y, containing a trinary node whose x is consumed and y is
    ;;     unconsumed.

    (define-record-type <at-x>
      (make-at-x tree)
      at-x?
      (tree at-x-tree))

    (define-record-type <at-y>
      (make-at-y tree)
      at-y?
      (tree at-y-tree))

    (define (make-tree23-iter tree)
      (push-left '() tree))

    (define (push-left iter tree)
      (case/tree ((l x m y r) tree)
	(empty   iter)
	(binary  (push-left (cons tree iter) l))
	(trinary (push-left (cons (make-at-x tree) iter) l))))
                     
    (define tree23-iter-empty? null?)

    (define (tree23-iter-peek iter)
      (let ((top (car iter)))
	(cond
	 ((binary? top) (binary-x top))
	 ((at-x? top)   (trinary-x (at-x-tree top)))
	 (else          (trinary-y (at-y-tree top))))))

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

    (define binary-u binary-r)
    (define trinary-u trinary-r)

    (define make-tree23-scaffold (constant empty))

    #|
    (define (tree23-scaffold-insert-max scaf q)
      (if (empty? scaf)
	  (make-binary empty q empty)
	  (let up ((l empty)
		   (x q)
		   (u scaf))
	    (cond
	     ((binary? u)
	      (make-trinary (binary-l u)
			    (binary-x u)
			    l
			    x
			    (binary-u u)))
	     ((trinary? u)
	      (let ((ul (trinary-l u))
		    (ux (trinary-x u)))
 		(make-binary l
			     x
			     (up (if (empty? ul)
				     ux
				     (make-binary ul ux (trinary-m u)))
				 (trinary-y u)
				 (trinary-u u)))))
	     (else
	      (make-binary l x empty))))))
    |#

    (define (tree23-scaffold-insert-max scaf q)
      (let up ((l empty)
	       (x q)
	       (u scaf))
	(case/tree ((ul ux um uy uu) u)
	  (empty   (make-binary l x empty))
	  (binary  (make-trinary ul ux l x uu))
	  (trinary (make-binary l x (up (make-binary ul ux um) uy uu))))))

    #|
    (define (tree23-scaffold-finish scaf)
      (if (empty? scaf)
	  (make-tree23)
	  (let-values (((leaf node) (if (binary? scaf)
					(values (binary-u scaf)
						(binary-x scaf))
					(values (trinary-u scaf)
						(make-bileaf (trinary-x scaf)
							     (trinary-y scaf))))))
	    (let up ((leaf leaf) (node node))
	      (cond
	       ((binary? leaf)  (up (binary-u leaf)
				    (if (empty? (binary-l leaf))
					(binary-x leaf)
					(make-binary (binary-l leaf)
						     (binary-x leaf)
						     node))))
	       ((trinary? leaf) (up (trinary-u leaf)
				    (make-trinary (trinary-l leaf)
						  (trinary-x leaf)
						  (trinary-m leaf)
						  (trinary-y leaf)
						  node)))
	       (else node)))))) ; leaf is empty, done
    |#
    (define (tree23-scaffold-finish scaf)
      (let up ((scaf scaf)
	       (tree empty))
	(case/tree ((l x m y u) scaf)
	  (empty   tree)
	  (binary  (up u (make-binary l x tree)))
	  (trinary (up u (make-trinary l x m y tree))))))

    #|
    (define (tree23-fold f knil tree)
      (if (empty? tree)
	  knil
	  (let recurse ((tree tree) (knil knil))
	    (case/node ((l x m y r) tree)
	      (unileaf (f x knil))
	      (bileaf  (f y (f x knil)))
	      (binary  (recurse r (f x (recurse l knil))))
	      (trinary (recurse r (f y (recurse m (f x (recurse l knil))))))))))
    |#
    (define (tree23-fold f knil tree)
      (let recurse ((tree tree)
		    (knil knil))
	(case/tree ((l x m y r) tree)
          (empty   knil)
	  (binary  (recurse r (f x (recurse l knil))))
	  (trinary (recurse r (f y (recurse m (f x (recurse l knil)))))))))

    #|
    (define (tree23-filter f tree)
      (if (empty? tree)
	  tree
	  (tree23-scaffold-finish
	   (tree23-fold (lambda (x scaf)
			  (if (f x)
			      (tree23-scaffold-insert-max scaf x)
			      scaf))
			(make-tree23-scaffold)
			tree))))
    |#
    (define (tree23-filter f tree)
      (tree23-scaffold-finish
       (tree23-fold (lambda (x scaf)
		      (if (f x)
			  (tree23-scaffold-insert-max scaf x)
			  scaf))
		    (make-tree23-scaffold)
		    tree)))

    #|
    (define (tree23-map/monotone f tree)
      (if (empty? tree)
	  tree
	  (let recurse ((tree tree))
	    (case/node ((l x m y r) tree)
	      (unileaf  (f x))
	      (bileaf   (make-bileaf (f x) (f y)))
	      (binary   (make-binary (recurse l)
				     (f x)
				     (recurse r)))
	      (trinary  (make-trinary (recurse l)
				      (f x)
				      (recurse m)
				      (f y)
				      (recurse r)))))))
    |#
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

    (define (tree23-map f comparator selector tree)
      (tree23-fold (lambda (x result)
		     (tree23-insert comparator selector result (f x)))
		   (make-tree23)
		   tree))

    #|
    (define (tree23->ordered-list tree)
      ;; Note: we traverse the tree backwards in right-to-left order,
      ;; consing elements to the start of lst; the two reversals
      ;; cancel each other out.
      (if (empty? tree)
	  '()
	  (let recurse ((tree tree)
			(lst '()))
	    (case/node ((l x m y r) tree)
	      (unileaf (cons x lst))
	      (bileaf  (cons x (cons y lst)))
	      (binary   (recurse l (cons x (recurse r lst))))
	      (trinary  (recurse l (cons x (recurse m (cons y (recurse r lst))))))))))
    |#

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

    (define (ordered-list->tree23 lst)
      (tree23-scaffold-finish
       (fold (flip tree23-scaffold-insert-max)
	     (make-tree23-scaffold)
	     lst)))

    (define (list->tree23 comparator selector lst)
      (fold (flip (cute tree23-insert comparator selector <> <>))
	    (make-tree23)
	    lst))

    #|
    (define (tree23-range too-small? too-large? tree)
      (if (empty? tree)
	  empty
	  (tree23-scaffold-finish
	   (let recurse ((tree tree)
			 (scaf (make-tree23-scaffold)))

	     (define (insert x scaf)
	       (tree23-scaffold-insert-max scaf x))

	     (define (maybe-insert x scaf)
	       (if (or (too-small? x) (too-large? x))
		   scaf
		   (insert x scaf)))

	     (define (insert-binary l x r scaf)
	       (recurse r (insert x (recurse l scaf))))

	     (case/node ((l x m y r) tree)
	       (unileaf (maybe-insert x scaf))
	       (bileaf  (maybe-insert y (maybe-insert x scaf)))
	       (binary  (cond
			 ((too-small? x) (recurse r scaf))
			 ((too-large? x) (recurse l scaf))
			 (else   	 (insert-binary l x r scaf))))
	       (trinary (cond
			 ((too-large? x)
			  (recurse l scaf))
			 ((too-small? y)
			  (recurse r scaf))
			 ((too-small? x)
			    (if (too-large? y)
				(recurse m scaf)
				(insert-binary m y r scaf)))
			 ((too-large? y)
			  (insert-binary l x m scaf))
			 (else
			  (recurse r (insert y (insert-binary l x m scaf)))))))))))
    |#
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

    #|
    (define (tree23-display tree)
      (display "tree23 with ")
      (display (tree23-size tree))
      (display " elements:")
      (newline)

      (define indent-increment "  ")
      (let recurse ((node tree) (indent ""))
	(let ((next-indent (string-append indent indent-increment)))
	  (case/node ((l x m y r) node)
	    (unileaf (display indent)
		     (display "unileaf: ")
		     (display x)
		     (newline))
	    (bileaf  (display indent)
		     (display "bileaf: ")
		     (display x)
		     (display " ")
		     (display y)
		     (newline))
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
    |#
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

    #|
    (define (tree23-validate comparator tree)
      (unless (empty? tree)

	(define pass (constant #f))

	(define (assert-in-order a b)
	  (unless (<? comparator a b)
		  (error "order constraint violation")))

	(define (assert-for-all proc node)
	  (for-each proc (tree23->ordered-list node)))

        (let inorder ((tree tree))
	  (case/node ((l x m y r) tree)
            (unileaf (pass))
	    (bileaf  (assert-in-order x y))
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

	(define (depth q)
	  (let recurse ((tree tree) (d 0))
	    (let ((d+1 (add1 d)))
  	      (case/node-query ((l x m y r) tree) comparator q
	        (unileaf   d)
		(bileaf    d)
		(binary:l  (recurse l d+1))
		(binary:x  d)
		(binary:r  (recurse r d+1))
		(trinary:l (recurse l d+1))
		(trinary:x d)
		(trinary:m (recurse m d+1))
		(trinary:y d)
		(trinary:r (recurse r d+1))))))

	(let ((target-depth (depth (tree23-min tree))))
	  (assert-for-all (lambda (q)
			    (= target-depth (depth q)))
			  tree))
	))
    |#

    (define (tree23-validate comparator tree)
      (define pass (constant #f))

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
