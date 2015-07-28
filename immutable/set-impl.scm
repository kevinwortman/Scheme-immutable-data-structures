;;;; missing from the SRFI 121 reference implementation

(define (list->generator list)
  (let ((list list))
    (lambda ()
      (if (null? list)
	  (eof-object)
	  (let ((obj (car list)))
	    (set! list (cdr list))
	    obj)))))

(define-record-type <iset>
  (make-iset comparator size tree)
  iset?
  (comparator iset-comparator)
  (size iset-size)
  (tree iset-tree))

(define (comparator+tree->iset comparator tree)
  (make-iset comparator (finger-tree-length tree) tree))

(define (kget e) e) ; identity function

(define empty-error (cute error "unexpected empty iset"))

(define (check-nonempty set)
  (when (iset-empty? set)
    (empty-error)))

(define-syntax let-iset
  (syntax-rules ()
    ((let-iset ((C N T) SET)
       BODY)
     (let ((C (iset-comparator SET))
	   (N (iset-size SET))
	   (T (iset-tree SET)))
       BODY))))

(define iset
  (case-lambda
   ((comparator)
    (make-iset comparator 0 (finger-tree)))
   ((comparator . elements)
    (list->iset comparator elements))))

(define (iset-tabulate comparator n proc)
  (generator->iset comparator (make-tabulation-generator n proc)))

(define (iset-unfold comparator stop? mapper successor seed)
  (generator->iset comparator (make-unfold-generator stop? mapper successor seed)))

(define (iset-empty? set)
  (finger-tree-empty? (iset-tree set)))

(define (iset-member? set obj)
  (let-iset ((c n t) set)
    (pseudoset-finger-tree-find c kget t obj
				(lambda (e)
				  #true)
				(lambda ()
				  #false))))

(define (iset-min set)
  (check-nonempty set)
  (finger-tree-front (iset-tree set)))

(define (iset-max set)
  (check-nonempty set)
  (finger-tree-back (iset-tree set)))

;; predecessor, successor TODO

(define (iset-adjoin set obj)
  (let-iset ((c n t) set)
    (pseudoset-finger-tree-update c kget t obj
				  (lambda (e replace remove)
				    set)
				  (lambda (insert)
				    (make-iset c (+ 1 n) (insert))))))

(define (iset-adjoin-all set list)
  (fold (lambda (obj result)
	  (iset-adjoin result obj))
	set
	list))

(define (iset-replace set obj)
  (let-iset ((c n t) set)
    (pseudoset-finger-tree-update c kget t obj
				  (lambda (e replace remove)
				    (make-iset c n (replace obj)))
				  (lambda (insert)
				    (make-iset c (+ 1 n) (insert))))))

(define (iset-delete set obj)
  (let-iset ((c n t) set)
    (pseudoset-finger-tree-update c kget t obj
				  (lambda (e replace remove)
				    (make-iset c (- n 1) (remove)))
				  (lambda (insert)
				    set))))

(define (iset-delete-elements set list)
  (fold (lambda (obj result)
	  (iset-delete result obj))
	set
	list))

(define (iset-find set obj failure)
  (let-iset ((c n t) set)
    (pseudoset-finger-tree-update c kget t obj
				  (lambda (e replace remove)
				    e)
				  (lambda (insert)
				    (failure)))))

(define (iset-count pred set)
  (generator-count pred (iset->generator set)))

(define (iset-any pred set)
  (generator-any pred (iset->generator set)))

(define (iset-every pred set)
  (generator-every pred (iset->generator set)))

;; range queries TODO

(define (iset-filter pred set)
  (generator->iset (iset-comparator set)
		   (gfilter pred
			    (iset->generator set))))

(define (iset-remove pred set)
  (generator->iset (iset-comparator set)
		   (gremove pred
			    (iset->generator set))))

(define (iset-partition pred set)
  (values (iset-filter pred set)
	  (iset-remove pred set)))

(define (iset-fold proc nil set)
  (generator-fold proc nil (iset->generator set)))

(define (iset-fold-right proc nil set)
  (generator-fold proc nil (reverse-finger-tree->generator (iset-tree set))))

(define iset-map/monotone
  (case-lambda
   ((proc set)
    (iset-map/monotone proc set (iset-comparator set)))
   ((proc set comparator)
    (increasing-generator->iset comparator (gmap proc (iset->generator set))))))

(define iset-map
  (case-lambda
   ((proc set)
    (iset-map proc set (iset-comparator set)))
   ((proc set comparator)
    (generator->iset comparator (gmap proc (iset->generator set))))))

(define (iset-for-each proc set)
  (generator-for-each proc (iset->generator set)))

(define-syntax define-iset-subset
  (syntax-rules ()
    ((define-iset-subset IDENTIFIER FINGER-TREE-PROC)
     (define (IDENTIFIER set1 . rest)
       (apply FINGER-TREE-PROC
	      (iset-comparator set1)
	      kget
	      (iset-tree set1)
	      (map! iset-tree rest))))))

(define-iset-subset iset=? pseudoset-finger-tree=?)
(define-iset-subset iset<? pseudoset-finger-tree<?)
(define-iset-subset iset>? pseudoset-finger-tree>?)
(define-iset-subset iset<=? pseudoset-finger-tree<=?)
(define-iset-subset iset>=? pseudoset-finger-tree>=?)

(define (iset->list set)
  (generator->list (iset->generator set)))

(define (increasing-list->iset comparator list)
  (increasing-generator->iset comparator (list->generator list)))

(define (list->iset comparator list)
  (generator->iset comparator (list->generator list)))

(define (iset->generator set)
  (finger-tree->generator (iset-tree set)))

(define (increasing-generator->iset comparator gen)
  (comparator+tree->iset comparator
			 (increasing-generator->pseudoset-finger-tree gen)))

(define (generator->iset comparator gen)
  (generator-fold (lambda (e set)
		    (iset-adjoin set e))
		  (iset comparator)
		  gen))

(define (merge-left a b) a)

(define (iset-union set1 . rest)
  (let ((cmp (iset-comparator set1)))
    (comparator+tree->iset cmp
			   (apply pseudoset-finger-tree-union
				  merge-left
				  cmp
				  kget
				  (iset-tree set1)
				  (map! iset-tree rest)))))

(define (iset-intersection set1 . rest)
  (let ((cmp (iset-comparator set1)))
    (comparator+tree->iset cmp
			   (apply pseudoset-finger-tree-intersection
				  merge-left
				  cmp
				  kget
				  (iset-tree set1)
				  (map! iset-tree rest)))))

(define (iset-difference set1 . rest)
  (let ((cmp (iset-comparator set1)))
    (comparator+tree->iset cmp
			   (apply pseudoset-finger-tree-difference
				  cmp
				  kget
				  (iset-tree set1)
				  (map! iset-tree rest)))))

(define (iset-xor set1 set2)
  (let ((cmp (iset-comparator set1)))
    (comparator+tree->iset cmp
			   (pseudoset-finger-tree-xor cmp 
						      kget
						      (iset-tree left)
						      (iset-tree right)))))
