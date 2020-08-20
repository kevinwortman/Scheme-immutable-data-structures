;;;; missing from the SRFI 121 reference implementation
#|
(define (list->generator list)
  (let ((list list))
    (lambda ()
      (if (null? list)
	  (eof-object)
	  (let ((obj (car list)))
	    (set! list (cdr list))
	    obj)))))
|#
;;;;

(define *deque-measure*
  (make-measure (lambda (elt) 1)
		+))

(define *mzero* 0)

(define-record-type <ideque>
  (make-ideque length tree)
  ideque?
  (length ideque-length)
  (tree ideque-tree))

(define (length+tree deque)
  (values (ideque-length deque) (ideque-tree deque)))

(define underflow-error (cute error "ideque underflow"))

(define (check-length deque n)
  (when (> n (ideque-length deque))
    (underflow-error)))

(define check-nonempty (cute check-length <> 1))

(define (length+generator->ideque length gen)
  (make-ideque length
	       (generator->finger-tree madd mget gen)))

(define (ideque . elements)
  (list->ideque elements))

(define (ideque-tabulate n proc)
  (length+generator->ideque n (make-tabulation-generator n proc)))

(define (ideque-unfold stop? mapper successor seed)
  (generator->ideque (make-unfold-generator stop? mapper successor seed)))

(define (ideque-unfold-right stop? mapper successor seed)
  (ideque-reverse (ideque-unfold stop? mapper successor seed)))

(define (ideque-empty? deque)
  (finger-tree-empty? (ideque-tree deque)))

(define (ideque-front deque)
  (finger-tree-front (ideque-tree deque)))

(define (ideque-back deque)
  (finger-tree-back (ideque-tree deque)))

(define (ideque-remove-front deque)
  (check-nonempty deque)
  (receive (l t) (length+tree deque)
    (make-ideque (- l 1) (finger-tree-remove-front t))))

(define (ideque-remove-back deque)
  (check-nonempty deque)
  (receive (l t) (length+tree deque)
    (make-ideque (- l 1) (finger-tree-remove-back t))))

(define (ideque-add-front deque obj)
  (receive (l t) (length+tree deque)
    (make-ideque (+ 1 l) (finger-tree-add-front madd mget t obj))))

(define (ideque-add-back deque obj)
  (receive (l t) (length+tree deque)
    (make-ideque (+ 1 l) (finger-tree-add-back madd mget t obj))))

(define (ideque-take deque n)
  (receive (pre suf) (ideque-split-at deque n)
    pre))

(define (ideque-drop deque n)
  (receive (pre suf) (ideque-split-at deque n)
    suf))

(define (split-right deque n)
  (if (zero? n)
      (values deque (ideque))
      (ideque-split-at deque (- (ideque-length deque) n))))

(define (ideque-take-right deque n)
  (receive (pre suf) (split-right deque n)
    suf))

(define (ideque-drop-right deque n)
  (receive (pre suf) (split-right deque n)
    pre))

(define (ideque-split-at deque n)
  (check-length deque n)
  (finger-tree-scan/context madd mget (cute > <> n) mseed (ideque-tree deque)
			    (lambda (pre e suf)
			      (values (make-ideque n pre)
				      (make-ideque (- (ideque-length deque) n)
						   (finger-tree-add-front madd mget suf e))))
			    underflow-error))

(define (ideque-append first . rest)
  (let ((deques (cons first rest)))
    (make-ideque (apply + (map ideque-length deques))
		 (apply finger-tree-append madd mget (map ideque-tree deques)))))

(define (ideque-concatenate list-of-deques)
  (fold (lambda (right left)
	  (ideque-append left right))
	(ideque)
	list-of-deques))

(define (ideque-reverse deque)
  (receive (l t) (length+tree deque)
    (length+generator->ideque l (reverse-finger-tree->generator t))))

(define (ideque-count pred deque)
  (generator-count pred (ideque->generator deque)))

;; TODO ideque-zip

;; TODO: for some reason gmap is not visible here...
(define (gmap proc generator)
  (lambda ()
    (let ((value (generator)))
      (if (eof-object? value)
          value
          (proc value)))))

(define (ideque-map proc first . rest)
  (generator->ideque (apply gmap proc (map! ideque->generator (cons first rest)))))

(define (ideque-for-each proc first . rest)
  (apply generator-for-each proc (map! ideque->generator (cons first rest))))

(define (ideque-fold proc nil first . rest)
  (apply generator-fold proc nil (map! ideque->generator (cons first rest))))

(define (ideque-fold-right proc nil first . rest)
  (apply generator-fold proc nil (map! reverse-ideque->generator (cons first rest))))

;; TODO ideque-append-map

(define (ideque-filter pred deque)
  (generator->ideque (gfilter pred (ideque->generator deque))))

(define (ideque-remove pred deque)
  (ideque-filter (lambda (obj)
		   (not (pred obj)))
		 deque))

(define (ideque-partition pred deque)
  (values (ideque-filter pred deque) (ideque-remove pred deque)))

(define (generator-find/failure pred gen failure)
  (call/cc
   (lambda (return)
     (generator-for-each (lambda (obj)
			   (when (pred obj)
			     (return obj)))
			 gen)
     (failure))))

(define (ideque-find pred deque failure)
  (generator-find/failure pred (ideque->generator deque) failure))

(define (ideque-find-right pred deque failure)
  (generator-find/failure pred (reverse-ideque->generator deque) failure))

(define (ideque-take-while pred deque)
  (generator->ideque (gtake-while pred (ideque->generator deque))))

(define (ideque-take-while-right pred deque)
  (ideque-reverse (generator->ideque (gtake-while pred (reverse-ideque->generator deque)))))

(define (drop-while get drop pred deque)
  (do ((deque deque (drop deque)))
      ((or (ideque-empty? deque)
	   (not (pred (get deque))))
       deque)))

(define ideque-drop-while (cute drop-while ideque-front ideque-remove-front <> <>))

(define ideque-drop-while-right (cute drop-while ideque-back ideque-remove-back <> <>))

(define (ideque-span pred deque)
  (values (ideque-take-while pred deque)
	  (ideque-drop-while pred deque)))

(define (ideque-break pred deque)
  (receive (which-do which-dont) (ideque-span deque)
    (values which-dont which-do)))

(define (ideque-any pred deque)
  (generator-any pred (ideque->generator deque)))

(define (ideque-every pred deque)
  (generator-every pred (ideque->generator deque)))

(define (list->ideque list)
  (generator->ideque (list->generator list)))

(define (ideque->list deque)
  (generator->list (ideque->generator deque)))

(define (generator->ideque gen)
  (let ((tree (generator->finger-tree madd mget gen)))
    (make-ideque (finger-tree-length tree) tree)))

(define (ideque->generator deque)
  (finger-tree->generator (ideque-tree deque)))

(define (reverse-ideque->generator deque)
  (reverse-finger-tree->generator (ideque-tree deque)))

(define (make-ideque-comparator comparator)
  (make-list-comparator ideque?
                        comparator
                        ideque-empty?
                        ideque-front
                        ideque-remove-front))

(define ideque-comparator (make-ideque-comparator default-comparator))
