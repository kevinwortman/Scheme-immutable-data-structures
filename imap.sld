
(define-library (imap)
  (import
   (comparators)
   (iset)
   (scheme base)
   (selector)
   (srfi 1)
   (srfi 8)
   (srfi 26)
   (util))

  (export

   imap
   imap?

   imap-key-comparator
   imap-empty?
   imap-size

   imap-member?
   imap-min-key
   imap-max-key
   imap-min-value
   imap-max-value

   imap-ref
   imap-set
   imap-exclude

   imap-between

   imap-filter
   imap-fold
   imap-map-keys
   imap-map-keys/nondecreasing
   imap-map-values
   imap-map
   imap-map/nondecreasing

   imap->ordered-alist
   imap-keys
   imap-values
   ordered-alist->imap
   alist->imap)

  (begin

    (define make-entry  cons)
    (define entry-key   car)
    (define entry-value cdr)

    (define-syntax-rule (let/entry ((KEY VALUE) ENTRY) BODY)
      (let* ((e ENTRY)
	     (KEY (entry-key e))
	     (VALUE (entry-value e)))
	BODY))

    (define (make-entry-map f)
      (lambda (entry)
	(let/entry ((key value) entry)
	  (receive (key value) (f key value)
            (make-entry key value)))))

    (define sentinel (cute make-entry <> #false))

    (define-record-type <imap>
      (make-imap key-comparator entries)
      imap?
      (key-comparator imap-key-comparator)
      (entries imap-entries))

    (define (make-importer alist->iset)
      (lambda (key-comparator alist)
	(make-imap key-comparator
		   (alist->iset (make-car-comparator key-comparator)
				right-selector
				alist))))

    (define from-ordered-alist (make-importer ordered-list->iset))
    (define from-alist (make-importer list->iset))

    (define (replace-entries like entries)
      (make-imap (imap-key-comparator like) entries))

    (define (imap . args)
      (if (or (null? args)
	      (not (comparator? (first args))))
	  (from-alist default-comparator args)
	  (from-alist (first args) (cdr args))))

    (define (imap-empty? map)
      (iset-empty? (imap-entries map)))

    (define (imap-size map)
      (iset-size (imap-entries map)))

    (define (imap-member? map key)
      (iset-member? (imap-entries map) (sentinel key)))

    (define (imap-min-key map)
      (entry-key (iset-min (imap-entries map))))

    (define (imap-max-key map)
      (entry-key (iset-max (imap-entries map))))

    (define (imap-min-value map)
      (entry-value (iset-min (imap-entries map))))

    (define (imap-max-value map)
      (entry-value (iset-max (imap-entries map))))

    (define (imap-ref map key absent-proc)
      (if (imap-member? map key)
	  (entry-value (iset-find (imap-entries map)
				  (sentinel key)
				  (cute error "unreachable state")))
	  (absent-proc)))

    (define (imap-set map key value)
      (replace-entries
       map
       (iset-include (imap-entries map)
		     (make-entry key value))))

    (define imap-exclude
      (left-associative
       (lambda (map key)
	 (replace-entries
	  map
	  (iset-exclude (imap-entries map)
			(sentinel key))))))

    (define (imap-between map min min-inclusive max max-inclusive)
      (replace-entries
       map
       (iset-between (imap-entries map)
		     (sentinel min) min-inclusive
		     (sentinel max) max-inclusive)))

    (define (imap-filter f map)
      (replace-entries
       map
       (iset-filter (lambda (entry)
		      (let/entry ((key value) entry)
		        (f key value)))
		    (imap-entries map))))

    (define (imap-fold f knil map)
      (iset-fold (lambda (entry accum)
		   (let/entry ((key value) entry)
		     (f key value accum)))
		 knil
		 (imap-entries map)))

    (define (imap-map f map)
      (replace-entries 
       map
       (iset-map (make-entry-map f)
		 (imap-entries map))))

    (define (imap-map/nondecreasing f map)
      (replace-entries
       map
       (iset-map/nondecreasing (make-entry-map f)
			       (imap-entries map))))

    (define (imap-map-keys f map)
      (imap-map (lambda (key value)
		  (values (f key) value))
		map))

    (define (imap-map-keys/nondecreasing f map)
      (imap-map/nondecreasing (lambda (key value)
				(values (f key) value))
			      map))

    (define (imap-map-values f map)
      (imap-map/nondecreasing (lambda (key value)
				(values key (f value)))))

    (define (imap->ordered-alist map)
      (iset->ordered-list (imap-entries map)))

    (define (imap-keys map)
      (iset-map/nondecreasing entry-key (imap-entries map)))

    (define (imap-values map)
      (reverse! (iset-fold (lambda (e result)
			     (cons (entry-value e) result))
			   '()
			   (imap-entries map))))

    (define-syntax-rule (define-importer IDENTIFIER FROM-PROC)
      (define IDENTIFIER
	(lambda (left . rest)
	  (if (null? rest)
	      (FROM-PROC default-comparator left)
	      (FROM-PROC left (car rest))))))
      
    (define-importer ordered-alist->imap from-ordered-alist)
    (define-importer alist->imap from-alist)

    ))
