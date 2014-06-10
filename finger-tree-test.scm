
(import
 (finger-tree)
 (chibi test)
 (scheme base)
 (scheme write)
 (srfi 1)
 (util))

(define empty (make-finger-tree))
(define five-list (iota 5))
(define five (list->finger-tree five-list))
(define century-list (iota 100))
(define century (list->finger-tree century-list))

;; make-finger-tree
(test #true (finger-tree? (make-finger-tree)))

;; finger-tree?
(test #true (finger-tree? empty))
(test #true (finger-tree? five))
(test #true (finger-tree? century))
(test #false (finger-tree? '(1 2 3)))
(test #false (finger-tree? 7))
(test #false (finger-tree? #false))

;; finger-tree-empty?
(test #true (finger-tree-empty? empty))
(test #false (finger-tree-empty? century))

;; finger-tree-length
(test 0 (finger-tree-length empty))
(test 5 (finger-tree-length five))
(test 100 (finger-tree-length century))

;; finger-tree-front
(test 0 (finger-tree-front five))
(test 0 (finger-tree-front century))

;; finger-tree-back
(test 4 (finger-tree-back five))
(test 99 (finger-tree-back century))

;; finger-tree-push-front
;; finger-tree-push-back
(let loop ((i 0)
	   (front-list '())
	   (back-list '())
	   (front-tree (make-finger-tree))
	   (back-tree (make-finger-tree)))
  (unless (= i 1000)
    (test front-list (finger-tree->list front-tree))
    (test back-list (finger-tree->list back-tree))
    (loop (add1 i)
	  (cons i front-list)
	  (append back-list (list i))
	  (finger-tree-push-front front-tree i)
	  (finger-tree-push-back back-tree i))))

;; finger-tree-pop-front
;; finger-tree-pop-back
(let loop ((front-list century-list)
	   (back-list century-list)
	   (front-tree century)
	   (back-tree century))
  (unless (finger-tree-empty? front-tree)
    (test front-list (finger-tree->list front-tree))
    (test back-list (finger-tree->list back-tree))
    (loop (cdr front-list)
	  (drop-right back-list 1)
	  (finger-tree-pop-front front-tree)
	  (finger-tree-pop-back back-tree))))

;; finger-tree-append
;; TODO failing
(let loop ((i 0)
	   (lin-list five-list)
	   (exp-list five-list)
	   (lin-tree five)
	   (exp-tree five))
  (unless (= i 2)
    (test lin-list (finger-tree->list lin-tree))
    (test exp-list (finger-tree->list exp-tree))
    (loop (add1 i)
	  (append lin-list five-list)
	  (append exp-list exp-list)
	  (finger-tree-append lin-tree five)
	  (finger-tree-append exp-tree exp-tree))))

;; finger-tree-filter
(test (filter even? century-list)
      (finger-tree->list (finger-tree-filter even? century)))

;; finger-tree-fold
(test (fold + 0 century-list)
      (finger-tree-fold + 0 century))
(test (fold cons '() century-list)
      (finger-tree-fold cons '() century))

;; finger-tree-map
(test (map add1 century-list)
      (finger-tree->list (finger-tree-map add1 century)))

;; finger-tree->list
(test '() (finger-tree->list empty))
(test five-list (finger-tree->list five))
(test century-list (finger-tree->list century))

;; list->finger-tree
(test century-list
      (finger-tree->list (list->finger-tree century-list)))

(let ((n (* 1000 1000)))
  (newline)
  (timer (string-append "finger-tree->list, n = "
			(number->string n))
	 (list->finger-tree (iota n))))
