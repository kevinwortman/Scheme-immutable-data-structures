
(import
 (chibi loop)
 (chibi test)
 (ideque)
 (scheme base)
 (scheme write)
 (srfi 1)
 (util))

;; ideque
;; ideque-length
(define empty (ideque))
(test-assert (ideque? empty))
(test-assert (ideque-empty? empty))
(define three (ideque 0 1 2))
(test-assert (ideque? three))
(test 3 (ideque-length three))
(define mil/list (iota 1000))
(define mil (list->ideque mil/list))
(test-assert (ideque? mil))
(test 1000 (ideque-length mil))

;; ideque?
(test-assert (ideque? empty))
(test-assert (ideque? three))
(test-not (ideque? '()))
(test-not (ideque? #f))

;; ideque-empty?
(test-assert (ideque-empty? empty))
(test-not (ideque-empty? three))

;; ideque-front
;; ideque-back
(test-error (ideque-front empty))
(test-error (ideque-back empty))
(test 0 (ideque-front three))
(test 2 (ideque-back three))
(test 0 (ideque-front mil))
(test 999 (ideque-back mil))

;; ideque-push-front
(let ((dq (ideque-push-front empty 'x)))
  (test-not (ideque-empty? dq))
  (test 1 (ideque-length dq))
  (test '(x) (ideque->list dq)))
(let ((dq (ideque-push-front three 'x)))
  (test-not (ideque-empty? dq))
  (test 4 (ideque-length dq))
  (test '(x 0 1 2) (ideque->list dq)))
(let ((dq (ideque-push-front mil 'x)))
  (test-not (ideque-empty? dq))
  (test 1001 (ideque-length dq))
  (test (cons 'x mil/list) (ideque->list dq)))

;; ideque-push-back
(let ((dq (ideque-push-back empty 'x)))
  (test-not (ideque-empty? dq))
  (test 1 (ideque-length dq))
  (test '(x) (ideque->list dq)))
(let ((dq (ideque-push-back three 'x)))
  (test-not (ideque-empty? dq))
  (test 4 (ideque-length dq))
  (test '(0 1 2 x) (ideque->list dq)))
(let ((dq (ideque-push-back mil 'x)))
  (test-not (ideque-empty? dq))
  (test 1001 (ideque-length dq))
  (test (append mil/list '(x)) (ideque->list dq)))

;; ideque-pop-front
(test-error (ideque-pop-front empty))
(let ((dq (ideque-pop-front three)))
  (test-not (ideque-empty? dq))
  (test 2 (ideque-length dq))
  (test '(1 2) (ideque->list dq)))
(let ((dq (ideque-pop-front mil)))
  (test-not (ideque-empty? dq))
  (test 999 (ideque-length dq))
  (test (cdr mil/list) (ideque->list dq)))

;; ideque-pop-back
(test-error (ideque-pop-back empty))
(let ((dq (ideque-pop-back three)))
  (test-not (ideque-empty? dq))
  (test 2 (ideque-length dq))
  (test '(0 1) (ideque->list dq)))
(let ((dq (ideque-pop-back mil)))
  (test-not (ideque-empty? dq))
  (test 999 (ideque-length dq))
  (test (drop-right mil/list 1) (ideque->list dq)))

;; ideque-take-front
;; ideque-take-front/list
;; ideque-take-front/values
(test-error (ideque-take-front empty 1))
(test '() (ideque->list (ideque-take-front three 0)))
(test '() (ideque-take-front/list three 0))
(test '(0) (ideque->list (ideque-take-front three 1)))
(test '(0) (ideque-take-front/list three 1))
(test-values (values 0) (ideque-take-front/values three 1))
(test '(0 1) (ideque->list (ideque-take-front three 2)))
(test '(0 1) (ideque-take-front/list three 2))
(test-values (values 0 1) (ideque-take-front/values three 2))
(test '(0 1 2) (ideque->list (ideque-take-front three 3)))
(test '(0 1 2) (ideque-take-front/list three 3))
(test-values (values 0 1 2) (ideque-take-front/values three 3))
(test-error (ideque-take-front three 4))
(test-error (ideque-take-front/list three 4))
(test-error (ideque-take-front/values three 4))

;; ideque-take-back
;; ideque-take-back/list
;; ideque-take-back/values
(test-error (ideque-take-back empty 1))
(test '() (ideque->list (ideque-take-back three 0)))
(test '() (ideque-take-back/list three 0))
(test-values (values) (ideque-take-back/values three 0))
(test '(2) (ideque->list (ideque-take-back three 1)))
(test '(2) (ideque-take-back/list three 1))
(test-values (values 2) (ideque-take-back/values three 1))
(test '(1 2) (ideque->list (ideque-take-back three 2)))
(test '(1 2) (ideque-take-back/list three 2))
(test-values (values 1 2) (ideque-take-back/values three 2))
(test '(0 1 2) (ideque->list (ideque-take-back three 3)))
(test '(0 1 2) (ideque-take-back/list three 3))
(test-values (values 0 1 2) (ideque-take-back/values three 3))
(test-error (ideque-take-back three 4))
(test-error (ideque-take-back/list three 4))
(test-error (ideque-take-back/values three 4))

;; ideque-append
;; two arguments
(test '() (ideque->list (ideque-append empty empty)))
(test '(0 1 2) (ideque->list (ideque-append empty three)))
(test '(0 1 2) (ideque->list (ideque-append three empty)))
(test '(0 1 2 0 1 2) (ideque->list (ideque-append three three)))
(test '(0 1 2 x y) (ideque->list (ideque-append three (ideque 'x 'y))))
(test '(x y 0 1 2) (ideque->list (ideque-append (ideque 'x 'y) three)))
;; multiple arguments
(test '(7 8 1 2 3 0 1) (ideque->list (ideque-append (ideque 7 8) (ideque 1 2 3) (ideque 0 1))))
;; various lengths
(loop ((for i (up-from 0 (to 20))))
      (loop ((for j (up-from 0 (to 20))))
	    (let ((l (iota i))
		  (r (iota j)))
	    (test (append (iota i) (iota j))
		  (ideque->list (ideque-append (list->ideque l) (list->ideque r)))))))

;; ideque-filter
;; ideque-fold
;; ideque-map
(test (filter even? mil/list) (ideque->list (ideque-filter even? mil)))
(test (fold cons '() mil/list) (ideque-fold cons '() mil))
(test (map add1 mil/list) (ideque->list (ideque-map add1 mil)))

;; list->ideque
;; ideque->list
(test '(0) (ideque->list (ideque 0)))
(test '(0 1 2) (ideque->list three))
(test mil/list (ideque->list mil))
(loop ((for n (up-from 0 (to 100))))
      (let* ((lst (iota n))
	     (dq (list->ideque lst)))
	(test (zero? n) (ideque-empty? dq))
	(test n (ideque-length dq))
	(test lst (ideque->list dq))
	(test lst (ideque->list (list->ideque lst)))))
