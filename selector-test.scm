
(import
 (chibi test)
 (scheme base)
 (selector))

;; make-selector
(define max-selector (make-selector max))
(test-assert (selector? max-selector))
(test-assert (eq? max (selector-procedure max-selector)))

;; selector?
(test-assert (selector? left-selector))
(test-assert (selector? right-selector))
(test-assert (selector? max-selector))
(test-not (selector? '()))
(test-not (selector? max))

;; selector-procedure
(test-assert (procedure? (selector-procedure left-selector)))
(test-assert (procedure? (selector-procedure right-selector)))
(test-assert (eq? max (selector-procedure max-selector)))

;; selector-select
(test 1 (selector-select left-selector 1 2))
(test 2 (selector-select right-selector 1 2))
(test 2 (selector-select max-selector 1 2))
(test 2 (selector-select left-selector 2 1))
(test 1 (selector-select right-selector 2 1))
(test 2 (selector-select max-selector 2 1))

;; define-selector
(define-selector (min-selector left right)
  (min left right))
(test-assert (selector? min-selector))
(test 1 (selector-select min-selector 1 2))
(test 1 (selector-select min-selector 2 1))

;; left-selector
;; right-selector
(test-assert (selector? left-selector))
(test-assert (selector? right-selector))
;; (Correctness was tested above.)
