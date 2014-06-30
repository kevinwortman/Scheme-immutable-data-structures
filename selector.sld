
(define-library (selector)
  (import
   (scheme base)
   (util))

  (export
   make-selector
   selector?
   selector-procedure
   selector-select
   define-selector
   left-selector
   right-selector)

  (begin
    (define-record-type <selector>
      (make-selector procedure)
      selector?
      (procedure selector-procedure))

    (define (selector-select selector left right)
      ((selector-procedure selector) left right))

    (define-syntax-rule (define-selector (IDENTIFIER LEFT RIGHT) BODY ...)
      (define IDENTIFIER (make-selector (lambda (LEFT RIGHT) BODY ...))))

    (define-selector (left-selector left right)
      left)

    (define-selector (right-selector left right)
      right)
    
    ))
