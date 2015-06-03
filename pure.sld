;; -*-Scheme-*-

(define-library (pure)
  (import
   (comparators)
   (generators)
   (scheme base)
   (scheme case-lambda)
   (scheme lazy)
   (scheme write) ; TODO remove
   (srfi 1)
   (srfi 8)
   (srfi 26))

  ;; ideque

  (include "pure-impl.scm"))
