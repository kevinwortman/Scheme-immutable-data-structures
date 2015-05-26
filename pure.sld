;; -*-Scheme-*-

(define-library (pure)
  (import
   (comparators)
   (generators)
   (scheme base)
   (scheme case-lambda)
   (scheme lazy)
   (scheme write) ; TODO
   (srfi 1)
   (srfi 8)
   (srfi 26))

  (export
   finger-tree/empty
   finger-tree
   finger-tree?
   finger-tree-empty?
   finger-tree-length
   finger-tree-left
   finger-tree-right
   finger-tree-push-left
   finger-tree-push-right
   finger-tree-pop-left
   finger-tree-pop-right
   finger-tree-append
   finger-tree-scan
   finger-tree-scan/context
   generator->finger-tree
   finger-tree->generator
   list->finger-tree
   finger-tree->list)

  (include "pure-impl.scm"))
