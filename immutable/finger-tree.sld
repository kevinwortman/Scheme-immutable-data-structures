;; -*-Scheme-*-

(define-library (immutable finger-tree)
  (import
   (comparators)
   (generators)
   (scheme base)
   (scheme case-lambda)
   (scheme lazy)
   (srfi 1)
   (srfi 8)
   (srfi 26))

  (export
   finger-tree
   finger-tree?
   finger-tree-empty?
   finger-tree-length
   finger-tree-front
   finger-tree-back
   finger-tree-add-front
   finger-tree-add-back
   finger-tree-remove-front
   finger-tree-remove-back
   generator->finger-tree
   finger-tree->generator
   reverse-finger-tree->generator
   list->finger-tree
   finger-tree->list
   finger-tree-append
   finger-tree-scan
   finger-tree-scan/context
   pseudoset-finger-tree-find
   pseudoset-finger-tree-update
   pseudoset-finger-tree=?
   pseudoset-finger-tree<?
   pseudoset-finger-tree<=?
   pseudoset-finger-tree>?
   pseudoset-finger-tree>=?
   pseudoset-finger-tree-union
   pseudoset-finger-tree-intersection
   pseudoset-finger-tree-difference
   pseudoset-finger-tree-xor
   increasing-generator->pseudoset-finger-tree)
  
  (include "finger-tree-impl.scm"))
