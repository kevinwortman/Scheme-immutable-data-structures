;; -*-Scheme-*-

(define-library (immutable finger-tree)
  (import
   (scheme comparator)
   (scheme generator)
   (scheme base)
   (scheme case-lambda)
   (scheme lazy)
   (srfi 1)
   (srfi 26))

  (export

   ;; measure
   make-measure
   measure?
   measure-get-proc
   measure-add-proc
   measure-get
   measure-add
   measure-get+add

   ;; construction and basic properties
   make-finger-tree
   finger-tree
   finger-tree?
   finger-tree-empty?
   finger-tree-non-empty?
   finger-tree-length

   ;; manipulating the left or right element
   finger-tree-left
   finger-tree-right
   finger-tree-add-left
   finger-tree-add-right
   finger-tree-remove-left
   finger-tree-remove-right

   ;; fundamental operations
   finger-tree-append
   finger-tree-scan
   finger-tree-bisect
   finger-tree-reverse

   ;; import
   generator->finger-tree
   list->finger-tree
   vector->finger-tree

   ;; export
   finger-tree->generator
   finger-tree->reverse-generator
   finger-tree->list
   finger-tree->vector

   ;; set order
   make-set-order
   set-order?
   set-order-measure
   set-order-comparator
   set-order-key
   
   ;; set operations on individual elements
   finger-tree-set-search
   finger-tree-set-update
   finger-tree-set-adjoin
   finger-tree-set-replace
   finger-tree-set-delete
   finger-tree-set-predecessor
   finger-tree-set-successor

   ;; set relations
   finger-tree-set<?
   finger-tree-set<=?
   finger-tree-set=?
   finger-tree-set>=?
   finger-tree-set>?

   ;; set operations
   finger-tree-set-union
   finger-tree-set-intersect
   finger-tree-set-difference
   finger-tree-set-xor

   ;; set import
   increasing-generator->finger-tree-set
   increasing-list->finger-tree-set
   increasing-vector->finger-tree-set
   generator->finger-tree-set
   list->finger-tree-set
   vector->finger-tree-set
   )

  (include "finger-tree-impl.scm"))
