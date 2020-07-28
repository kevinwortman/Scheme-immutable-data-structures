;; -*-Scheme-*-

(define-library (immutable set)
  (import
   (scheme comparator)
   (scheme generator)
   (immutable finger-tree)
   (scheme base)
   (scheme case-lambda)
   (scheme write) ; TODO remove
   (srfi 1)
   (srfi 26))

  (export
   iset
   iset-tabulate
   iset-unfold
   iset?
   iset-empty?
   iset-member?
   iset-min
   iset-max
   iset-comparator
   iset-predecessor
   iset-successor
   iset-adjoin
   iset-adjoin-all
   iset-replace
   iset-delete
   iset-delete-elements
   iset-search
   iset-size
   iset-find
   iset-count
   iset-any
   iset-every
   iset-range=
   iset-range<
   iset-range>
   iset-range<=
   iset-range>=
   iset-filter
   iset-remove
   iset-partition
   iset-fold
   iset-fold-right
   iset-map/monotone
   iset-map
   iset-for-each
   iset=?
   iset<?
   iset>?
   iset<=?
   iset>=?
   iset->list
   increasing-list->iset
   list->iset
   iset->generator
   increasing-generator->iset
   generator->iset
   iset-union
   iset-intersection
   iset-difference
   iset-xor)

  (include "set-impl.scm"))
