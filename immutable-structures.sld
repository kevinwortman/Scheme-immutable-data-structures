;; -*-Scheme-*-

(define-library (immutable-structures)
  (import (scheme base)
	  (srfi 1))

  (include "immutable-structures-impl.scm")

  (export

   ideque
   ideque?
   ideque-empty?
   ideque-length
   ideque->list
   list->ideque
   ideque-front
   ideque-back
   ideque-push-front
   ideque-push-back
   ideque-pop-front
   ideque-pop-back
   ideque-append

   iset?
   iset/merger
   iset
   iset-empty?
   iset-size
   iset-precedes?
   iset-xor
   iset-difference
   iset-union
   iset-intersection
   iset-member?
   iset-update
   iset-find
   iset-include
   iset-exclude
   iset-filter
   iset-fold
   iset-map/monotone
   iset-map
   iset->ordered-list
   ordered-list->iset
   list->iset/merger
   list->iset
   iset<?
   iset<=?
   iset=?
   iset>=?
   iset>?

   imap?
   imap-empty?
   imap-size
   imap-member?
   imap-min-key
   imap-max-key
   imap-min-value
   imap-max-value
   imap-update
   imap-find
   imap-include
   imap-exclude
   imap-filter
   imap-fold
   imap-map/monotone
   imap-map
   imap-map-values
   imap->ordered-alist
   imap-values
   imap-keys
   ordered-alist->imap
   alist->imap
   imap=?
   ))
