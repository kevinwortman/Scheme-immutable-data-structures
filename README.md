Scheme-immutable-data-structures
================================

Kevin A. Wortman
kwortman@gmail.com
kwortman@fullerton.edu

Immutable (a.k.a. persistent or pure-functional) deque, set, and map data structures in portable Scheme.

This is a reference implementation of the proposal at
http://trac.sacrideo.us/wg/wiki/ImmutableDataStructuresWortman

Note that the library spec and this code are both "experimental" and are not in perfect sync.

The deque is based on unlabeled 2-3 finger trees by Hinze and Paterson; the set and map are both based on 1-2 brother trees by Hinze. See the source code for more complete references.

(immutable-structures) is an R7RS library that depends only on R7RS-small and (srfi 1). The unit tests and benchmarks are programs that depend additionally on libraries distributed with Chibi-Scheme.
