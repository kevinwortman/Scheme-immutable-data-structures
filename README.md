Scheme-immutable-data-structures
================================

Kevin A. Wortman

kwortman@gmail.com

kwortman@fullerton.edu

Immutable (a.k.a. persistent or pure-functional) deque, set, and map data structures in portable Scheme.

This is a reference implementation of the proposal at
http://trac.sacrideo.us/wg/wiki/ImmutableDataStructuresWortman

Note that the library spec and this code are both "experimental" and are not in perfect sync.

The deque is based on unlabeled 2-3 finger trees; the set and map are both based on 2-3 binary search trees. See the source code for more complete references.

The libraries depend only on R7RS-small and SRFIs 1, 26, and 114.

The unit tests depend additionally on libraries distributed with
Chibi-Scheme.
