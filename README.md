Scheme-immutable-data-structures
================================

Kevin A. Wortman (kwortman@gmail.com, kwortman@fullerton.edu)

Immutable (a.k.a. persistent or pure-functional) deque, set, and map data structures in portable Scheme.

This is a reference implementation of the proposal at
http://trac.sacrideo.us/wg/wiki/ImmutableDataStructuresWortman

Note that the library spec and this code are both *experimental* and are not in perfect sync.

The high level data types are based on a low-level 2-3 finger tree data structure. See the source code for more complete references.

The libraries depend only on R7RS-small and SRFIs 1, 8, 26, and 114, and 121.

The unit tests depend additionally on libraries distributed with Chibi-Scheme.
