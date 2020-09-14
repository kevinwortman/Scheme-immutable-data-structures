# finger tree


## Status

early draft

## Abstract

Generic immutable (a.k.a. persistent or pure-functional) data
structure. It is generic because it can be used to implement
efficiently other immutable data structures like set and deque.

## Rationale

...

## Specification

### `(make-measure get-proc add-proc zero)`

Return a measure object with the given `GET-PROC`, `ADD-PROC` and `ZERO`.

### `(measure? obj)`

Return `#t` if `OBJ` is a measure. Otherwise, it returns `#f`.

### `(measure-get-proc measure)`

Return

### `(measure-add-proc measure)`

Return 

### `(measure-get measure element)`

### `(measure-add measure m1 m2 [m3])`

### `(measure-get+add measure m1 element)`

### `(make-finger-tree)`

Construct an empty finger-tree.

### `(finger-tree measure . elements)`

Construct a finger-tree with `MEASURE` with the given `ELEMENTS`.

### `(finger-tree? obj)`

Return `#t` if `OBJ` is a finger-tree. Otherwise, it returns `#f`.

### `(finger-tree-empty? finger-tree)`

Return `#t` if `FINGER-TREE` is an empty finger-tree. Otherwise, it
returns `#f`.

### `(finger-tree-length finger-tree)`

Return the length of `FINGER-TREE`.

### `(finger-tree-left finger-tree)`

Return the left most element of `FINGER-TREE`.

### `(finger-tree-right finger-tree)`

Return the right most element of `FINGER-TREE`.

### `(finger-tree-add-left finger-tree obj)`

Return a finger-tree based on `FINGER-TREE` where `OBJ` is the left
most element.

### `(finger-tree-add-right finger-tree obj)`

Return a finger-tree based on `FINGER-TREE` where `OBJ` is the right
most element.

### `(finger-tree-remove-left finger-tree)`

Return a finger-tree based on `FINGER-TREE` where the left most
element was removed.

### `(finger-tree-remove-right finger-tree)`

Return a finger-tree based on `FINGER-TREE` where the right most
element was removed.

### `(finger-tree-append measure finger-tree . finger-trees)`

Return a new finger-tree made of `FINGER-TREE` and `FINGER-TREES`
using `MEASURE`.

### `(finger-tree-scan measure mpred finger-tree match absent)`

Performs a *scan* on tree. This is a very general operation whose
meaning depends on the measure in use. In a set-like tree, scan is
analogous to a search operation. In a vector-like tree, scan is
analogous to vector-ref.

A scan works like a fold that accumulates a measurement value,
starting from (measurement-zero meas). Elements are visited in
left-to-right order. Each element is measured, and that measurement is
added to the accumulated measurement. The element counts as a match if
(mpred <accumulated-measure>) yields true. mpred must be *monotone* in
the sense that it is false for very low measurments, true for very
high measurements, and transitions from false to true at most once. In
most applications, mpred tests whether the measurement is >= some
measurement value.

### `(finger-tree-bisect measure mpred finger-tree match absent)`

Bisect a tree. This is similar to the scan operation implemented in
finger-tree-scan. The definition for meas, mpred, tree, and absent
are identical to finger-tree-scan. The difference is in match.

On success, calls:

```scheme
(match m prefix suffix)
```

Where

- m is the accumulated measurement *before* the matching element,
  as in a scan.

- prefix is a tree containing all elements before the match. This
  may be empty if the match is the left element of the original
  tree.

- suffix is a tree containing the match and all elements
  after. This will never be empty since it always contains at least
  the matching element.

Just like scan, this is a very general operation whose effect depends
on the measurement in use. Depending on the measurement, it could be
used for a split-at, predecessor, successor, or tree removal
operation.

Time efficiency is O(log n), but more expensive than finger-tree-scan,
so only use finger-tree-bisect when you actually need the prefix or
suffix.

### `(finger-tree-reverse measure finger-tree)`

Reverse the `FINGER-TREE`.

### `(generator->finger-tree measure generator)`

Create a finger-tree with `MEASURE` and the elements coming from
`GENERATOR`.

### `(list->finger-tree measure lst)`

Create a finger-tree with `MEASURE` and the elements from `LST`.

### `(vector->finger-tree measure vector)`

Create a finger-tree with `MEASURE` and the elements from `VECTOR`.

### `(finger-tree->generator finger-tree)`

Create a generator with elements from `FINGER-TREE` from left to right.

### `(finger-tree->reverse-generator finger-tree)`

Create a generator with elements from `FINGER-TREE` from right to left.

### `(finger-tree->list finger-tree)`

Create a list based on `FINGER-TREE`.

### `(finger-tree->vector finger-tree)`

Create a vector based on `FINGER-TREE`.

### `(make-set-order measure comparator)`

### `(set-order? obj)`

### `(set-order-measure set-order)`

### `(set-order-comparator set-order)`

### `(set-order-key set-order element)`

### `(finger-tree-set-search order tree key match absent)`

### `(finger-tree-set-update order tree element match absent)`

### `(finger-tree-set-adjoin order tree element)`

### `(finger-tree-set-replace order tree element)`

### `(finger-tree-set-delete order tree element)`

### `(finger-tree-set-predecessor order tree key match absent)`

### `(finger-tree-set-successor order tree key match absent)`

### `(finger-tree-set<? order set1 set2 . sets)`

### `(finger-tree-set<=? order set1 set2 . sets)`

### `(finger-tree-set=? order set1 set2 . sets)`

### `(finger-tree-set>=? order set1 set2 . sets)`

### `(finger-tree-set>? order set1 set2 . sets)`

### `(finger-tree-set-union order set1 set2 . sets)`

### `(finger-tree-set-intersect order set1 set2 . sets)`

### `(finger-tree-set-difference order set1 set2 . sets)`

### `(finger-tree-set-xor order set1 set2 . sets)`

### `(increasing-generator->finger-tree-set order generator)`

### `(increasing-list->finger-tree-set order lst)`

### `(increasing-vector->finger-tree-set order vector)`

### `(generator->finger-tree-set order reduce generator)`

### `(list->finger-tree-set order reduce list)`

### `(vector->finger-tree-set order reduce vector)`
