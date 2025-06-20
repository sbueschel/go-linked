package linked

import "iter"

// flags used by the [Iterator] (see Iterator.flags)
const (
	_ = 1 << iota
	iterStarted
)

// Iterator is a simple object-based iterator that provides leftward and
// rightward traversal of the [Node] objects found in a [List].
//
// Note: this object does *NOT* track any of the [Node]s that it has visited,
// and is therefore sensitive to any mutations of the associated [List].
type Iterator[T any] struct {
	// Node is the current position of the iterator. When nil, iteration has
	// completed. For performance reasons, the field is public, rather than
	// wrapped as a method.
	//
	// It is discouraged (but not always incorrect) to assign directly to this
	// field once iteration has started.
	Node *Node[T]

	// Pos is a pseudo-index which tracks the iteration position relative to
	// the origin [Node]. For performance reasons, the field is public, rather
	// than wrapped as a method.
	//
	// It is discouraged to assign directly to this field.
	Pos int

	// version of the associated [List]] when iteration began.
	version uint

	// flags is a bit mask containing state/config.
	//
	// The lowest bit (bit 0) is special; it indicates the direction of
	// traversal as the INVERSE of [Left] or [Right]. Why? Because, the
	// zero value should traverse from [Left] -> [Right] by default, as
	// is the norm for slice iteration. Therefore, when decoding this
	// bit to get the neighbor index of the next node, be sure to flip
	// it like so:
	//
	// 	opposite[flags&1]
	//
	// bit 1 indicates if iteration has started.
	flags uint
}

// Init initializes the [Iterator], beginning at the given [Node]. Iteration
// occurs from [Left] to [Right] unless backward is true.
func (it *Iterator[T]) Init(node *Node[T], backward ...bool) {
	it.init(node, btoi[Edge](len(backward) > 0 && backward[0]))
}

// All consumes this [Iterator] to yield all remaining [Node]s.
func (it *Iterator[T]) All() iter.Seq[*Node[T]] { return it.all }

// Next advances the iterator. It returns true if the [Node] on the [Iterator]
// can be safely read or false if it will be nil.
func (it *Iterator[T]) Next() bool {
	if node := it.Node; node == nil {
		return false
	} else if flags := it.flags; flags&iterStarted == 0 {
		it.flags |= iterStarted
		return true
	} else if node = node.peers[opposite[flags&1]]; node != nil {
		it.Node = node
		it.Pos++
		return true
	}

	it.Node = nil
	return false
}

// Count returns the number of times this [Iterator] has been successfully
// advanced.
func (it *Iterator[T]) Count() (n int) {
	if it.flags&iterStarted != 0 {
		n = it.Pos + 1
	}

	return n
}

// IsBackward indicates if this [Iterator] is iterating in reverse order.
func (it *Iterator[T]) IsBackward() bool { return it.flags&1 != 0 }

// Gather expends the remainder of this [Iterator], placing the value of each
// remaining [Node] into a slice. The slice is nil if the iterator is spent.
func (it *Iterator[T]) Gather() (values []T) {
	if n := it.getGatherSizeFast(); n > 0 {
		values = make([]T, 0, n)
	}

	for it.Next() {
		values = append(values, it.Node.Value)
	}

	return values
}

// Into advances the iterator up to len(dst) times, placing the value of each
// yielded [Node] into the given slice at the next position. Returns the number
// of values placed into the slice.
func (it *Iterator[T]) Into(dst []T) (n int) {
	for n < len(dst) && it.Next() {
		dst[n] = it.Node.Value
		n++
	}

	return n
}

// Version returns the value of [List.Version] at the time [Iterator.Init] was
// called, useful for detecting if the [List] has mutated (though it won't tell
// you where or how).
func (it *Iterator[T]) Version() uint { return it.version }

func (it *Iterator[T]) init(node *Node[T], edge Edge) {
	var version uint
	it.Node = node
	it.Pos = 0
	it.flags = uint(edge)
	if node != nil && node.list != nil {
		version = node.list.version
	}

	it.version = version
}

func (it *Iterator[T]) all(yield func(*Node[T]) bool) {
	for it.Next() && yield(it.Node) {
	}
}

// getGatherSizeFast returns the length of the current [Node]'s list when
// iteration has not started, it is the [Front] or [Back] of its list, and
// this [Iterator] is traversing in the opposite direction of the node's
// position in the list.
func (it *Iterator[T]) getGatherSizeFast() int {
	n := it.Node
	if n == nil || n.list == nil || it.version != 0 {
		return 0
	} else if list, ends := n.list, n.list.edges; n == ends[Front] && it.flags&1 == uint(Front) {
		return list.length
	} else if n == ends[Back] && it.flags&1 == uint(Back) {
		return list.length
	}

	return 0
}

type integer interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
		~uintptr
}

func btoi[T integer](b bool) T {
	if b {
		return 1
	}

	return 0
}
