package linked

import (
	"iter"
)

// Node represents an element of a [List].
type Node[T any] struct {
	peers [2]*Node[T]
	list  *List[T]
	Value T
}

// OK returns true if the receiver is non-nil without causing a panic. This is
// intended for convenience as a shortcut for determining if a [List] operation
// was successful.
func (n *Node[T]) OK() bool { return n != nil }

// All yields the value stored in this [Node] and the value of each [Node]
// which occurs [After] it (i.e. iterates from [Front] to [Back]).
func (n *Node[T]) All() iter.Seq[T] {
	if n != nil {
		return n.all
	}

	return NoopSeq[T]
}

// Backward yields the value stored in this [Node] and the value of each [Node]
// which occurs [Before] it (i.e. iterates from [Back] to [Front]).
func (n *Node[T]) Backward() iter.Seq[T] {
	if n != nil {
		return n.backward
	}

	return NoopSeq[T]
}

// IsEdge indicates if this [Node] is at the given [Edge] of its [List]. The
// edge must be valid, otherwise this method panics.
func (n *Node[T]) IsEdge(edge Edge) bool {
	list := n.list
	return list != nil && n.list.edges[edge] == n
}

// IsFront indicates if this [Node] is at the [Front] of its [List] using
// [Node.IsEdge].
func (n *Node[T]) IsFront() bool { return n.IsEdge(Front) }

// IsBack indicates if this [Node] is at the [Back] of its [List] using
// [Node.IsEdge].
func (n *Node[T]) IsBack() bool { return n.IsEdge(Back) }

// IsMember indicates if this [Node] is a member of the given [List]. If the
// given [List] is nil, indicates if this [Node] is orphaned.
func (n *Node[T]) IsMember(of *List[T]) bool { return n.list == of }

// IsOrphan indicates if this [Node] does not belong to a [List].
func (n *Node[T]) IsOrphan() bool { return n.list == nil }

// Iter returns an [Iterator] which begins at (and is inclusive of) this
// [Node]. Consumes up to one optional argument indicating if reverse
// iteration is desired. Calling this method against a nil receiver never
// panics.
func (n *Node[T]) Iter(backward ...bool) (it Iterator[T]) {
	it.Init(n, backward...)
	return it
}

// Peer returns the [Node] to the [Left] or [Right] edge of this [Node], if
// any. Panics if edge is invalid.
func (n *Node[T]) Peer(edge Edge) *Node[T] { return n.peers[edge] }

// Next returns the [Node] on the [Right] edge of the receiver, if any.
func (n *Node[T]) Next() *Node[T] { return n.peers[Next] }

// Prev returns [Node] on the [Left] edge of the receiver, if any.
func (n *Node[T]) Prev() *Node[T] { return n.peers[Prev] }

// Move moves this [Node] to the [Left] or [Right] [Edge] of the target [Node]
// using [List.Move]. On success, returns the receiver, otherwise nil.
func (n *Node[T]) Move(target *Node[T], edge Edge) *Node[T] {
	if list := n.list; list != nil {
		return list.Move(n, target, edge)
	}

	return nil
}

// MoveBefore moves this [Node] to the [Left] [Edge] of the target [Node]
// using [Node.Move].
func (n *Node[T]) MoveBefore(target *Node[T]) *Node[T] { return n.Move(target, Before) }

// MoveAfter moves this [Node] to the [Right] [Edge] of the target [Node]
// using [Node.Move].
func (n *Node[T]) MoveAfter(target *Node[T]) *Node[T] { return n.Move(target, After) }

// MoveToEdge moves this [Node] to the [Front] or [Back] edge of its [List].
// Returns the receiver if this [Node] belongs to any [List], otherwise
// nil. Edge must be [Left], [Right], or an alias thereof.
func (n *Node[T]) MoveToEdge(edge Edge) *Node[T] {
	list := n.list
	if list == nil {
		return nil
	} else if edge.Must() == Front {
		return list.MoveToFront(n)
	}

	return list.MoveToBack(n)
}

// MoveToFront moves this [Node] to the [Front] of its [List] using
// [Node.MoveToEdge].
func (n *Node[T]) MoveToFront() *Node[T] {
	if list := n.list; list != nil {
		return list.MoveToFront(n)
	}

	return nil
}

// MoveToBack moves this [Node] to the [Back] of its [List] using
// [Node.MoveToEdge].
func (n *Node[T]) MoveToBack() *Node[T] {
	if list := n.list; list != nil {
		return list.MoveToBack(n)
	}

	return nil
}

// Pop removes this [Node] from the [List] to which it belongs, if any. Always
// returns itself.
func (n *Node[T]) Pop() *Node[T] {
	if l := n.list; l != nil {
		l.remove(&span[T]{n, n}, false)
	}

	return n
}

// Remove removes this [Node] from the [List] to which it belongs (if any) and
// then returns the value it stores.
func (n *Node[T]) Remove() T { return n.Pop().Value }

func (n *Node[T]) all(yield func(T) bool) {
	for n := n; n != nil && yield(n.Value); n = n.peers[Right] {
	}
}

func (n *Node[T]) backward(yield func(T) bool) {
	for n := n; n != nil && yield(n.Value); n = n.peers[Left] {
	}
}

// NoopSeq is a no-op [iter.Seq] that never yields anything.
func NoopSeq[T any](_ func(T) bool) {}
