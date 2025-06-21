// Package linked provides a doubly-linked list implementation with support for
// generics.
package linked

import (
	"iter"
)

// List implements a doubly-linked list. The zero value of a [List] is ready
// to use. Must not be modified concurrently.
type List[T any] struct {
	edges   span[T]
	length  int
	version uint
}

// New creates a new doubly-linked [List], initially populated with the values
// of the given slice (which may be nil). If backward is true, they are placed
// into the [List] in reverse order.
func New[T any](values []T, backward ...bool) *List[T] {
	var l List[T]
	l.edges, l.length = newSpan(&l, values, len(backward) > 0 && backward[0])
	return &l
}

// All iterates over all values in this [List], from [Front] to [Back].
func (l *List[T]) All() iter.Seq[T] { return l.edges[Front].All() }

// Backward iterates over all values in this [List], from [Back] to [Front].
func (l *List[T]) Backward() iter.Seq[T] { return l.edges[Back].Backward() }

// Copy creates an exact copy of this [List] and [Node] objects. The [Node]s of
// the new [List] will be allocated in contiguous memory space, if possible (OS
// and runtime dependent).
func (l *List[T]) Copy() *List[T] {
	var i int
	var cp List[T]
	var node *Node[T]
	var nodes []Node[T]

	cp.version = l.version
	if cp.length = l.length; cp.length == 0 {
		return &cp
	}

	nodes = make([]Node[T], cp.length)
	cp.edges[Front] = &nodes[0]
	cp.edges[Back] = &nodes[len(nodes)-1]

	node = l.edges[Front]
	nodes[0].Value = node.Value
	nodes[0].list = &cp

	for i, node = 1, node.peers[Next]; node != nil; i, node = i+1, node.peers[Next] {
		nodes[i].Value = node.Value
		nodes[i].list = &cp
		nodes[i].peers[Prev] = &nodes[i-1]
		nodes[i-1].peers[Next] = &nodes[i]
	}

	return &cp
}

// Iter returns an [Iterator] which traverses the [Node]s of this [List] from
// [Front] to [Back], or from [Back] to [Front] if backward is true.
func (l *List[T]) Iter(backward ...bool) (it Iterator[T]) {
	edge := btoi[Edge](len(backward) > 0 && backward[0])
	it.init(l.edges[edge], edge)
	return it
}

// IsMember indicates if the given [Node] belongs to this [List]. The [Node]
// must not be nil. Always returns false if this [List] is nil.
func (l *List[T]) IsMember(node *Node[T]) bool {
	other := node.list
	return other != nil && other == l
}

// Len returns the length of this [List] with O(1) time complexity.
func (l *List[T]) Len() int { return l.length }

// Clear removes all elements in this [List]. Returns the receiver.
func (l *List[T]) Clear() *List[T] {
	if l.length > 0 {
		l.edges.destroy()
		l.edges.reset()
		l.length = 0
		l.version++
	}

	return l
}

// Edge returns the [Node] on the [Left] or [Right] edge of this [List], or
// panics if edge is invalid.
func (l *List[T]) Edge(edge Edge) *Node[T] { return l.edges[edge] }

// Back returns the last [Node] in this [List], or nil if empty.
func (l *List[T]) Back() *Node[T] { return l.edges[Back] }

// Front returns the first [Node] in this [List], or nil if empty.
func (l *List[T]) Front() *Node[T] { return l.edges[Front] }

// Insert places a new value into this [List] on the [Left] or [Right] edge
// of the target [Node]. If the target is nil, it implies the [Node] on the
// specified [Edge] of this [List] (behaving as [List.Push] does).
//
// If target does not belong to this [List], this method is a no-op and returns
// nil to indicate failure.
//
// The edge must be valid, otherwise this method panics.
//
// On success returns the [Node] of the value that was just inserted.
func (l *List[T]) Insert(v T, target *Node[T], edge Edge) *Node[T] {
	var n Node[T]
	var ok bool

	if target, ok = l.resolveTarget(target, edge); !ok {
		return nil
	} else if n.Value, n.list = v, l; target != nil {
		l.insert(&span[T]{&n, &n}, target, edge)
	} else {
		l.edges[Front] = &n
		l.edges[Back] = &n
	}

	l.length++
	l.version++

	return &n
}

// InsertAfter inserts a new value [After] the target [Node] using
// [List.Insert].
func (l *List[T]) InsertAfter(v T, target *Node[T]) *Node[T] {
	return l.Insert(v, target, After)
}

// InsertBefore inserts a new value [Before] the target [Node] using
// [List.Insert].
func (l *List[T]) InsertBefore(v T, target *Node[T]) *Node[T] {
	return l.Insert(v, target, Before)
}

// InsertMany efficiently inserts an entire slice of values into this [List]
// [Before] or [After] the target [Node].
//
// If backward is true, the values are inserted in reverse order.
//
// If the target [Node] is nil, it implies the [Node] on the specified [Edge]
// of this [List]. Otherwise, if the target is not a member of this [List],
// this method is a no-op and returns two nil pointers to indicate failure.
//
// The edge must be valid, otherwise this method panics.
//
// On success, returns the first and last [Node]s that were inserted.
func (l *List[T]) InsertMany(values []T, target *Node[T], edge Edge, backward ...bool) (*Node[T], *Node[T]) {
	var ok bool
	var sp span[T]
	var n int

	if target, ok = l.resolveTarget(target, edge); !ok {
		return nil, nil
	} else if sp, n = newSpan(l, values, len(backward) > 0 && backward[0]); target == nil {
		copy(l.edges[:], sp[:])
	} else {
		l.insert(&sp, target, edge)
	}

	l.length += n
	l.version++
	return sp[0], sp[1]
}

// InsertManyBefore inserts the slice of values [Before] the target [Node]
// using [List.InsertMany].
func (l *List[T]) InsertManyBefore(values []T, target *Node[T], backward ...bool) (*Node[T], *Node[T]) {
	return l.InsertMany(values, target, Before, backward...)
}

// InsertManyAfter inserts the slice of values [After] the target [Node]
// using [List.InsertMany].
func (l *List[T]) InsertManyAfter(values []T, target *Node[T], backward ...bool) (*Node[T], *Node[T]) {
	return l.InsertMany(values, target, After, backward...)
}

// Move moves the given [Node] so that it occurs [Before] or [After] the target
// [Node]. If the target is nil, it implies the [Node] on the specified [Edge]
// of this [List]. The node which is being moved must not be nil. The edge must
// also be valid, otherwise this method panics.
//
// If either [Node] does not belong to this [List], this method is a no-op and
// returns nil to indicate failure.
//
// On success, returns the (same) [Node] that was moved.
func (l *List[T]) Move(node, target *Node[T], edge Edge) *Node[T] {
	if target, ok := l.resolveTarget(target, edge); ok && target != node {
		l.unlink(&span[T]{node, node})
		l.insert(&span[T]{node, node}, target, edge)
		l.version++
	} else if !ok {
		return nil
	}

	return node
}

// MoveBefore moves the given [Node] to the [Left] of the target [Node] using
// [List.Move].
func (l *List[T]) MoveBefore(node, target *Node[T]) *Node[T] {
	return l.Move(node, target, Before)
}

// MoveAfter moves the given [Node] to the [Right] of the target [Node] using
// [List.Move].
func (l *List[T]) MoveAfter(node, target *Node[T]) *Node[T] {
	return l.Move(node, target, After)
}

// MoveToEdge efficiently moves the given [Node] to the [Front] or [Back]
// [Edge] of this [List]. The node must not be nil and the edge must be valid,
// otherwise this method panics.
//
// If the [Node] doesn't belong to this [List], this method is a no-op and
// returns nil to indicate failure.
//
// On success, returns the same [Node]. See also: [List.MoveToFront] and
// [List.MoveToBack], which are slightly more efficient.
func (l *List[T]) MoveToEdge(node *Node[T], edge Edge) *Node[T] {
	switch edge.Must() {
	case Front:
		return l.MoveToFront(node)
	default:
		return l.MoveToBack(node)
	}
}

// MoveToFront moves the given [Node] to the [Front] edge of this [List] in the
// same manner as described by [List.MoveToEdge].
func (l *List[T]) MoveToFront(node *Node[T]) *Node[T] {
	if target := l.edges[Front]; node.list == l && node != target {
		l.unlink(&span[T]{node, node})
		l.insert(&span[T]{node, node}, target, Front)
		l.version++
	} else if node != target {
		return nil
	}

	return node
}

// MoveToBack moves the given [Node] to the [Back] edge of this [List] in the
// same manner as described by [List.MoveToEdge].
func (l *List[T]) MoveToBack(node *Node[T]) *Node[T] {
	if target := l.edges[Back]; node.list == l && node != target {
		l.unlink(&span[T]{node, node})
		l.insert(&span[T]{node, node}, target, Back)
		l.version++
	} else if node != target {
		return nil
	}

	return node
}

// Push inserts a new value at the [Front] or [Back] of this [List] and
// returns the [Node] that stores the value. The edge must be valid,
// otherwise this method panics.
func (l *List[T]) Push(v T, edge Edge) *Node[T] {
	var n Node[T]
	n.Value = v
	n.list = l

	if target := l.edges[edge]; target != nil {
		l.insert(&span[T]{&n, &n}, target, edge)
	} else {
		l.edges[Front] = &n
		l.edges[Back] = &n
	}

	l.length++
	l.version++
	return &n
}

// PushFront pushes a new value to the [Front] of this [List] via [List.Push].
func (l *List[T]) PushFront(v T) *Node[T] { return l.Push(v, Front) }

// PushBack pushes a new value to the [Back] of this [List] via [List.Push].
func (l *List[T]) PushBack(v T) *Node[T] { return l.Push(v, Back) }

// Pop removes and returns the node from the [Front] or [Back] of this [List],
// or returns nil if empty. The edge must be valid, otherwise this method
// panics.
func (l *List[T]) Pop(edge Edge) (node *Node[T]) {
	if node = l.edges[edge]; node != nil {
		l.remove(&span[T]{node, node}, false)
	}

	return node
}

// PopFront removes and returns [Front] [Node] via [List.Pop].
func (l *List[T]) PopFront() (node *Node[T]) { return l.Pop(Front) }

// PopBack removes and returns [Back] [Node] via [List.Pop].
func (l *List[T]) PopBack() (node *Node[T]) { return l.Pop(Back) }

// Remove deletes the given [Node] from this [List]. The [Node] must not be
// nil. Does nothing if the [Node] does not belong to this [List]. Always
// returns the value stored on the [Node].
func (l *List[T]) Remove(node *Node[T]) T {
	if node.list == l {
		l.remove(&span[T]{node, node}, false)
	}

	return node.Value
}

// Rotate rotates the [Node]s of this [List] by the specified number of steps
// to the [Left] or [Right], as determined by [Edge]. The edge must be valid.
// This method is a no-op if steps is less than 1 or a multiple of [List.Len].
// The number of steps can be greater than the list length, which reduces via:
//
//	steps %= list.Len()
//
// Always returns the receiver (this [List]).
func (l *List[T]) Rotate(steps int, edge Edge) *List[T] {
	var node *Node[T]
	var sp span[T]
	oppo := Edge(opposite[edge])
	if steps = stepmod(steps, l.length); steps == 0 {
		return l
	}

	node = l.edges[edge]
	steps--

	for range steps {
		node = node.peers[oppo]
	}

	sp[oppo] = node
	sp[edge] = l.edges[edge]

	l.unlink(&sp)
	l.insert(&sp, l.edges[oppo], oppo)
	l.version++
	return l
}

// RotateLeft rotates this [List] to the [Left] by the specified number of
// steps using [List.Rotate].
func (l *List[T]) RotateLeft(steps int) *List[T] { return l.Rotate(steps, Left) }

// RotateRight rotates this [List] to the [Right] by the specified number of
// steps using [List.Rotate].
func (l *List[T]) RotateRight(steps int) *List[T] { return l.Rotate(steps, Right) }

// Take inherits a [Node] that does not belong to this [List], placing it into
// the [List] [Before] or [After] the target [Node]. If the target is nil, it
// implies the [Node] on the specified [Edge] of this [List].
//
// If the [Node] to inherit already belongs to this [List], or the target
// [Node] *does not* belong to this [List], this method is a no-op and returns
// nil to indicate failure.
//
// The [Node] to inherit must not be nil and the edge must be valid, otherwise
// this method panics.
//
// Returns the inherited [Node] on success.
func (l *List[T]) Take(inherit, target *Node[T], edge Edge) *Node[T] {
	var ok bool

	if target, ok = l.resolveTarget(target, edge); !ok || inherit.list == l {
		return nil
	} else if inherit.list != nil {
		inherit.Pop()
	}

	l.insert(&span[T]{inherit, inherit}, target, edge)
	l.length++
	l.version++
	return inherit
}

// TakeBefore inherits a [Node], inserting it [Before] the target [Node] using
// [List.Take].
func (l *List[T]) TakeBefore(inherit, target *Node[T]) *Node[T] {
	return l.Take(inherit, target, Before)
}

// TakeAfter inherits a [Node], inserting it [After] the target [Node] using
// [List.Take].
func (l *List[T]) TakeAfter(inherit, target *Node[T]) *Node[T] {
	return l.Take(inherit, target, After)
}

// Version returns the current version of this [List]. The version is
// incremented each time this [List] is successfully mutated.
func (l *List[T]) Version() uint { return l.version }

// ForEach iterates over the values of this [List] and executes the given
// function for each value. If backward is true, it iterates in reverse order.
func (l *List[T]) ForEach(fn func(v T), backward ...bool) {
	for it := l.Iter(backward...); it.Next(); {
		fn(it.Node.Value)
	}
}

// resolveTarget validates that the target [Node] can be used for an operation,
// resolving the implied [Edge] where necessary. It always returns false if the
// target does not belong to this [List].
func (l *List[T]) resolveTarget(target *Node[T], edge Edge) (*Node[T], bool) {
	if target == nil {
		return l.edges[edge], true
	} else if target.list != l {
		return nil, false
	}

	return target, true
}

// insert inserts the nodes in [span] next on the specified [Edge] of the
// target. The caller makes the following guarantees:
//
//   - The [span] is not nil and both [Node]s belong to this [List].
//   - The target was resolved via [List.resolveTarget].
//   - The caller will update the length and version where appropriate.
func (l *List[T]) insert(sp *span[T], target *Node[T], edge Edge) {
	oppo := opposite[edge]
	edgeNode := target.peers[edge]
	if edgeNode == nil {
		l.edges[edge] = sp[edge]
	} else {
		edgeNode.peers[oppo] = sp[edge]
	}

	sp[oppo].peers[oppo] = target
	sp[edge].peers[edge] = edgeNode
	target.peers[edge] = sp[oppo]
}

// remove the given [span] from this [List]. First, the [span] is unlinked via
// [List.unlink]. Next, either [span.disown] or [span.destroy] is called to
// remove references to this [List] and/or [Node] peers (dependent upon the
// destroy argument).
//
// The caller assures that:
//
//   - The [span] is not nil and both [Node]s belong to this [List].
//   - They *WILL NOT* update the length or version of this [List]; in this
//     case, this is delegated to this method.
func (l *List[T]) remove(sp *span[T], destroy bool) {
	var size int
	l.unlink(sp)

	if destroy {
		size = sp.destroy()
	} else {
		size = sp.disown()
	}

	l.version++
	l.length -= size
}

// unlink removes the given [span] from this [List]. The [Node] which occurs
// before the first [Node] in the [span] will be updated to point to the [Node]
// which occurs after the last [Node] in the [span]; the edges of this [List]
// are updated appropriately during this process.
//
// This method *does not* perform any housekeeping on the [Node]s within the
// [span] (e.g. [span.disown] or [span.destroy]); it simply unlinks the [span]
// from the [List].
func (l *List[T]) unlink(sp *span[T]) {
	isHead := sp[Front].peers[Before] == nil
	isTail := sp[Back].peers[After] == nil

	switch {
	case isHead && isTail:
		l.edges[Front], l.edges[Back] = nil, nil
	case isHead && !isTail:
		// Node after the end of the span is the new list front; nothing
		// preceeds it.
		l.edges[Front] = sp[Back].peers[After]
		sp[Back].peers[After].peers[Before] = nil

		// Isolate the span
		sp[Back].peers[After] = nil
	case !isHead && isTail:
		// Node before the start of the span is now the back of the list;
		// nothing follows it.
		l.edges[Back] = sp[Front].peers[Before]
		sp[Front].peers[Before].peers[After] = nil

		// Isolate the span
		sp[Front].peers[Before] = nil
	default:
		// Link the node before the start of the span to the node after the end
		sp[Front].peers[Before].peers[After] = sp[Back].peers[After]

		// Link the node after the end of the span to the node before the start
		sp[Back].peers[After].peers[Before] = sp[Front].peers[Before]

		// Isolate the span
		sp[Front].peers[Before] = nil
		sp[Back].peers[After] = nil
	}
}

// span naively represents a contiguous range of values, inclusive of the
// [Node] object(s) which it references. The references of a valid span are
// always non-nil, contiguous, and either belong to the same [List] or are all
// orphans.
type span[T any] [2]*Node[T]

// newSpan creates a [Node] for each value in in the given slice, links them
// together, and sets their owner to the given [List]. When backward is true,
// they are linked together in reverse order.
//
// Note: this function *DOES NOT* modify the length or version properties of
// the given [List].
func newSpan[T any](list *List[T], values []T, backward bool) (sp span[T], length int) {
	var i, inc, start, stop int
	var nodes []Node[T]

	if len(values) == 0 {
		return sp, 0
	} else if backward {
		inc, start, stop = -1, len(values)-1, -1
	} else {
		inc, stop = 1, len(values)
	}

	nodes = make([]Node[T], len(values))
	nodes[0].Value = values[start]
	nodes[0].list = list
	i = 1

	sp[Front] = &nodes[0]
	sp[Back] = &nodes[len(values)-1]

	for start += inc; start != stop; start += inc {
		nodes[i].Value = values[start]
		nodes[i-1].peers[Next] = &nodes[i]
		nodes[i].peers[Prev] = &nodes[i-1]
		nodes[i].list = list
		i++
	}

	return sp, len(values)
}

// reset sets the member values to nil. It does not call either [span.disown]
// nor [span.destroy].
func (s *span[T]) reset() { s[0], s[1] = nil, nil }

// disown sets the [Node.list] value to nil for each [Node] in the [span].
// Returns the number of affected nodes.
func (s *span[T]) disown() (i int) {
	i = 1
	node := s[0]
	node.list = nil

	if node = node.peers[Next]; node == nil {
		goto Done
	}

	for ; node != nil; i++ {
		node.list = nil
		node = node.peers[Next]
	}

Done:
	return i
}

// destroy is like [span.disown], but also sets the links between each [Node]
// in the span to nil. Returns the number of affected nodes.
//
// Note: this does not set the references held by the [span] to nil. That must
// be done manually or via [span.reset].
func (s *span[T]) destroy() (i int) {
	i = 1
	node := s[0]
	node.list = nil
	if node = node.peers[Next]; node == nil {
		goto Done
	}

	for ; node != nil; i++ {
		node.list = nil
		node.peers[Prev].peers[Next] = nil
		node.peers[Prev] = nil
		node = node.peers[Next]
	}

Done:
	return i
}

func stepmod(steps, length int) int {
	if length == 0 {
		return 0
	} else if steps = max(0, steps); steps > length {
		return steps % length
	}

	return steps
}
