package linked_test

import (
	"fmt"
	"iter"
	"slices"
	"testing"

	"github.com/sbueschel/go-linked"
)

var (
	UpperAlphabet = []string{
		"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
		"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
	}

	LowerAlphabet = []string{
		"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
		"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
	}
)

type TestSuite[T comparable] struct {
	// Name of the test.
	Name string

	// State is the known-state of the [linked.List] being tested. See
	// [ListTestSuite.Isolate] for more information.
	State *State[T]

	canCopy  int // -1 == failed, 0 == never tested, 1 == passed
	canClear int // -1 == failed, 0 == never tested, 1 == passed
	failedAt string
}

type State[T comparable] struct {
	List    *linked.List[T]
	Nodes   []*linked.Node[T]
	Values  []T
	Version uint
}

func Test(t *testing.T) {
	NewTestSuite("Nil", []string(nil)).Run(t)
	NewTestSuite("Empty", []string{}).Run(t)
	NewTestSuite("OneItem", UpperAlphabet[:1]).Run(t)
	NewTestSuite("Alphabet", UpperAlphabet).Run(t)
}

// NewTestSuite returns a partially initialized test-suite which tests a
// [linked.List] that is initially populated with the given values.
func NewTestSuite[T comparable](name string, values []T) *TestSuite[T] {
	return &TestSuite[T]{
		Name:  name,
		State: &State[T]{Values: slices.Clone(values)},
	}
}

// List returns the [linked.List] stored at [TestSuite].State.List.
func (ts *TestSuite[T]) List() *linked.List[T] { return ts.State.List }

// Nodes returns the [linked.Node] objects enumerated during initialization of
// the [State] stored at [TestSuite].State.Nodes. Modifying the slice
// modifies the [State].
func (ts *TestSuite[T]) Nodes() []*linked.Node[T] { return ts.State.Nodes }

// Values returns the values stored at [TestSuite].State.Values. Modifying
// the slice modifies the [State].
func (ts *TestSuite[T]) Values() []T { return ts.State.Values }

// Run executes this test-suite as a sub-test of the current testing namespace.
func (ts *TestSuite[T]) Run(t *testing.T) { t.Run(ts.Name, ts.Test) }

// RunTest executes the test function, but only if no prior test failures have
// occurred, otherwise it immediately fails.
func (ts *TestSuite[T]) RunTest(t *testing.T, name string, fn func(*testing.T)) (ok bool) {
	if ts.failedAt != "" {
		t.Run(name, ts.fail)
	} else if ok = t.Run(name, fn); !ok {
		ts.failedAt = fmt.Sprintf("%s/%s", t.Name(), name)
	}

	return ok
}

// Isolate creates a deep copy of the current [TestSuite.State] before
// running the test, isolating any peer tests from seeing changes it made
// during execution (via [TestSuite.RunTest]). After execution completes,
// the previous [TestSuite.State] is restored.
func (ts *TestSuite[T]) Isolate(t *testing.T, name string, fn func(*testing.T)) (ok bool) {
	oldState := ts.State

	ok = ts.RunTest(t, name, func(t *testing.T) {
		if ts.State = ts.State.Copy(t); ts.State != nil {
			fn(t)
			ts.State.Close(ts.canClear > 0)
		}
	})

	ts.State = oldState
	return ok
}

func (ts *TestSuite[T]) fail(t *testing.T) {
	t.Logf("fast fail due to prior testing failure\n\tCaused by: %s", ts.failedAt)
}

func (ts *TestSuite[T]) Test(t *testing.T) {
	ts.RunTest(t, "List", ts.TestList)
	ts.RunTest(t, "Node", ts.TestNode)
}

func (ts *TestSuite[T]) TestList(t *testing.T) {
	ts.RunTest(t, "New", ts.TestListNew)
	ts.RunTest(t, "Len", ts.TestListLen)
	ts.RunTest(t, "Front", ts.TestListFront)
	ts.RunTest(t, "Back", ts.TestListBack)
	ts.RunTest(t, "All", ts.TestListAll)
	ts.RunTest(t, "Backward", ts.TestListBackward)
	ts.RunTest(t, "Iter", ts.TestListIter)
	ts.RunTest(t, "Copy", ts.TestListCopy)
	ts.RunTest(t, "ForEach", ts.TestListForEach)
	ts.Isolate(t, "Clear", ts.TestListClear)
	ts.Isolate(t, "Push", ts.TestListPush)
	ts.Isolate(t, "Insert", ts.TestListInsert)
	ts.Isolate(t, "InsertMany", ts.TestListInsertMany)
	ts.Isolate(t, "Take", ts.TestListTake)
	ts.Isolate(t, "Move", ts.TestListMove)
	ts.Isolate(t, "Rotate", ts.TestListRotate)
	ts.Isolate(t, "Pop", ts.TestListPop)
	ts.Isolate(t, "Remove", ts.TestListRemove)
}

func (ts *TestSuite[T]) TestNode(t *testing.T) {
	ts.RunTest(t, "Iter", ts.TestNodeIter)
	ts.Isolate(t, "Move", ts.TestNodeMove)
	ts.Isolate(t, "Pop", ts.TestNodePop)
	ts.Isolate(t, "Remove", ts.TestNodeRemove)
}

func (ts *TestSuite[T]) TestListNew(t *testing.T) {
	ts.RunTest(t, "Init", ts.State.Init)
	ts.Isolate(t, "Backward", func(t *testing.T) {
		AssertListValues(t, Reversed(ts.Values()), linked.New(ts.Values(), true))
	})
}

func (ts *TestSuite[T]) TestListLen(t *testing.T) {
	Assert(t, len(ts.Values()), ts.List().Len(), "unexpected length")
}

func (ts *TestSuite[T]) TestListFront(t *testing.T) {
	ts.testListEdge(t, linked.Front)
}

func (ts *TestSuite[T]) TestListBack(t *testing.T) {
	ts.testListEdge(t, linked.Back)
}

func (ts *TestSuite[T]) TestListCopy(t *testing.T) {
	if ts.canCopy == 0 {
		ts.canCopy = -1
	}

	orig := ts.List()
	cp := orig.Copy()

	if !AssertNotSame(t, orig, cp, "Copy() should return a distinct %T", cp) {
		return
	} else if !Assert(t, orig.Len(), cp.Len(), "Copy().Len()") {
		return
	} else if !Assert(t, orig.Version(), cp.Version(), "Copy().Version()") {
		return
	} else if AssertListEqual(t, orig, cp) && ts.canCopy == 0 {
		ts.canCopy = 1
	}
}

func (ts *TestSuite[T]) TestListClear(t *testing.T) {
	if ts.canClear == 0 {
		ts.canClear = -1
	}

	nodes := slices.Clone(ts.State.Nodes)
	values := slices.Clone(ts.State.Values)
	expectVersion := ts.List().Version()

	if len(nodes) > 0 {
		expectVersion++
	}

	for i := range nodes {
		if !AssertSame(
			t, nodes[i], ts.State.Nodes[i],
			"at index %d: shallow copy should refer to same %T",
			i, nodes[i],
		) {
			return
		}
	}

	if !AssertSame(
		t, ts.List(), ts.List().Clear(),
		"Clear() should return method receiver",
	) {
		return
	}

	for i := range nodes {
		if node := nodes[i]; !Assert(
			t, false, node.IsMember(ts.List()),
			"after Clear(): %[1]T at index %[2]d: (%[1]T).IsMember(%[3]T@%[3]p)",
			node, i, ts.State.List,
		) {
			return
		} else if !Assert(
			t, true, node.IsMember(nil),
			"after Clear(): %[1]T at index %[2]d: (%[1]T).IsMember(nil)",
			node, i,
		) {
			return
		} else if prev := node.Prev(); !AssertNil(
			t, prev,
			"after Clear(): %[1]T at index %[2]d: (%[1]T).Prev() -> %#[1]v",
			prev, i,
		) {
			return
		} else if next := node.Next(); !AssertNil(
			t, next,
			"after Clear(): %[1]T at index %[2]d: (%[1]T).Next() -> %#[1]v",
			next, i,
		) {
			return
		} else if !Assert(
			t, values[i], node.Value,
			"after Clear(): %[1]T at index %[2]d: (%[1]T).Value should not change",
			node, i,
		) {
			return
		}
	}

	ts.State.Values = nil
	ts.State.Nodes = nil

	ts.TestListLen(t)
	ts.TestListFront(t)
	ts.TestListBack(t)
	AssertVersion(t, expectVersion, ts.List().Version())

	if !t.Failed() && ts.canClear == 0 {
		ts.canClear = 1
	}
}

func (ts *TestSuite[T]) TestListAll(t *testing.T) { ts.testListIterSeq(t, linked.Front) }

func (ts *TestSuite[T]) TestListBackward(t *testing.T) { ts.testListIterSeq(t, linked.Back) }

func (ts *TestSuite[T]) TestListIter(t *testing.T) {
	ts.RunTest(t, "FrontToBack", NewIteratorTestSuite(ts.State, nil, false).Test)
	ts.RunTest(t, "BackToFront", NewIteratorTestSuite(ts.State, nil, true).Test)
}

func (ts *TestSuite[T]) TestListPush(t *testing.T) {
	ts.Isolate(t, "Front", func(t *testing.T) {
		ts.Isolate(t, "Cleared", func(t *testing.T) { ts.testListInsertPush(t, linked.Front, true, true) })
		ts.Isolate(t, "NoClear", func(t *testing.T) { ts.testListInsertPush(t, linked.Front, true, false) })
	})

	ts.Isolate(t, "Back", func(t *testing.T) {
		ts.Isolate(t, "Cleared", func(t *testing.T) { ts.testListInsertPush(t, linked.Back, true, true) })
		ts.Isolate(t, "NoClear", func(t *testing.T) { ts.testListInsertPush(t, linked.Back, true, false) })
	})
}

func (ts *TestSuite[T]) TestListInsert(t *testing.T) {
	var edge linked.Edge

	for edge = linked.Before; edge.IsValid(); edge++ {
		ts.Isolate(t, edge.Suffix(), func(t *testing.T) {
			ts.Isolate(t, "Mirrored", func(t *testing.T) { ts.testListInsertMirrored(t, edge) })
			ts.Isolate(t, "Implicit", func(t *testing.T) { ts.testListInsertPush(t, edge, false, true) })
			ts.Isolate(t, "Foreign", func(t *testing.T) { ts.testListInsertForeign(t, edge) })
		})
	}
}

func (ts *TestSuite[T]) TestListInsertMany(t *testing.T) {
	var edge linked.Edge

	for edge = linked.Before; edge.IsValid(); edge++ {
		ts.Isolate(t, edge.Suffix(), func(t *testing.T) {
			ts.Isolate(t, "Implicit", func(t *testing.T) { ts.testListInsertMany(t, edge, false) })
			ts.Isolate(t, "Reversed", func(t *testing.T) { ts.testListInsertMany(t, edge, true, ts.List().Edge(edge)) })
			ts.Isolate(t, "Foreign", func(t *testing.T) { ts.testListInsertMany(t, edge, false, &linked.Node[T]{}) })
		})
	}
}

func (ts *TestSuite[T]) TestListTake(t *testing.T) {
	ts.Isolate(t, "Before", func(t *testing.T) { ts.testListTake(t, linked.Before) })
	ts.Isolate(t, "After", func(t *testing.T) { ts.testListTake(t, linked.After) })
}

func (ts *TestSuite[T]) TestListMove(t *testing.T) {
	ts.Isolate(t, "Before", func(t *testing.T) {
		ts.Isolate(t, "Reverse", func(t *testing.T) { ts.testListMove(t, linked.Before) })
		ts.Isolate(t, "Foreign", func(t *testing.T) { ts.testListMoveForeign(t, linked.Before) })
	})

	ts.Isolate(t, "After", func(t *testing.T) {
		ts.Isolate(t, "Reverse", func(t *testing.T) { ts.testListMove(t, linked.After) })
		ts.Isolate(t, "Foreign", func(t *testing.T) { ts.testListMoveForeign(t, linked.After) })
	})

	ts.Isolate(t, "ToFront", func(t *testing.T) { ts.testListToEdge(t, linked.Front) })
	ts.Isolate(t, "ToBack", func(t *testing.T) { ts.testListToEdge(t, linked.Back) })
}

func (ts *TestSuite[T]) TestListPop(t *testing.T) {
	ts.Isolate(t, "Front", func(t *testing.T) { ts.testListPop(t, linked.Front) })
	ts.Isolate(t, "Back", func(t *testing.T) { ts.testListPop(t, linked.Back) })
}

func (ts *TestSuite[T]) TestListRemove(t *testing.T) {
	var foreign linked.Node[T]
	list := ts.List()
	version := list.Version()
	if !Assert(
		t, foreign.Value, ts.List().Remove(&foreign),
		"should always return value of node, even when foreign",
	) {
		return
	} else if !AssertVersion(t, version, list.Version()) {
		return
	}

	nodes := slices.Clone(ts.Nodes())
	values := slices.Clone(ts.Values())
	size := list.Len()
	for i := 0; list.Len() > 0; i++ {
		if !Assert(
			t, values[i], list.Remove(list.Front()),
			"unexpected node at pseudo-index %d", i,
		) {
			return
		} else if version++; !AssertVersion(t, version, list.Version()) {
			return
		} else if !Assert(t, true, i < size, "failed to delete nodes properly") {
			return
		}
	}

	for i := range nodes {
		if !Assert(
			t, true, nodes[i].IsMember(nil),
			"deleted node at index %d should identify as member of nil", i,
		) {
			return
		} else if !AssertNil(
			t, nodes[i].Prev(),
			"deleted node at index %d should have no link to Prev() node", i,
		) {
			return
		} else if !AssertNil(
			t, nodes[i].Next(),
			"deleted node at index %d should have no link to Next() node", i,
		) {
			return
		}
	}
}

func (ts *TestSuite[T]) TestListRotate(t *testing.T) {
	var expect []T
	var steps int

	list := ts.List()
	size := list.Len()
	values := ts.Values()
	version := list.Version()

	if !AssertSame(t, list, list.RotateLeft(-1), "should return receiver %T", list) {
		return
	} else if !AssertVersion(t, version, list.Version()) { // should be no-op
		return
	} else if !AssertSame(t, list, list.RotateRight(size*4), "should return receiver %T", list) {
		return
	} else if !AssertVersion(t, version, list.Version()) { // should be no-op
		return
	}

	for i := range size {
		expect = slices.Concat(values[i:], values[:i])

		if i > 0 {
			version++
		}

		if steps = i; steps&1 == 1 {
			steps += size // to test modulo functionality correctly
		}

		if !AssertSame(t, list, list.RotateLeft(steps), "should return receiver %T", list) {
			return
		} else if !AssertVersion(t, version, list.Version()) {
			return
		} else if !AssertListValues(t, expect, list) {
			return
		} else if i == 0 {
			continue // no need to undo the rotation
		} else if !AssertSame(t, list, list.RotateRight(i), "should return receiver %T", list) {
			return
		} else if version++; !AssertVersion(t, version, list.Version()) {
			return
		} else if !AssertListValues(t, values, list) {
			return
		}
	}
}

func (ts *TestSuite[T]) TestListForEach(t *testing.T) {
	t.Run("Forward", func(t *testing.T) {
		var expect, actual []T

		if ts.List().Len() > 0 {
			expect = ts.Values()
		}

		ts.List().ForEach(func(v T) { actual = append(actual, v) })
		AssertSlice(t, expect, actual)
	})

	t.Run("Backward", func(t *testing.T) {
		var expect, actual []T

		if ts.List().Len() > 0 {
			expect = Reversed(ts.Values())
		}

		ts.List().ForEach(func(v T) { actual = append(actual, v) }, true)
		AssertSlice(t, expect, actual)
	})

}

func (ts *TestSuite[T]) TestNodeIter(t *testing.T) {
	ts.RunTest(t, "Front", func(t *testing.T) {
		t.Run("Forward", NewIteratorTestSuite(ts.State, ts.List().Front(), false).Test)
		t.Run("Backward", NewIteratorTestSuite(ts.State, ts.List().Front(), true).Test)
	})

	ts.RunTest(t, "Back", func(t *testing.T) {
		t.Run("Forward", NewIteratorTestSuite(ts.State, ts.List().Back(), false).Test)
		t.Run("Backward", NewIteratorTestSuite(ts.State, ts.List().Back(), true).Test)
	})
}

func (ts *TestSuite[T]) TestNodeMove(t *testing.T) {
	ts.Isolate(t, "Before", func(t *testing.T) { ts.testNodeMove(t, linked.Before) })
	ts.Isolate(t, "After", func(t *testing.T) { ts.testNodeMove(t, linked.After) })

	ts.Isolate(t, "ToFront", func(t *testing.T) { ts.testNodeMoveToEdge(t, linked.Front, false) })
	ts.Isolate(t, "ToBack", func(t *testing.T) { ts.testNodeMoveToEdge(t, linked.Back, false) })

	ts.Isolate(t, "ToEdge", func(t *testing.T) {
		ts.Isolate(t, "Front", func(t *testing.T) { ts.testNodeMoveToEdge(t, linked.Front, true) })
		ts.Isolate(t, "Back", func(t *testing.T) { ts.testNodeMoveToEdge(t, linked.Back, true) })
	})
}

func (ts *TestSuite[T]) TestNodePop(t *testing.T) {
	var count int
	var expect []T
	var next *linked.Node[T]
	list := ts.List()
	size := list.Len()

	node := list.Front()
	if size > 3 {
		expect = []T{node.Value}
		node = node.Next()
	}

	for node != nil {
		next = node.Next()
		if !AssertSame(t, node, node.Pop(), "should return receiver") {
			return
		} else if !Assert(t, true, node.IsOrphan(), "(%T).IsOrphan()", node) {
			return
		} else if !Assert(t, true, count < size, "popped too many nodes") {
			return
		}

		count++
		node = next
	}

	AssertListValues(t, expect, list)
}

func (ts *TestSuite[T]) TestNodeRemove(t *testing.T) {
	var values []T
	list := ts.List()
	version := list.Version()

	for list.Len() > 0 {
		values = append(values, list.Front().Remove())
		if version++; !AssertVersion(t, version, list.Version()) {
			return
		}
	}

	if !Assert(t, len(values), len(ts.Values())) || len(values) == 0 {
		return
	}

	AssertSlice(t, ts.Values(), values)
}

func (ts *TestSuite[T]) testListEdge(t *testing.T, edge linked.Edge) {
	var actual *linked.Node[T]
	var index, n int
	var sig string

	edgeArg := fmt.Sprintf("%d /* %s */", edge, edge.List())
	list := ts.List()
	switch edge.Must() {
	case linked.Front:
		actual = list.Front()
		sig = fmt.Sprintf("(%T).Front()", list)
	default:
		actual = list.Back()
		sig = fmt.Sprintf("(%T).Back()", list)
	}

	if n = list.Len(); edge == linked.Back {
		index = len(ts.Values()) - 1
	}

	if !Assert(
		t, n > 0, actual.OK(),
		"%s.OK() should be %T when receiver is %snil",
		sig, n > 0, When(n > 0, "non-"),
	) || n == 0 {
		return
	} else if !Assert(
		t, ts.Values()[index], actual.Value,
		"%s.Value should equal initialization value at index %d",
		sig, index,
	) {
		return
	} else if !AssertSame(
		t, actual, list.Edge(edge),
		"(%T).Edge(%s)", list, edgeArg,
	) {
		return
	} else if !Assert(
		t, true, actual.IsEdge(edge),
		"%s.IsEdge(%s)", sig, edgeArg,
	) {
		return
	} else if (n == 1 || edge == linked.Front) && !Assert(
		t, true, actual.IsFront(),
		"%s.IsFront()", sig,
	) {
		return
	} else if n == 1 || edge == linked.Back {
		Assert(
			t, true, actual.IsBack(),
			"%s.IsBack()", sig,
		)
	}
}

func (ts *TestSuite[T]) testListIterSeq(t *testing.T, edge linked.Edge) {
	var fn func() iter.Seq[T]
	var expect []T

	if edge == linked.Front {
		fn, expect = ts.List().All, ts.Values()
	} else {
		fn, expect = ts.List().Backward, Reversed(ts.Values())
	}

	AssertNoError(t, VerifyIterSeq(t, expect, fn()))
}

func (ts *TestSuite[T]) testListPop(t *testing.T, edge linked.Edge) {
	var i, inc int
	var pop func() *linked.Node[T]

	state := ts.State

	if edge == linked.Front {
		pop, inc = state.List.PopFront, 1
	} else {
		pop, inc, i = state.List.PopBack, -1, len(state.Nodes)-1
	}

	for range len(state.Nodes) {
		node := pop()
		if !AssertSame(
			t, state.Nodes[i], node,
			"expected to pop %[1]T at index %[2]d\n\tPopped: %[1]#v\n\tWanted: %[3]#v",
			node, i, state.Nodes[i],
		) {
			return
		} else if !Assert(
			t, state.Values[i], node.Value,
			"at index %d: unexpected (%T).Value",
			i, node,
		) {
			return
		} else if !AssertNil(t, node.Next(), "(%T).Next()", node) {
			return
		} else if !AssertNil(t, node.Prev(), "(%T).Prev()", node) {
			return
		} else if !Assert(
			t, false, state.List.IsMember(node),
			"(%[1]T).IsMember(%[2]T@%[2]p)",
			state.List, node,
		) {
			return
		} else if !Assert(
			t, false, node.IsMember(state.List),
			"(%[1]T).IsMember(%[2]T@%[2]p)",
			node, state.List,
		) {
			return
		} else if !Assert(
			t, true, node.IsOrphan(),
			"(%[1]T).IsOrphan()",
			node,
		) {
			return
		}

		i += inc
	}

	if !AssertNil(t, pop(), "should have popped all values") {
		return
	}

	expectVersion := state.Version + uint(len(state.Nodes))
	state.Values = nil
	state.Nodes = nil

	ts.TestListLen(t)
	AssertVersion(t, expectVersion, ts.List().Version())
}

// testListMove selects the node initially at the specified edge of the current
// list as the destination target of a move operation. Afterwards, it moves
// the node at the opposite edge of list so that the node appears on the
// specified edge of the target node. This operation is repeated until all
// nodes in the list have been move, causing the list to be reversed in-place.
//
// Each node's linkage is checked after each move, and this test never attempts
// to perform more than [linked.List.Len] move operations.
func (ts *TestSuite[T]) testListMove(t *testing.T, edge linked.Edge) {
	var move func(*linked.Node[T], *linked.Node[T]) *linked.Node[T]
	var count, size int
	var node, target, actual *linked.Node[T]
	var sigFmt, sig string
	var list *linked.List[T]
	var oppo linked.Edge
	var version uint

	list = ts.List()
	if size = list.Len(); size == 0 {
		return // nothing to test
	} else if _ = edge.Must(); edge == linked.Before {
		move = list.MoveBefore
	} else {
		move = list.MoveAfter
	}

	version = list.Version()
	target = list.Edge(edge)
	oppo = edge.Opposite()
	sigFmt = fmt.Sprintf(
		"Move%s(\n\t%%s, // node\n\t%s, // target\n)",
		edge.Suffix(), NodeString(target),
	)

	for node = list.Edge(oppo); node != target; node = list.Edge(oppo) {
		sig = fmt.Sprintf(sigFmt, node)

		if actual = move(node, target); !AssertSame(
			t, node, actual,
			"should return the same node\n\tMethod: %s",
			sig,
		) {
			return
		} else if !AssertSame(
			t, target, node.Peer(oppo),
			"node should see target as its %s\n\tMethod: %s",
			oppo.Node(), sig,
		) {
			return
		} else if !AssertSame(
			t, node, target.Peer(edge),
			"target should see node as its %s\n\tMethod: %s",
			edge.Node(), sig,
		) {
			return
		} else if version++; !AssertVersion(t, version, list.Version()) {
			return
		} else if !Assert(t, true, count < size, "too many moves") {
			return
		}

		count++
	}

	if !AssertSame(t, node, target, "should have traversed to target node") {
		return
	} else if sig = fmt.Sprintf(sigFmt, node); !AssertSame(
		t, target, move(node, target),
		"should be a no-op returning the same node when node == target\n\tMethod: %s",
		sig,
	) {
		return
	} else if !AssertVersion(t, version, list.Version()) {
		return
	}

	AssertListValues(t, Reversed(ts.Values()), list)
}

func (ts *TestSuite[T]) testListMoveForeign(t *testing.T, edge linked.Edge) {
	var foreign linked.Node[T]
	var result *linked.Node[T]

	list := ts.List()
	if list.Len() == 0 {
		return // nothing to do
	}

	version := list.Version()

	if edge.Must() == linked.Before {
		result = list.MoveBefore(list.Front(), &foreign)
	} else {
		result = list.MoveAfter(list.Back(), &foreign)
	}

	_ = AssertNil(t, result, "should not do anything when target is not a list member") &&
		AssertVersion(t, version, list.Version())
}

func (ts *TestSuite[T]) testListInsertForeign(t *testing.T, edge linked.Edge) {
	var foreign linked.Node[T]
	var result *linked.Node[T]

	list := ts.List()
	version := list.Version()

	if edge.Must() == linked.Before {
		result = list.InsertBefore(foreign.Value, &foreign)
	} else {
		result = list.InsertAfter(foreign.Value, &foreign)
	}

	_ = AssertNil(t, result, "should not do anything when target is not a list member") &&
		AssertVersion(t, version, list.Version())
}

// testListInsertPush tests the list's insert and push method, dependent upon the
// vale of push. This test will push (or insert) the current values to the
// front or back of the list.
//
// This test results in a list that repeats its original values at the end of
// the list (edge is [linked.Back]), or one that mirrors the original values at
// the front (edge is [linked.Front]).
//
// If clear is true, the list is first cleared before proceeding. The code
// below demonstrates a trivial example/expectation:
//
//	list := linked.New([]string{"A", "B", "C"})
//
//	// edge == linked.Back
//	// if push == false, then list.InsertAfter(..., nil) is used, instead.
//	list.PushBack("A") -> {"A", "B", "C", "A"}
//	list.PushBack("B") -> {"A", "B", "C", "A", "B"}
//	list.PushBack("C") -> {"A", "B", "C", "A", "B", "C"}
//
//	// edge == linked.Front
//	// if push == false, then list.InsertBefore(..., nil) is used, instead.
//	list.PushFront("A") -> {"A", "A", "B", "C"}
//	list.PushFront("B") -> {"B", "A", "A", "B", "C"}
//	list.PushFront("C") -> {"C", "B", "A", "A", "B", "C"}
func (ts *TestSuite[T]) testListInsertPush(t *testing.T, edge linked.Edge, push, clear bool) {
	type PushFunc func(T) *linked.Node[T]
	type InsertFunc func(T, *linked.Node[T]) *linked.Node[T]
	var insert InsertFunc
	var node, edgeNode *linked.Node[T]
	var expect, values []T
	var oppo linked.Edge

	list := ts.List()
	if list.Len() == 0 {
		return // nothing to do
	} else if clear {
		list.Clear()
	}

	insert = func() func(T, *linked.Node[T]) *linked.Node[T] {
		if push {
			fn := [2]PushFunc{list.PushFront, list.PushBack}[edge]
			return func(v T, _ *linked.Node[T]) *linked.Node[T] { return fn(v) }
		} else {
			fn := [2]InsertFunc{list.InsertBefore, list.InsertAfter}[edge]
			return func(v T, n *linked.Node[T]) *linked.Node[T] { return fn(v, n) }
		}
	}()

	version := list.Version()
	values = slices.Clone(ts.Values())

	if expect = values; edge == linked.Front {
		expect = Reversed(expect)
	}

	if !clear {
		expect = slices.Concat(expect, values)
	}

	oppo = edge.Opposite()
	edgeNode = list.Edge(edge)

	for i := range values {
		if node = insert(values[i], nil); !AssertNotNil(t, node) {
			return
		} else if !AssertSame(
			t, node, list.Edge(edge),
			"(%T).%s() should be new node returned by Push%s(%#v)",
			list, edge.List(), edge.List(), values[i],
		) {
			return
		} else if !AssertSame(
			t, edgeNode, node.Peer(oppo),
			"Push%s(%#v).%s() should see old (%T).%s() node as its %s()",
			edge.List(), values[i], oppo.Node(), list, edge.List(), oppo.Node(),
		) {
			return
		} else if edgeNode != nil && !AssertSame(
			t, node, edgeNode.Peer(edge),
			"old (%T).%s() should see new %s as its %s()",
			list, edge.List(), edge.List(), oppo.Node(),
		) {
			return
		} else if version++; !AssertVersion(t, version, list.Version()) {
			return
		}

		edgeNode = node
	}

	AssertListValues(t, expect, list)
}

// testListInsertMirrored produces a list which mirrors itself using the insert
// method specified by edge (either [linked.Before] or [linked.After]). Roughly,
// the procedure follows the following steps:
//
//   - All node pointers currently in the list are copied to a slice.
//   - All values currently in the list are copied to a slice.
//   - Each recorded node is iterated over (by index, in reverse). The value
//     at the opposite corresponding index in the slice of values is then
//     inserted before or after the node (dictated by the edge argument).
//
// The pseudo-code below shows a trivial unrolled example of the effect:
//
//	list := linked.New([]string{"A", "B", "C"})
//	nodes := []*linked.Node[string]{{"A"}, {"B"}, {"C"}} // copied pointers
//	values := []string{"A", "B", "C"}
//
//	// edge == linked.Before
//	list.InsertBefore("A", nodes[2]) // {"A", "B", "A", "C"}
//	list.InsertBefore("B", nodes[1]) // {"C", "A", "B", "B", "C"}
//	list.InsertBefore("C", nodes[0]) // {"C", "A", "B", "B", "A", "C"}
//
//	// edge == linked.After
//	list.InsertAfter("A", nodes[2]) // {"A", "B", "C", "A"}
//	list.InsertAfter("B", nodes[1]) // {"A", "C", "B", "B", "C"}
//	list.InsertAfter("C", nodes[0]) // {"A", "C", "B", "B", "C", "A"}
func (ts *TestSuite[T]) testListInsertMirrored(t *testing.T, edge linked.Edge) {
	var insert func(T, *linked.Node[T]) *linked.Node[T]
	var i, j, k, m int
	var oppo linked.Edge

	list := ts.List()
	n := list.Len()
	if n == 0 {
		return // nothing to do
	}

	nodes := slices.Clone(ts.Nodes())
	values := slices.Clone(ts.Values())

	expectValues := make([]T, n*2)
	expectNodes := make([]*linked.Node[T], n*2)

	switch oppo = edge.Opposite(); edge {
	case linked.Before:
		insert = list.InsertBefore
	default:
		insert = list.InsertAfter
	}

	version := list.Version()

	// this was a bit of an interesting code golf challenge to implement
	// efficiently. here, a single loop with only list.Len() iterations
	// takes care of
	//
	//	- populating the slice of expected values with correct ordering
	//	- populating the slice of expected nodes with correct link ordering
	//	- testing the insert function and verifying node linkages
	for i = range n {
		// index of the duplicated value. It is offset by one only if the
		// edge is [linked.Before].
		j = (i * 2) + int(oppo)

		// shared index for duplicated value *and* where we expect the new
		// node to be. It counts down by 2. It's always even when edge is
		// [linked.Before], otherwise odd.
		k = (2 * n) - int(edge) - (2 * (i + int(oppo)))

		// index where the existing node is expected to be. It's always one
		// away from k, either +1 when edge is [linked.Before], otherwise -1.
		m = (2 * n) - int(oppo) - (2 * (i + int(edge)))

		expectValues[j] = values[i]
		expectValues[k] = values[i]

		expectNodes[m] = nodes[n-i-1]
		expectNodes[k] = insert(values[i], expectNodes[m])

		if !AssertNotNil(t, expectNodes[k]) {
			return
		} else if !Assert(t, values[i], expectNodes[k].Value, "bad value on new node") {
			return
		} else if !Assert(t, true, expectNodes[k].IsMember(list), "new node should be member") {
			return
		} else if !AssertSame(
			t, expectNodes[m], expectNodes[k].Peer(oppo),
			"inserted node should see target node as its %s()",
			oppo.Node(),
		) {
			return
		} else if !AssertSame(
			t, expectNodes[k], expectNodes[m].Peer(edge),
			"target of insert op should see new node as its %s()",
			edge.Node(),
		) {
			return
		} else if version++; !AssertVersion(t, version, list.Version()) {
			return
		}
	}

	actualNodes, err := GatherNodes(list.Front(), false)
	if !AssertNoError(t, err, "encountered error while trying to validate nodes") {
		return
	} else if !AssertNodesEqual(t, expectNodes, actualNodes) {
		return
	}

	AssertSlice(t, expectValues, NodeSliceToValues(actualNodes), "unexpected node values")
}

func (ts *TestSuite[T]) testListInsertMany(t *testing.T, edge linked.Edge, backward bool, useTarget ...*linked.Node[T]) {
	var insert func([]T, *linked.Node[T], ...bool) (*linked.Node[T], *linked.Node[T])
	var expect []T
	var expectLeft, expectRight T
	var target, left, right *linked.Node[T]
	var cutoff int

	list := ts.List()
	values := ts.Values()

	switch edge.Must() {
	case linked.Before:
		insert = list.InsertManyBefore
	default: // linked.After
		insert = list.InsertManyAfter
	}

	// determine the cutoff point so we can calculate the values we expect
	if len(useTarget) == 0 || useTarget[0] == nil {
		if edge == linked.Before {
			cutoff = 0
		} else {
			cutoff = list.Len()
		}

		if len(useTarget) == 0 {
			target = list.Edge(edge)
		}
	} else if target = useTarget[0]; target != nil {
		if cutoff = slices.Index(ts.Nodes(), target); cutoff > -1 {
			cutoff += int(edge)
		}
	}

	// Now calculate the values to expect
	if cutoff > -1 {
		if expect = slices.Clone(values); backward {
			slices.Reverse(expect)
		}

		switch len(expect) {
		default:
			expectLeft = expect[0]
			expectRight = expect[len(expect)-1]
		case 1:
			expectLeft = expect[0]
			expectRight = expectLeft
		case 0:
		}

		expect = slices.Concat(values[:cutoff], expect, values[cutoff:])
	}

	version := list.Version()

	// Finally, the test
	if left, right = insert(ts.Values(), target, backward); cutoff == -1 || len(expect) == 0 {
		if !AssertNil(t, left, "should be a no-op and left %T should be nil", left) {
			return
		} else if !AssertNil(t, right, "should be a no-op and right %T should be nil", right) {
			return
		}

		return
	} else if version++; !AssertNotNil(t, left, "should have left %T", left) {
		return
	} else if !AssertNotNil(t, right, "should have right %T", right) {
		return
	} else if len(expect) == 1 && !AssertSame(
		t, left, right,
		"left and right should be same node when inserting 1 value",
	) {
		return
	} else if !Assert(t, expectLeft, left.Value, "unexpected value of left %T", left) {
		return
	} else if !Assert(t, expectRight, right.Value, "unexpected value of right %T", right) {
		return
	}

	AssertVersion(t, version, list.Version())
	AssertListValues(t, expect, list)
}

// testListToEdge rotates the entire list by moving nodes from the opposite edge of
// the list to the specified edge.
func (ts *TestSuite[T]) testListToEdge(t *testing.T, edge linked.Edge) {
	var foreign linked.Node[T]
	list := ts.List()
	version := list.Version()
	oppo := edge.Opposite()

	n := list.Len()
	expect := slices.Clone(ts.Values())

	if !AssertNil(t, list.MoveToEdge(&foreign, edge), "should return nil when node is foreign") {
		return
	} else if !AssertVersion(t, version, list.Version()) {
		return
	}

	for range n {
		node := list.Edge(oppo)
		oldEdge := list.Edge(edge)
		if !AssertSame(
			t, node, list.MoveToEdge(node, edge),
			"should return same node on success",
		) {
			return
		}

		if node != oldEdge {
			version++
		}

		if !AssertVersion(t, version, list.Version()) {
			return
		}
	}

	AssertListValues(t, expect, list)
}

func (ts *TestSuite[T]) testListTake(t *testing.T, edge linked.Edge) {
	var take func(*linked.Node[T], *linked.Node[T]) *linked.Node[T]
	var expect []T
	list := ts.List()
	size := list.Len()

	if size == 0 {
		return // nothing to do
	} else if expect = Reversed(ts.Values()); edge.Must() == linked.Before {
		take = list.TakeBefore
		expect = slices.Concat(expect, ts.Values())
	} else {
		take = list.TakeAfter
		expect = slices.Concat(ts.Values(), expect)
	}

	oppo := edge.Opposite()
	version := list.Version()
	target := list.Edge(edge)
	node := list.Edge(oppo)

	// Round 1: all no-ops
	for range size {
		if node == nil {
			break
		} else if !AssertNil(t, take(node, target), "should be a no-op") {
			return
		} else if !AssertVersion(t, version, list.Version()) {
			return
		}

		node = node.Peer(oppo)
	}

	// Round 2: Non-member
	other := list.Copy()
	for i := 0; other.Len() > 0; i++ {
		inherit := other.Edge(oppo)
		if !AssertSame(t, inherit, take(inherit, target)) {
			return
		} else if version++; !AssertVersion(t, version, list.Version()) {
			return
		} else if !Assert(t, true, i < size, "other list not draining") {
			return
		}
	}

	AssertListValues(t, expect, list)
}

func (ts *TestSuite[T]) testNodeMove(t *testing.T, edge linked.Edge) {
	type moveFunc func(n, t *linked.Node[T]) *linked.Node[T]
	var move moveFunc
	var node *linked.Node[T]
	var orphan linked.Node[T]

	list := ts.List()
	version := list.Version()
	size := list.Len()
	target := list.Edge(edge)
	oppo := edge.Opposite()
	sig := fmt.Sprintf("(%T).Move%s()", node, edge.Suffix())

	switch edge {
	case linked.Before:
		move = func(n, t *linked.Node[T]) *linked.Node[T] { return n.MoveBefore(t) }
	default:
		move = func(n, t *linked.Node[T]) *linked.Node[T] { return n.MoveAfter(t) }
	}

	if !AssertNil(
		t, move(&orphan, list.Front()),
		"should return nil to indicate failure when (%T).IsOrphan()",
		&orphan,
	) {
		return
	}

	for range size {
		if node = list.Edge(oppo); node != target {
			version++
		}

		if !AssertSame(t, node, move(node, target), "%s should succeed and return receiver node", sig) {
			return
		} else if !AssertVersion(t, version, list.Version()) {
			return
		}

		target = node
	}
}

func (ts *TestSuite[T]) testNodeMoveToEdge(t *testing.T, edge linked.Edge, usesEdgeArg bool) {
	var move func(*linked.Node[T]) *linked.Node[T]
	var orphan linked.Node[T]
	var node *linked.Node[T]

	list := ts.List()
	version := list.Version()
	size := list.Len()
	edgeNode := list.Edge(edge)
	oppo := edge.Opposite()
	sig := fmt.Sprintf("(%T).%s()", list, oppo.List())

	// Setup: finish building signature and wrap the method being tested
	if usesEdgeArg {
		sig = fmt.Sprintf("%s.MoveToEdge(%d /* %s */)", sig, edge, edge.List())
		move = func(n *linked.Node[T]) *linked.Node[T] { return n.MoveToEdge(edge) }
	} else if sig = fmt.Sprintf("%s.MoveTo%s()", sig, edge.List()); edge == linked.Before {
		move = func(n *linked.Node[T]) *linked.Node[T] { return n.MoveToFront() }
	} else {
		move = func(n *linked.Node[T]) *linked.Node[T] { return n.MoveToBack() }
	}

	if !AssertNil(
		t, move(&orphan),
		"should return nil to indicate failure when (%T).IsOrphan()",
		&orphan,
	) {
		return
	}

	for range size {
		if node = list.Edge(oppo); node != edgeNode {
			version++
		}

		if !AssertSame(t, node, move(node), "%s should succeed and return receiver node", sig) {
			return
		} else if !AssertVersion(t, version, list.Version()) {
			return
		}

		edgeNode = node
	}
}

// ****************************************************************************
// === [ ListState ] ==========================================================
// ****************************************************************************

// Copy creates a deep copy of the [State]. It fails the test and returns
// nil if it cannot guarantee that the copy is equivalent in every way to the
// original.
func (s *State[T]) Copy(t *testing.T) *State[T] {
	var cp State[T]
	var err error

	cp.List = s.List.Copy()
	cp.Values = slices.Clone(s.Values)

	if cp.Version = cp.List.Version(); !Assert(
		t, s.List.Version(), cp.Version,
		"While copying %T: version mismatch",
		s,
	) {
		return nil
	} else if cp.Nodes, err = GatherNodes(cp.List.Front(), false); !AssertNoError(
		t, err,
		"While copying %T\n\tError: %v",
		s, err,
	) {
		return nil
	} else if !AssertNoError(
		t, VerifyNodeValues(cp.Nodes, cp.Values),
		"While copying %T: failed to verify expected state\n\tError: %v",
		s, err,
	) {
		return nil
	}

	return &cp
}

// Close releases all values and references held by this [State]. Calls
// [linked.List.Clear] if clear is true.
func (s *State[T]) Close(clear bool) {
	if clear {
		s.List.Clear()
	}

	s.Values = nil
	s.Nodes = nil
	s.List = nil
}

func (s *State[T]) Init(t *testing.T) {
	var err error
	if s.List != nil {
		return
	} else if s.List = linked.New(s.Values); !AssertNotNil(
		t, s.List,
		"constructor should not return a nil %T",
		s.List,
	) {
		return
	} else if s.Nodes, err = GatherNodes(s.List.Front(), false); !AssertNoError(
		t, err,
		"While initializing %T:\n\tError: %v",
		s, err,
	) {
		return
	} else if !AssertNoError(
		t, VerifyNodeValues(s.Nodes, s.Values),
		"While initializing %T:\n\tError: %v",
		s, err,
	) {
		return
	}

	Assert(
		t, uint(0), s.List.Version(),
		"While initializing %T:\n\t(%T).Version(): initial value should be 0",
		s, s.List,
	)
}
