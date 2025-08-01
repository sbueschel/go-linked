package linked_test

import (
	"testing"

	"github.com/sbueschel/go-linked"
)

type IteratorTestSuite[T comparable] struct {
	Iter          func(...bool) linked.Iterator[T]
	ExpectValues  []T
	ExpectNodes   []*linked.Node[T]
	ExpectVersion uint
	Backward      bool
}

// NewIteratorTestSuite initializes an object that tests [linked.Iterator] using
// one of two constructors, which it stores in [IteratorTestSuite.Iter].
//
// If the [linked.Node] given is nil, it tests [linked.List.Iter], otherwise it
// tests [linked.Node.Iter].
func NewIteratorTestSuite[T comparable](s *State[T], node *linked.Node[T], backward bool) *IteratorTestSuite[T] {
	var its IteratorTestSuite[T]

	if its.Backward = backward; node == nil {
		// node will get set to a non-nil value so that we can determine
		// expected values in the next section.
		if its.Iter = s.List.Iter; backward {
			node = s.List.Back()
		} else {
			node = s.List.Front()
		}
	} else {
		its.Iter = node.Iter
	}

	if backward && node == s.List.Front() {
		its.ExpectValues = s.Values[:min(len(s.Values), 1)]
		its.ExpectNodes = s.Nodes[:min(len(s.Values), 1)]
	} else if backward && node == s.List.Back() {
		its.ExpectValues = Reversed(s.Values)
		its.ExpectNodes = Reversed(s.Nodes)
	} else if !backward && node == s.List.Front() {
		its.ExpectValues = s.Values
		its.ExpectNodes = s.Nodes
	} else if !backward && node == s.List.Back() {
		its.ExpectValues = s.Values[max(0, len(s.Values)-1):]
		its.ExpectNodes = s.Nodes[max(0, len(s.Nodes)-1):]
	}

	its.ExpectVersion = s.List.Version()
	return &its
}

func (its *IteratorTestSuite[T]) Test(t *testing.T) {
	t.Run("Next", its.TestNext)
	t.Run("All", its.TestAll)
	t.Run("Gather", its.TestGather)
	t.Run("Into", its.TestInto)
}

func (its *IteratorTestSuite[T]) TestNext(t *testing.T) {
	var j int
	it := its.Iter(its.Backward)

	if !Assert(
		t, its.Backward, it.IsBackward(),
		"IsBackward() returned incorrect value",
	) {
		return
	} else if !Assert(
		t, its.ExpectVersion, it.Version(),
		"Version(): should match version of originating list",
	) {
		return
	}

	for j = 0; it.Next(); j++ {
		if !Assert(t, j, it.Pos, "Pos") {
			return
		} else if !Assert(t, j+1, it.Count(), "Count()") {
			return
		} else if !AssertNotNil(t, its.ExpectNodes[j], "Node") {
			return
		} else if !Assert(t, its.ExpectNodes[j], it.Node, "Node") {
			return
		} else if !Assert(t, its.ExpectValues[j], it.Node.Value, "Node.Value") {
			return
		}
	}

	Assert(t, false, it.Next(), "Next() should return false after completion")
	Assert(t, len(its.ExpectValues), j, "Total number of items (j)")
}

func (its *IteratorTestSuite[T]) TestGather(t *testing.T) {
	var expect []T
	it := its.Iter(its.Backward)

	if len(its.ExpectNodes) > 0 {
		expect = NodeSliceToValues(its.ExpectNodes)
	}

	AssertSlice(t, expect, it.Gather())
	AssertNil(t, it.Gather(), "iterator should be exhausted")
}

func (its *IteratorTestSuite[T]) TestInto(t *testing.T) {
	var buf [2]T
	var expect, actual []T
	it := its.Iter(its.Backward)

	if len(its.ExpectNodes) > 0 {
		expect = NodeSliceToValues(its.ExpectNodes)
	}

	for it.Node != nil {
		n := it.Into(buf[:])
		actual = append(actual, buf[:n]...)

		if len(actual) > len(expect) {
			t.Logf(
				"too many items: expected at most %d, copied %d",
				len(expect), len(actual),
			)

			t.Fail()
			return
		}
	}

	AssertSlice(t, expect, actual)
}

func (its *IteratorTestSuite[T]) TestAll(t *testing.T) {
	it := its.Iter(its.Backward)
	AssertNoError(t, VerifyIterSeq(t, its.ExpectNodes, it.All()))
}
