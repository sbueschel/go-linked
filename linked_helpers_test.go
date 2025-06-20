package linked_test

import (
	"fmt"
	"iter"
	"slices"
	"testing"

	"github.com/sbueschel/go-linked"
	"github.com/stretchr/testify/assert"
)

// When is a functional ternary statement which returns "this" when cond is
// true. If cond is false, it returns "that" (if given), otherwise the zero
// value of T.
func When[T any](cond bool, this T, that ...T) (v T) {
	if cond {
		return this
	} else if len(that) > 0 {
		return that[0]
	}

	return v
}

// Reversed creates a copy of the given slice, then reverses the copy.
func Reversed[T any](slice []T) (cp []T) {
	cp = slices.Clone(slice)
	slices.Reverse(cp)
	return cp
}

// GatherNodes produces a slice of all [linked.Node]s starting at (inclusive of)
// the given [linked.Node]. If backward is true, it retrieves each subsequent
// node using the equivalent of [linked.Node.Prev], otherwise [linked.Node.Next].
//
// This function returns a non-nil error if it sees an individual [linked.Node]
// more than once. If this happens, it also returns a nil slice.
func GatherNodes[T comparable](node *linked.Node[T], backward bool) ([]*linked.Node[T], error) {
	var edge linked.Edge // initially equivalent to [linked.Prev]
	var nodes []*linked.Node[T]
	seen := map[*linked.Node[T]]int{}

	if !backward {
		edge = linked.Next
	}

	for i := 0; node != nil; i, node = i+1, node.Peer(edge) {
		if initial, ok := seen[node]; ok {
			return nil, fmt.Errorf(
				"encountered duplicate %[1]T at index %[2]d\n\t"+
					"Initial index: %[3]d\n\t"+
					"Node: %#[1]v",
				node, i, initial,
			)
		}

		seen[node] = i
		nodes = append(nodes, node)
	}

	return nodes, nil
}

// VerifyNodeValues returns a non-nil error if the value of each node does not
// match the expected values.
func VerifyNodeValues[T comparable](nodes []*linked.Node[T], expect []T) error {
	if len(nodes) != len(expect) {
		return fmt.Errorf("expected %d items in %T (have %d)", len(expect), nodes, len(nodes))
	}

	for i := range expect {
		if nodes[i] == nil {
			return fmt.Errorf("at index %d: %T is nil", i, nodes[i])
		} else if nodes[i].Value != expect[i] {
			return fmt.Errorf("at index %d:\n\tExpect:%#v\n\tActual: %#v", i, expect[i], nodes[i].Value)
		}
	}

	return nil
}

func NodeSliceToValues[T comparable](nodes []*linked.Node[T]) (values []T) {
	values = make([]T, len(nodes))
	for i := range nodes {
		values[i] = nodes[i].Value
	}

	return values
}

func NodeString[T comparable](node *linked.Node[T]) string {
	if node == nil {
		return "(nil)"
	}

	return fmt.Sprintf("(%[1]T@%[1]p)(%#[2]v)", node, node.Value)
}

// GatherIterSeq exhausts the given iterator, copying each yielded value into
// a slice. When unique is true, this function will track which values it has
// seen, returning an error if a duplicate is present.
func GatherIterSeq[T comparable](seq iter.Seq[T], unique bool) ([]T, error) {
	var i int
	var values []T
	seen := map[T]int{}

	for v := range seq {
		if at, ok := seen[v]; ok && unique {
			return nil, fmt.Errorf(
				"encountered duplicate %[1]T at index %[2]d\n\t"+
					"First: %[3]d\n\t"+
					"Value: %#[1]v",
				v, i, at,
			)
		} else if values = append(values, v); unique {
			seen[v] = i
		}

		i++
	}

	return values, nil
}

func VerifyIterSeq[T comparable](t *testing.T, expect []T, seq iter.Seq[T]) error {
	var i int

	for v := range seq {
		if i+1 > len(expect) {
			return fmt.Errorf(
				"possible infinite loop or underlying mutation: iter.Seq[%T] "+
					"yielded too many values: expected %d, got %d",
				v, len(expect), i+1,
			)
		} else if v != expect[i] {
			return fmt.Errorf(
				"unexpected value at index %d:\n\tExpect: %#v\n\tActual: %#v",
				i, expect[i], v,
			)
		}

		i++
	}

	return nil
}

func AssertNodeValues[T comparable](t *testing.T, expect []T, nodes []*linked.Node[T], msgAndArgs ...any) (ok bool) {
	if !assert.Len(
		t, nodes, len(expect),
		"expected %d items in %T (have %d)",
		len(expect), nodes, len(nodes),
	) {
		return false
	} else if len(expect) == 0 {
		return true
	}

	for i := range expect {
		if !assert.NotNil(t, nodes[i], "%T at index %d", nodes[i], i) {
			return false
		} else if !assert.Equal(t, expect[i], nodes[i].Value, "%T at index %d", nodes[i], i) {
			t.Logf("Members:\n\tExpect: %#v\n\tActual: %#v\n", expect, NodeSliceToValues(nodes))
			return false
		}
	}

	return true
}

func AssertNodesEqual[T comparable](t *testing.T, expect, actual []*linked.Node[T]) bool {
	if !assert.Len(
		t, actual, len(expect),
		"expected %d items in %T (have %d)",
		len(expect), actual, len(actual),
	) {
		return false
	} else if len(expect) == 0 {
		return true
	}

	for i := range expect {
		if !assert.NotNil(t, actual[i], "%T at index %d", actual[i], i) {
			return false
		} else if !assert.Same(t, expect[i], actual[i], "%T at index %d", actual[i], i) {
			return false
		}
	}

	return true
}

// AssertListEqual checks that actual contains all the same values in the same
// order as expected.
func AssertListEqual[T comparable](t *testing.T, expect, actual *linked.List[T]) bool {
	var expectNodes, actualNodes []*linked.Node[T]
	var err error
	if expect == actual {
		return true
	} else if expectNodes, err = GatherNodes(expect.Front(), false); !assert.NoError(
		t, err,
		"while gathering %[1]T from expected %[2]T@%[2]p",
		expectNodes, expect,
	) {
		return false
	} else if actualNodes, err = GatherNodes(expect.Front(), false); !assert.NoError(
		t, err,
		"while gathering %[1]T from actual %[2]T@%[2]p",
		actualNodes, actual,
	) {
		return false
	}

	return AssertNodeValues(t, NodeSliceToValues(expectNodes), actualNodes)
}

func AssertListValues[T comparable](t *testing.T, expect []T, list *linked.List[T]) bool {
	if !assert.NotNil(t, list) {
		return false
	}

	nodes, err := GatherNodes(list.Front(), false)
	return assert.NoError(t, err, "while gathering %T for comparison", nodes) &&
		AssertNodeValues(t, expect, nodes)
}

func AssertVersion(t *testing.T, expect, actual uint) bool {
	return assert.Equal(
		t, expect, actual,
		"Version(): should%s have changed",
		When(expect == actual, " not"),
	)
}
