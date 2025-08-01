package linked_test

import (
	"errors"
	"fmt"
	"iter"
	"reflect"
	"slices"
	"testing"

	"github.com/sbueschel/go-linked"
)

// Assert checks that two values are equal using the "==" operator. Returns true
// if equal. Otherwise, it fails the test, logs a message showing both expected
// and actual values, and returns false.
func Assert[T comparable](t *testing.T, expect, actual T, optMsg ...any) bool {
	if expect != actual {
		t.Logf(
			"Values are not equal:\n"+
				"\tExpect: %#v\n"+
				"\tActual:%#v\n"+
				"%s",
			expect, actual, handleMsgf(optMsg...),
		)

		t.Fail()
		return false
	}

	return true
}

// AssertSlice checks that two slices are equal, using [Assert] for each
// element in the slice. Returns false if a failure occurs, otherwise true.
func AssertSlice[T comparable](t *testing.T, expect, actual []T, optMsg ...any) bool {
	if len(expect) != len(actual) {
		t.Logf(
			"Slices have different lengths:\n"+
				"\tExpect length: %d\n"+
				"\tActual length: %d\n"+
				"%s",
			len(expect), len(actual), handleMsgf(optMsg...),
		)

		t.Fail()
		return false
	} else if expectNil, actualNil := expect == nil, actual == nil; expectNil != actualNil {
		t.Logf(
			"Slices have different nil states:\n"+
				"\tExpect nil: %t\n"+
				"\tActual nil: %t\n"+
				"%s",
			expectNil, actualNil, handleMsgf(optMsg...),
		)

		t.Fail()
		return false
	}

	for i := range expect {
		if !Assert(t, expect[i], actual[i]) {
			t.Logf(
				"\nSlices not equal beginning at index %d:\n"+
					"\tExpect: %#v\n"+
					"\tActual: %#v\n"+
					"%s",
				i, expect, actual, handleMsgf(optMsg...),
			)

			return false
		}
	}

	return true
}

// AssertNil checks that the given value is equal to nil. Fails the test and
// returns false if not, otherwise returns true.
func AssertNil(t *testing.T, value any, optMsg ...any) bool {
	return assertNil(t, reflect.ValueOf(value), true, optMsg...)
}

// AssertNotNil checks that the given value is non-nil. Fails the test and
// returns false if not, otherwise returns true.
func AssertNotNil(t *testing.T, value any, optMsg ...any) bool {
	return assertNil(t, reflect.ValueOf(value), false, optMsg...)
}

// AssertPanic tests that the given function panics at some point during its
// execution, failing the test and returning false, otherwise.
func AssertPanic(t *testing.T, fn func(), optMsg ...any) bool {
	return assertPanic(t, fn, true, optMsg...)
}

// AssertNoPanic tests that the given function does not panic during its
// execution, failing the test and returning false otherwise.
func AssertNoPanic(t *testing.T, fn func(), optMsg ...any) bool {
	return assertPanic(t, fn, false, optMsg...)
}

// AssertError tests that an error has occurred. This function accepts an
// additional target error after the first, which will also check that the
// error being checked identifies as the target (via [errors.Is]).
//
// Any remaining arguments are treated as an optional message with formatting
// arguments.
//
// Fails the test and returns false if the error is nil or if the error does
// not identify as the optional target error.
func AssertError(t *testing.T, err error, args ...any) bool {
	var target error

	if len(args) > 0 {
		if e, ok := args[0].(error); ok {
			target = e
			args = args[1:]
		} else if args[0] == nil {
			// if a literal nil is present, assume that the caller meant
			// a nil error.
			args = args[1:]
		}
	}

	return assertError(t, err, target, true, args...)
}

// AssertNoError tests that no error has occurred. Fails the test and returns
// false if the error is non-nil.
func AssertNoError(t *testing.T, err error, optMsg ...any) bool {
	return assertError(t, err, nil, false, optMsg...)
}

func assertError(t *testing.T, err, target error, should bool, optMsg ...any) bool {
	var haveErr bool
	var suffix string

	if haveErr = err != nil; should == haveErr {
		if target == nil || errors.Is(err, target) {
			return true
		}

		t.Logf(
			"error failed to identify as target error:\n"+
				"\tError:\n"+
				"\t\tType: %T\n"+
				"\t\tMsg : %q\n"+
				"\tTarget:\n"+
				"\t\tType: %T\n"+
				"\t\tMsg : %q\n"+
				"%s",
			err, err.Error(),
			target, target.Error(),
			handleMsgf(optMsg...),
		)

		t.Fail()
		return false
	} else if !should {
		suffix = " not"
	}

	t.Logf("an error should%s have occurred\n%s", suffix, handleMsgf(optMsg...))
	t.Fail()
	return false
}

func assertNil(t *testing.T, rv reflect.Value, should bool, optMsg ...any) bool {
	var isNil bool
	var suffix string

	switch rv.Kind() {
	case reflect.Invalid:
		isNil = true
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Pointer, reflect.Slice:
		isNil = rv.IsNil()
	}

	if should == isNil {
		return true
	} else if !should {
		suffix = " not"
	}

	t.Logf("Value should%s be nil\n%s", suffix, handleMsgf(optMsg...))
	t.Fail()
	return false
}

func assertPanic(t *testing.T, fn func(), should bool, optMsg ...any) bool {
	var panicked bool
	var prefix string

	func() {
		defer func() {
			// after go 1.21, this should only be non-nil when an actual panic has
			// occurred, unless panicnil=1 is set in GODEBUG.
			panicked = recover() != nil
		}()

		fn()
	}()

	if should == panicked {
		return true
	} else if should {
		prefix = "a"
	} else {
		prefix = "no"
	}

	t.Logf("Expected %s panic to occur\n%s", prefix, handleMsgf(optMsg...))
	t.Fail()
	return false
}

// AssertSame fails the test and returns false if expect and actual do not
// point to the same object, otherwise returns true.
func AssertSame[T any](t *testing.T, expect, actual *T, optMsg ...any) bool {
	return assertSame(t, expect, actual, true, optMsg...)
}

// AssertNotSame fails the test and returns false if expect and actual
// point to the same object, otherwise returns true.
func AssertNotSame[T any](t *testing.T, unexpected, actual *T, optMsg ...any) bool {
	return assertSame(t, unexpected, actual, false, optMsg...)
}

func assertSame[T any](t *testing.T, expect, actual *T, should bool, optMsg ...any) bool {
	var isSame bool
	var suffix, expectName string

	if isSame = expect == actual; isSame == should {
		return true
	} else if !should {
		suffix = "distinct from one another"
		expectName = "Not Expected"
	} else {
		suffix = "the same"
		expectName = "Expect"
	}

	t.Logf(
		"pointer values should be %[1]s\n"+
			"\t%[2]s: %[3]T@%[3]p\n"+
			"\tActual: %[4]T@%[4]p\n"+
			"%[5]s",
		suffix, expectName, expect, actual, handleMsgf(optMsg...),
	)

	t.Fail()
	return false
}

func handleMsgf(args ...any) (msg string) {
	var ok bool

	if len(args) == 0 {
		return msg
	} else if msg, ok = args[0].(string); !ok || len(args) == 1 {
		msg = fmt.Sprintf("%v", args[0])
	} else if len(args) > 1 {
		msg = fmt.Sprintf(msg, args[1:]...)
	}

	return msg
}

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
	if !Assert(t, len(nodes), len(expect), "unexpected length") {
		return false
	} else if len(expect) == 0 {
		return true
	}

	for i := range expect {
		if !AssertNotNil(t, nodes[i], "%T at index %d", nodes[i], i) {
			return false
		} else if !Assert(t, expect[i], nodes[i].Value, "%T at index %d", nodes[i], i) {
			t.Logf("Members:\n\tExpect: %#v\n\tActual: %#v\n", expect, NodeSliceToValues(nodes))
			return false
		}
	}

	return true
}

func AssertNodesEqual[T comparable](t *testing.T, expect, actual []*linked.Node[T]) bool {
	if !Assert(t, len(actual), len(expect), "unexpected length") {
		return false
	} else if len(expect) == 0 {
		return true
	}

	for i := range expect {
		if !AssertNotNil(t, actual[i], "%T at index %d", actual[i], i) {
			return false
		} else if !Assert(t, expect[i], actual[i], "should have same %T pointer at index %d", actual[i], i) {
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
	} else if expectNodes, err = GatherNodes(expect.Front(), false); !AssertNoError(
		t, err,
		"while gathering %[1]T from expected %[2]T@%[2]p",
		expectNodes, expect,
	) {
		return false
	} else if actualNodes, err = GatherNodes(expect.Front(), false); !AssertNoError(
		t, err,
		"while gathering %[1]T from actual %[2]T@%[2]p",
		actualNodes, actual,
	) {
		return false
	}

	return AssertNodeValues(t, NodeSliceToValues(expectNodes), actualNodes)
}

func AssertListValues[T comparable](t *testing.T, expect []T, list *linked.List[T]) bool {
	if !AssertNotNil(t, list) {
		return false
	}

	nodes, err := GatherNodes(list.Front(), false)
	return AssertNoError(t, err, "while gathering %T for comparison", nodes) &&
		AssertNodeValues(t, expect, nodes)
}

func AssertVersion(t *testing.T, expect, actual uint) bool {
	return Assert(
		t, expect, actual,
		"Version(): should%s have changed",
		When(expect == actual, " not"),
	)
}
