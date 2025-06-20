package linked_test

import (
	stdlist "container/list"
	"fmt"
	"iter"
	"os"
	"strconv"
	"testing"
	"time"

	"github.com/sbueschel/go-linked"
)

const (
	EnvironBenchListSize = "BENCH_LIST_SIZE"

	// DefaultBenchListSize of 256Ki elements should use at most 300MiB of
	// memory if running all benchmarks.
	DefaultBenchListSize = 1 << 18
)

var confBenchListSize int

func init() { confBenchListSize = getListBenchSize() }

type ListBenchmark[T any] struct {
	Name    string
	values  []T
	stdlist *stdlist.List
	list    *linked.List[T]
	sink    T
}

// CompatNode wraps a [stdlist.Element] and implements [iter.Seq]
// methods for benchmarking purposes. They are implemented as closely as
// possible to those of [linked.Node].
type CompatNode[T any] struct {
	*stdlist.Element
}

// CompatIterator is as similar as possible to [linked.Iterator] and is used for
// benchmarking purposes only.
type CompatIterator struct {
	Node    *stdlist.Element
	Pos     int
	version uint
	flags   uint32
}

// Simple struct used for benchmarking.
type Simple[T any] struct {
	Value T
}

// Struct containing many fields; used for benchmarking.
type Struct struct {
	Data []byte
	More string
	Num  [3]int64
	Sub  *Simple[int]
}

func BenchmarkList(b *testing.B) {
	NewListBenchmark("uint64", NewUint64).Run(b)
	NewListBenchmark("slice", NewByteSlice).Run(b)
	NewListBenchmark("struct", NewStruct).Run(b)
	NewListBenchmark("pointer", NewStructPointer).Run(b)
}

func ReportLoopMetrics(b *testing.B, numValues int, dur time.Duration) {
	b.ReportMetric(0, "ns/op")
	b.ReportMetric(float64(dur)/float64(b.N), "ns/loop")
	b.ReportMetric(float64(dur)/float64(numValues), "ns/val")
	b.ReportMetric(float64(numValues), "vals")
}

func NewUint64(i int) uint64 { return uint64(i) }

func NewByteSlice(i int) []byte {
	return fmt.Appendf(nil, "byte slice %d", i)
}

func NewStruct(i int) (s Struct) {
	s.Data = NewByteSlice(i)
	s.More = fmt.Sprintf("string %d", i)
	s.Num = [3]int64{int64(i), int64(i) >> 1, int64(i) << 1}
	s.Sub = &Simple[int]{Value: -i}
	return s
}

func NewStructPointer(i int) *Struct {
	s := NewStruct(i)
	return &s
}

func NewListBenchmark[T any](name string, newFn func(int) T) *ListBenchmark[T] {
	l := &ListBenchmark[T]{Name: name}
	values := make([]T, confBenchListSize)
	stdlist := stdlist.New()

	for i := range confBenchListSize {
		values[i] = newFn(i)
		stdlist.PushBack(values[i])
	}

	l.values = values
	l.stdlist = stdlist
	l.list = linked.New(values)

	return l
}

func (l *ListBenchmark[T]) Run(b *testing.B) { b.Run(l.Name, l.Benchmark) }

func (l *ListBenchmark[T]) Benchmark(b *testing.B) {
	b.Run("Iter", l.BenchmarkIter)
	b.Run("Push", l.BenchmarkPush)
	b.Run("Move", l.BenchmarkMove)
}

// === [ Iter ] ===============================================================
// These benchmarks evaluate the speed of using various iterators (and their
// make-shift equivalents) for [linked.List] and [stdlist.List] elements.
// ============================================================================

func (l *ListBenchmark[T]) BenchmarkIter(b *testing.B) {
	b.Run("NextElem", l.BenchmarkIterNextElem)
	b.Run("Iterator", l.BenchmarkIterIterator)
	b.Run("iter.Seq", l.BenchmarkIterIterSeq)
}

func (l *ListBenchmark[T]) BenchmarkIterNextElem(b *testing.B) {
	b.Run("Pkg", l.BenchmarkIterNextElemPkg)
	b.Run("Std", l.BenchmarkIterNextElemStd)
}

func (l *ListBenchmark[T]) BenchmarkIterNextElemPkg(b *testing.B) {
	var count int
	list := l.list
	start := time.Now()

	for b.Loop() {
		for e := list.Front(); e != nil; e = e.Next() {
			count++
		}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

func (l *ListBenchmark[T]) BenchmarkIterNextElemStd(b *testing.B) {
	var count int
	list := l.stdlist
	start := time.Now()

	for b.Loop() {
		for e := list.Front(); e != nil; e = e.Next() {
			count++
		}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

// === [ Iter ][ Iterator ] ===================================================
// These benchmarks evaluate the speed of using [linked.Iterator] to traverse a
// [linked.List] and the equivalent implementation for [stdlist.List].
// ============================================================================

func (l *ListBenchmark[T]) BenchmarkIterIterator(b *testing.B) {
	b.Run("Pkg", l.BenchmarkIterIteratorPkg)
	b.Run("Std", l.BenchmarkIterIteratorStd)
}

func (l *ListBenchmark[T]) BenchmarkIterIteratorPkg(b *testing.B) {
	var count int
	var it linked.Iterator[T]
	list := l.list
	start := time.Now()

	for b.Loop() {
		for it.Init(list.Front()); it.Next(); {
			count++
		}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

func (l *ListBenchmark[T]) BenchmarkIterIteratorStd(b *testing.B) {
	var count int
	var it CompatIterator
	list := l.stdlist
	start := time.Now()

	for b.Loop() {
		for it.Init(list.Front()); it.Next(); {
			count++
		}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

// === [ Iter ][ iter.Seq ] ===================================================
// These benchmarks evaluate the speed of using [iter.Seq]-style iterators to
// iterate over values of a [linked.List] and [stdlist.List].
// ============================================================================

func (l *ListBenchmark[T]) BenchmarkIterIterSeq(b *testing.B) {
	b.Run("Pkg", l.BenchmarkIterIterSeqPkg)
	b.Run("Std", l.BenchmarkIterIterSeqStd)
}

func (l *ListBenchmark[T]) BenchmarkIterIterSeqPkg(b *testing.B) {
	var count int
	var sink T
	var node *linked.Node[T]
	list := l.list
	start := time.Now()

	for b.Loop() {
		node = list.Front()
		for v := range node.All() {
			sink = v
			count++
		}
	}

	ReportLoopMetrics(b, count, time.Since(start))
	l.sink = sink
}

func (l *ListBenchmark[T]) BenchmarkIterIterSeqStd(b *testing.B) {
	var count int
	var sink T
	node := &CompatNode[T]{}
	list := l.stdlist
	start := time.Now()

	for b.Loop() {
		node.Element = list.Front()
		for v := range node.All() {
			sink = v
			count++
		}
	}

	ReportLoopMetrics(b, count, time.Since(start))
	l.sink = sink
}

// === [ Move ] ===============================================================
// These benchmarks evaluate the speed of moving existing items to the front
// and back of a [linked.List] and [stdlist.List].
// ============================================================================

func (l *ListBenchmark[T]) BenchmarkMove(b *testing.B) {
	b.Run("ToFront", l.BenchmarkMoveToFront)
	b.Run("ToBack", l.BenchmarkMoveToBack)
}

func (l *ListBenchmark[T]) BenchmarkMoveToFront(b *testing.B) {
	b.Run("Pkg", l.BenchmarkMoveToFrontPkg)
	b.Run("Std", l.BenchmarkMoveToFrontStd)
}

func (l *ListBenchmark[T]) BenchmarkMoveToFrontPkg(b *testing.B) {
	list := l.list
	for b.Loop() {
		list.MoveToFront(list.Back())
	}
}

func (l *ListBenchmark[T]) BenchmarkMoveToFrontStd(b *testing.B) {
	list := l.stdlist
	for b.Loop() {
		list.MoveToFront(list.Back())
	}
}

func (l *ListBenchmark[T]) BenchmarkMoveToBack(b *testing.B) {
	b.Run("Pkg", l.BenchmarkMoveToBackPkg)
	b.Run("Std", l.BenchmarkMoveToBackStd)
}

func (l *ListBenchmark[T]) BenchmarkMoveToBackPkg(b *testing.B) {
	list := l.list
	for b.Loop() {
		list.MoveToBack(list.Front())
	}
}

func (l *ListBenchmark[T]) BenchmarkMoveToBackStd(b *testing.B) {
	list := l.stdlist
	for b.Loop() {
		list.MoveToBack(list.Front())
	}
}

// === [ Push ] ===============================================================
// These benchmarks evaluate the speed of adding new items to the front and
// back of a [linked.List] and [stdlist.List].
// ============================================================================

func (l *ListBenchmark[T]) BenchmarkPush(b *testing.B) {
	b.Run("ToFront", l.BenchmarkPushToFront)
	b.Run("ToBack", l.BenchmarkPushToBack)
}

func (l *ListBenchmark[T]) BenchmarkPushToBack(b *testing.B) {
	b.Run("Pkg", l.BenchmarkPushToBackPkg)
	b.Run("Std", l.BenchmarkPushToBackStd)
}

func (l *ListBenchmark[T]) BenchmarkPushToFront(b *testing.B) {
	b.Run("Pkg", l.BenchmarkPushToFrontPkg)
	b.Run("Std", l.BenchmarkPushToFrontStd)
}

func (l *ListBenchmark[T]) BenchmarkPushToBackPkg(b *testing.B) {
	var count int
	var list linked.List[T]

	values := l.values
	start := time.Now()

	for b.Loop() {
		for i := range values {
			list.PushBack(values[i])
			count++
		}

		list = linked.List[T]{}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

func (l *ListBenchmark[T]) BenchmarkPushToBackStd(b *testing.B) {
	var count int
	var list stdlist.List

	values := l.values
	start := time.Now()

	for b.Loop() {
		for i := range values {
			list.PushBack(values[i])
			count++
		}

		list = stdlist.List{}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

func (l *ListBenchmark[T]) BenchmarkPushToFrontPkg(b *testing.B) {
	var count int
	var list linked.List[T]

	values := l.values
	start := time.Now()

	for b.Loop() {
		for i := range values {
			list.PushFront(values[i])
			count++
		}

		list = linked.List[T]{}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

func (l *ListBenchmark[T]) BenchmarkPushToFrontStd(b *testing.B) {
	var count int
	var list stdlist.List

	values := l.values
	start := time.Now()

	for b.Loop() {
		for i := range values {
			list.PushFront(values[i])
			count++
		}

		list = stdlist.List{}
	}

	ReportLoopMetrics(b, count, time.Since(start))
}

// === [ Equivalency ] ========================================================

const (
	iterBackward = 1 << iota
	iterStarted  = 2
)

func (cn *CompatNode[T]) All() iter.Seq[T] {
	if cn != nil {
		return cn.all
	}

	return linked.NoopSeq[T]
}

func (cn *CompatNode[T]) Backward() iter.Seq[T] {
	if cn != nil {
		return cn.backward
	}

	return linked.NoopSeq[T]
}

func (cn *CompatNode[T]) all(yield func(T) bool) {
	for n := cn.Element; n != nil && yield(n.Value.(T)); n = n.Next() {
	}
}

func (cn *CompatNode[T]) backward(yield func(T) bool) {
	for n := cn.Element; n != nil && yield(n.Value.(T)); n = n.Prev() {
	}
}

func (cit *CompatIterator) Init(elem *stdlist.Element, backward ...bool) {
	cit.Node = elem
	cit.Pos = 0
	cit.version = 0
	if len(backward) > 0 && backward[0] {
		cit.flags = iterBackward
	}
}

func (cit *CompatIterator) All() iter.Seq[*stdlist.Element] {
	if cit.flags&1 == 0 {
		return cit.all
	}

	return cit.backward
}

func (cit *CompatIterator) all(yield func(*stdlist.Element) bool) {
	for n := cit.Node; n != nil && yield(n); n = n.Next() {
	}
}

func (cit *CompatIterator) backward(yield func(*stdlist.Element) bool) {
	for n := cit.Node; n != nil && yield(n); n = n.Prev() {
	}
}

func (cit *CompatIterator) Next() bool {
	node := cit.Node

	if node == nil {
		return false
	} else if flags := cit.flags; flags&iterStarted == 0 {
		cit.flags |= iterStarted
		return true
	} else if flags&1 == 0 {
		node = node.Next()
	} else {
		node = node.Prev()
	}

	if node != nil {
		cit.Pos++
		cit.Node = node
		return true
	}

	cit.Node = nil
	return false
}

func (cit *CompatIterator) Count() (n int) {
	if cit.flags&iterStarted != 0 {
		n = cit.Pos + 1
	}

	return n
}

func (cit *CompatIterator) IsBackwards() bool { return cit.flags&1 != 0 }

func (cit *CompatIterator) Again() { cit.flags &^= iterStarted }

func (cit *CompatIterator) Version() uint { return cit.version }

func getListBenchSize() int {
	var err error
	val, ok := os.LookupEnv(EnvironBenchListSize)
	if !ok {
		return DefaultBenchListSize
	}

	u, err := strconv.ParseUint(val, 10, 64)
	if err != nil {
		fmt.Fprintf(
			os.Stderr,
			"warning: failed to parse value from environment variable %q: %v\n\n"+
				"falling back to default list size of %d\n",
			EnvironBenchListSize, err, DefaultBenchListSize,
		)

		u = DefaultBenchListSize
	}

	if u == 0 {
		fmt.Fprintf(
			os.Stderr,
			"warning: list size of 0 is invalid. falling back "+
				"to default list size of %d\n",
			DefaultBenchListSize,
		)

		u = DefaultBenchListSize
	}

	return int(u)
}
