package linked_test

import (
	"fmt"

	"github.com/sbueschel/go-linked"
)

func ExampleList_ForEach() {
	list := linked.New([]string{"A", "B", "C", "D"})
	list.ForEach(func(v string) { fmt.Println(v) })
	list.ForEach(func(v string) { fmt.Println(v) }, true) // reverse order

	// Output:
	// A
	// B
	// C
	// D
	// D
	// C
	// B
	// A
}

func ExampleList_InsertManyBefore() {
	list := linked.New([]string{"A", "G"})

	target := list.Back()
	oldPeer := target.Prev()

	left, right := list.InsertManyBefore([]string{"B", "C", "D", "E", "F"}, target)

	fmt.Printf("inserted %q after %q\n", left.Value, oldPeer.Value)
	fmt.Printf("inserted %q before %q\n", right.Value, target.Value)
	list.ForEach(func(v string) { fmt.Println(v) })

	// Output:
	// inserted "B" after "A"
	// inserted "F" before "G"
	// A
	// B
	// C
	// D
	// E
	// F
	// G
}

func ExampleList_InsertManyAfter() {
	list := linked.New([]string{"G", "A"})

	target := list.Front()
	oldPeer := target.Next()

	left, right := list.InsertManyAfter(
		[]string{"B", "C", "D", "E", "F"},
		target,
		true, // reverse the order when inserting
	)

	fmt.Printf("inserted %q after %q\n", left.Value, target.Value)
	fmt.Printf("inserted %q before %q\n", right.Value, oldPeer.Value)
	list.ForEach(func(v string) { fmt.Println(v) })

	// Output:
	// inserted "F" after "G"
	// inserted "B" before "A"
	// G
	// F
	// E
	// D
	// C
	// B
	// A
}

func ExampleList_Iter() {
	list := linked.New([]string{"A", "B", "C", "D", "E", "F", "G"})

	for it := list.Iter(); it.Next(); {
		fmt.Printf("%q // pos=%d, count=%d\n", it.Node.Value, it.Pos, it.Count())
	}

	// Output:
	// "A" // pos=0, count=1
	// "B" // pos=1, count=2
	// "C" // pos=2, count=3
	// "D" // pos=3, count=4
	// "E" // pos=4, count=5
	// "F" // pos=5, count=6
	// "G" // pos=6, count=7
}

func ExampleList_PushFront() {
	var list linked.List[int]

	for v := range 5 {
		list.PushFront(v)
	}

	list.ForEach(func(v int) { fmt.Printf("%d\n", v) })

	// Output:
	// 4
	// 3
	// 2
	// 1
	// 0
}

func ExampleList_PushBack() {
	var list linked.List[int]

	for v := range 5 {
		list.PushBack(v)
	}

	list.ForEach(func(v int) { fmt.Printf("%d\n", v) })

	// Output:
	// 0
	// 1
	// 2
	// 3
	// 4
}

func ExampleList_PopFront() {
	list := linked.New([]int{1, 2, 3, 4, 5})

	for list.Len() > 0 {
		fmt.Printf("%d\n", list.PopFront().Value)
	}

	// Output:
	// 1
	// 2
	// 3
	// 4
	// 5
}

func ExampleList_PopBack() {
	list := linked.New([]int{1, 2, 3, 4, 5})

	for list.Len() > 0 {
		fmt.Printf("%d\n", list.PopBack().Value)
	}

	// Output:
	// 5
	// 4
	// 3
	// 2
	// 1
}
