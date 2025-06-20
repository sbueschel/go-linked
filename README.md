# linked - Generic Linked List

`linked` is a package implementing a doubly linked list for Go.

![Coverage](cover.svg)
[![Docs](https://img.shields.io/badge/Docs-pkg.go.dev-blue)](https://pkg.go.dev/github.com/sbueschel/go-linked)

## Features

- **Generics**: no need to use `any` or `interface{}` (unless you really want to).
- **Iterators**: provides both `range` (as in [`iter.Seq`](https://pkg.go.dev/iter#Seq)) and object-based iterators.
- **...and more**: this package provides plenty in the way of features/ergonomics, but enumerating them is currently a TODO item 🙂

## Getting Started

Install:

```shell
go get github.com/sbueschel/go-linked
```

Usage:

```go
package main

import (
    "fmt"

    "github.com/sbueschel/go-linked"
)

func main() {
    var list linked.List[int]

    for n := range 10 {
        list.PushFront(n)
    }

    list.ForEach(func(v int) { fmt.Printf("%d\n", v) })
}
```
