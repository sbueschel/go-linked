package linked

import (
	"strconv"
	"strings"
)

const (
	Left  = Edge(iota) // Left edge of a [List] or [Node].
	Right              // Right edge of a [List] or [Node].
)

// Aliases for [Left] (for the pedantic amongst us).
const (
	Before = Left
	Prev   = Left
	Front  = Left
)

// Aliases for [Right] (for the pedantic amongst us).
const (
	After = Right
	Next  = Right
	Back  = Right
)

// simple lookup table to get the opposite of [Left] or [Right]. Also has a
// handy side-effect of crashing the program if someone gives us an invalid
// argument when we expect either [Left] or [Right], which is documented.
const opposite = "\x01\x00"

const edgeNames = "LeftRightBeforeAfterFrontBackPrevNext"

// Edge is a simple enumeration type which is used to refer to a [Node]
// relative to another [Node] or [List]. It has only two concrete values:
// [Left] and [Right].
type Edge int

// Must returns the receiver if valid, otherwise it panics.
func (e Edge) Must() Edge {
	_ = opposite[e]
	return e
}

// Opposite returns the opposite value of the receiver or panics if the
// receiver is not a valid [Edge].
func (e Edge) Opposite() Edge { return Edge(opposite[e]) }

// IsValid indicates whether the receiver is considered a valid edge.
func (e Edge) IsValid() bool { return e == Left || e == Right }

// String implements [fmt.Stringer] for [Edge] and returns the canonical
// name of the receiver ("Left" or "Right") or, if invalid, a string in
// the form of
//
//	"(%!BadEdge(%d))"
func (e Edge) String() string {
	switch e {
	case Left:
		return edgeNames[0:4]
	case Right:
		return edgeNames[4:9]
	default:
		return e.invalid()
	}
}

// Suffix returns the method suffix typically associated with the receiver when
// it is used as an argument in a [List] operation (either "Before" or "After").
// If invalid, it returns a string in the form of
//
//	"(%!BadEdge(%d))"
func (e Edge) Suffix() string {
	switch e {
	case Left:
		return edgeNames[9:15]
	case Right:
		return edgeNames[15:20]
	default:
		return e.invalid()
	}
}

// List returns "Front" or "Back" to describe which end of a [List] is referred
// to by this [Edge].
func (e Edge) List() string {
	switch e {
	case Left:
		return edgeNames[20:25]
	case Right:
		return edgeNames[25:29]
	default:
		return e.invalid()
	}
}

// Node returns "Prev" or "Next" to describe the [Node] associated with this
// Edge.
func (e Edge) Node() string {
	switch e {
	case Left:
		return edgeNames[29:33]
	case Right:
		return edgeNames[33:37]
	default:
		return e.invalid()
	}
}

func (e Edge) invalid() string {
	var b strings.Builder
	b.Grow(32)

	b.WriteString("(%!BadEdge(")
	b.WriteString(strconv.Itoa(int(e)))
	b.WriteString("))")
	return b.String()
}
