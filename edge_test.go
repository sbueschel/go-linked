package linked_test

import (
	"testing"

	"github.com/sbueschel/go-linked"
	"github.com/stretchr/testify/assert"
)

func TestEdge(t *testing.T) {
	for _, tc := range []struct {
		Name          string
		Edge          linked.Edge
		ExpectIsValid bool
		ExpectString  string
		ExpectSuffix  string
		ExpectList    string
		ExpectNode    string
	}{
		{
			Name:          "Left",
			Edge:          linked.Left,
			ExpectIsValid: true,
			ExpectString:  "Left",
			ExpectSuffix:  "Before",
			ExpectList:    "Front",
			ExpectNode:    "Prev",
		},
		{
			Name:          "Right",
			Edge:          linked.Right,
			ExpectIsValid: true,
			ExpectString:  "Right",
			ExpectSuffix:  "After",
			ExpectList:    "Back",
			ExpectNode:    "Next",
		},
		{
			Name:          "BadNegative",
			Edge:          -1,
			ExpectIsValid: false,
			ExpectString:  "(%!BadEdge(-1))",
			ExpectSuffix:  "(%!BadEdge(-1))",
			ExpectList:    "(%!BadEdge(-1))",
			ExpectNode:    "(%!BadEdge(-1))",
		},
		{
			Name:          "BadPositive",
			Edge:          2,
			ExpectIsValid: false,
			ExpectString:  "(%!BadEdge(2))",
			ExpectSuffix:  "(%!BadEdge(2))",
			ExpectList:    "(%!BadEdge(2))",
			ExpectNode:    "(%!BadEdge(2))",
		},
	} {
		t.Run(tc.Name, func(t *testing.T) {
			t.Run("IsValid", func(t *testing.T) {
				assert.Equal(t, tc.ExpectIsValid, tc.Edge.IsValid())
			})

			t.Run("String", func(t *testing.T) {
				assert.Equal(t, tc.ExpectString, tc.Edge.String())
			})

			t.Run("Suffix", func(t *testing.T) {
				assert.Equal(t, tc.ExpectSuffix, tc.Edge.Suffix())
			})

			t.Run("List", func(t *testing.T) {
				assert.Equal(t, tc.ExpectList, tc.Edge.List())
			})

			t.Run("Node", func(t *testing.T) {
				assert.Equal(t, tc.ExpectNode, tc.Edge.Node())
			})

			t.Run("Must", func(t *testing.T) {
				if tc.ExpectIsValid {
					assert.NotPanics(t, func() {
						assert.Equal(t, tc.Edge, tc.Edge.Must())
					})
				} else {
					assert.Panics(t, func() { _ = tc.Edge.Must() })
				}
			})

			t.Run("Opposite", func(t *testing.T) {
				var expect linked.Edge
				if !tc.ExpectIsValid {
					assert.Panics(t, func() { _ = tc.Edge.Opposite() })
					return
				} else if tc.Edge == linked.Left {
					expect = linked.Right
				}

				assert.NotPanics(t, func() {
					assert.Equal(t, expect, tc.Edge.Opposite())
				})
			})
		})
	}
}
