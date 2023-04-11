// bintree.go: provides a binary tree structure for manipulating with hashes.
// Extract deepmost node hash will work as a "get random hash from map".
// Because of it's simple semantic role, there is no delete or other methods provided.

package server

import "math"

type Tree struct {
	Hash        string
	Left, Right *Tree
}

// Constructor.
func InitNode() *Tree {
	return &Tree{}
}

// Add hash to tree.
func (t *Tree) Add(hash string) {
	if t.Hash == "" || t.Hash == hash {
		t.Hash = hash
		return
	}
	if t.Hash > hash {
		if t.Left == nil {
			t.Left = InitNode()
		}
		t.Left.Add(hash)
	} else {
		if t.Right == nil {
			t.Right = InitNode()
		}
		t.Right.Add(hash)
	}
}

// Check miximum height of the tree.
func (t *Tree) Depth() int32 {
	var ld, rd float64
	if t.Left != nil {
		ld = float64(t.Left.Depth())
	}
	if t.Right != nil {
		rd = float64(t.Right.Depth())
	}
	return 1 + int32(math.Max(ld, rd))
}

// Extract most deep node hash and delete it.
func ExtractLeaves(master, slave *Tree) string {
	if slave == nil {
		return ""
	}
	if slave.Left == nil && slave.Right == nil {
		if master == nil {
			return slave.Hash
		}
		if master.Right == slave {
			master.Right = nil
		} else {
			master.Left = nil
		}
		return slave.Hash
	}
	if slave.Left == nil {
		return ExtractLeaves(slave, slave.Right)
	}
	if slave.Right == nil {
		return ExtractLeaves(slave, slave.Left)
	}
	// Both slave's childs aren't nil.
	if slave.Left.Depth() > slave.Right.Depth() {
		return ExtractLeaves(slave, slave.Left)
	}
	return ExtractLeaves(slave, slave.Right)
}
