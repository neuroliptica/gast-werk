package server

import (
	"sync"
	"time"
)

var (
	Unsolved = &Tree{Hash: "root"}
	Queue    map[string]time.Time
	Solved   map[string]string

	// Whenever unsolved, queue or solved is accessed, lock this one.
	DataSync sync.Mutex
)

// Generic thread safe closure-callback.
//
// Usage example:
//
//	var GlobalTable table
//	var TableSync sync.Mutex
//
//	func UnsafeF(x1, x2) f1 { ... access to GlobalTable ... }
//	func UnsafeG(y1) g1 { ... another access to GlobalTable ... }
//
//	Controllers should never call UnsafeG and UnsafeF directly.
//	Instead, it should be used with as a clojure-callback to genric Locker:
//
//	func SafeF(x1, x2) f1 {
//		return Locker[f1](&TableSync, func() f1 {
//			return UnsafeF(x1, x2);
//		})
//	}
//
//	func SafeG(y1) g1 {
//		return Locker[g1](&TableSync, func() g1 {
//			return UnsafeG(y1);
//		})
//	}

type Closure[ReturnType any] func() ReturnType

func Locker[ReturnT any](mu *sync.Mutex, callback Closure[ReturnT]) ReturnT {
	mu.Lock()
	defer mu.Unlock()
	return callback()
}

// Thread safe queue aborted checker.
func Checker() {
	Locker[struct{}](&DataSync, func() struct{} {
		return struct{}{}
	})
}
