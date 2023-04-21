package server

import (
	"fmt"
	"sync"
	"time"
)

var (
	Total    = make(map[string]string)
	Unsolved = &Tree{Hash: "root"}
	Queue    = make(map[string]time.Time)
	Solved   = make(map[string]string)

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
//	Instead, it should be used with as a closure-callback to genric Locker:
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
	for {
		DataSync.Lock()
		fmt.Printf("running checker... %d will be checked.\n", len(Queue))
		for hash, value := range Queue {
			if time.Since(value).Minutes() >= 1.0 {
				delete(Queue, hash)
				Unsolved.Add(hash)
			}
		}
		DataSync.Unlock()
		time.Sleep(30 * time.Second)
	}
}
