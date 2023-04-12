package server

import "time"

var (
	Unsolved = &Tree{Hash: "root"}
	Queue    map[string]time.Time
	Solved   map[string]string
)

// Goroutine for Queue filtering depending on time.
func Checker() {
	// ..
}
