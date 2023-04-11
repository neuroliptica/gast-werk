// master.go: goroutines for input processing.
// many-readers-one-writer

package server

import "time"

var (
	Unsolved = &Tree{Hash: "root"}
	Queue    map[string]time.Time
	Solved   map[string]string
)

// Goroutine to process /captcha/get requests.
func GetMaster(master chan *GetRequest) {
	for req := range master {
		// mutex lock for Unsolved, Queue and Solved here.
		if Unsolved.Depth() == 1 {
			req.Send(GetResponse{
				Empty: 1,
			})
			continue
		}
		hash := ExtractLeaves(nil, Unsolved)
		req.Send(GetResponse{
			Hash:   hash,
			Base64: "nothing yet",
		})
		Queue[hash] = time.Unix().Now()
		// mutex unlock.
	}
}

// Goroutine to process /captcha/solve requests.
func SolveMaster(master chan *SolveRequest) {
	for req := range master {
		// mutex lock for Queue, Solved and Unsolved here.
		hash, value := req.Hash, req.Value
		if _, ok := Queue[hash]; !ok {
			req.Send(SolveResponse{
				Status: "error, timed out",
			})
			continue
		}
		Solved[hash] = value
		delete(Queue, hash)
		// mutex unlock.
	}
}

// Goroutine for Queue filtering depending on time.
func Checker() {
	// ..
}
