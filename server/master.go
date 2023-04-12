// master.go: goroutines for input processing.
// many-readers-one-writer

package server

import "time"

var (
	Unsolved = &Tree{Hash: "root"}
	Queue    map[string]time.Time
	Solved   map[string]string
)

// GET /captcha/get endpoint.
type (
	GetResponse struct {
		Base64 string `json:"img_base64"`
		Hash   string `json:"hash"`
		Empty  int32  `json:"empty"`
	}

	GetRequest struct{}
)

// POST /captcha/solve endpoint.
type (
	SolveResponse struct {
		Status string `json:"status"`
	}

	SolveRequest struct {
		Hash  string
		Value string
	}
)

// /captcha/get request constructor.
func CreateGetRequest() GetRequest {
	return GetRequest{}
}

// /captcha/solve request constructor.
func CreateSolveRequest(hash, value string) SolveRequest {
	return SolveRequest{
		Hash:  hash,
		Value: value,
	}
}

// Process /captcha/get requests.
func GetMaster(req GetRequest) GetResponse {
	// mutex lock for Unsolved, Queue and Solved here.
	if Unsolved.Depth() == 1 {
		return GetResponse{
			Empty: 1,
		}
	}
	hash := ExtractLeaves(nil, Unsolved)
	Queue[hash] = time.Now()
	return GetResponse{
		Hash:   hash,
		Base64: "nothing yet",
	}
	// mutex unlock.
}

// Goroutine to process /captcha/solve requests.
func SolveMaster(req SolveRequest) SolveResponse {
	// mutex lock for Queue, Solved and Unsolved here.
	hash, value := req.Hash, req.Value
	if _, ok := Queue[hash]; !ok {
		return SolveResponse{
			Status: "error, timed out",
		}
	}
	Solved[hash] = value
	delete(Queue, hash)

	return SolveResponse{
		Status: "ok",
	}
	// mutex unlock.
}

// Goroutine for Queue filtering depending on time.
func Checker() {
	// ..
}
