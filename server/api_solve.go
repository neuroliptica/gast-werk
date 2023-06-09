package server

// POST /api/solve schema.
type (
	SolveResponse struct {
		Status string    `json:"status"`
		Error  ErrorBody `json:"error"`
	}

	SolveRequest struct {
		Hash  string
		Value string
	}
)

// Model to process /api/solve requests.
func SolveMaster(req SolveRequest) SolveResponse {
	return Locker(&DataSync, func() SolveResponse {
		hash, value := req.Hash, req.Value
		if _, ok := Queue[hash]; !ok {
			return SolveResponse{
				Status: "failed",
				Error:  MakeErrorBody("hash is not in queue, timed out"),
			}
		}
		Solved[hash] = value
		delete(Queue, hash)
		return SolveResponse{
			Status: "ok, solved",
		}
	})
}
