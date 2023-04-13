package server

// POST /api/solve schema.
type (
	SolveResponse struct {
		Status string `json:"status"`
		Ok     int8   `json:"ok"`
	}

	SolveRequest struct {
		Hash  string
		Value string
	}
)

// /api/solve request constructor.
func CreateSolveRequest(hash, value string) SolveRequest {
	return SolveRequest{
		Hash:  hash,
		Value: value,
	}
}

// Model to process /api/solve requests.
func SolveMaster(req SolveRequest) SolveResponse {
	return Locker(&DataSync, func() SolveResponse {
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
			Ok:     1,
		}
	})
}
