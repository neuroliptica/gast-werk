package server

// POST /api/del schema.
type (
	DelResponse struct {
		Status string    `json:"status"`
		Error  ErrorBody `json:"error"`
	}

	DelRequest struct {
		Token string `json:"token"`
		Hash  string `json"hash"`
	}
)

// Thread safe model to process /api/del requests.
func DelMaster(req DelRequest) DelResponse {
	if !ValidToken(req.Token) {
		return DelResponse{
			Error: MakeErrorBody("unathorized, invalid token"),
		}
	}
	return Locker(&DataSync, func() DelResponse {
		// status := Unsolved.Del(req.Hash)
		// if status == nil => failed
		return DelResponse{
			Status: "ok, deleted",
		}
	})
}
