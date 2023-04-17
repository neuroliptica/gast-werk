package server

// POST /api/add schema.
type (
	AddResponse struct {
		Hashes []string  `json:"hash"`
		Error  ErrorBody `json:"error"`
	}

	AddRequest struct {
		Data  []string `json:"data"`
		Token string   `json:"token"`
	}
)

// TODO: normal hash function.
func MakeHash() string { return "ololo" }

// Thread safe model to process /api/add rquests.
func AddMaster(req AddRequest) AddResponse {
	if !ValidToken(req.Token) {
		return AddResponse{
			Error: MakeErrorBody("unathorized, invalid token"),
		}
	}
	return Locker(&DataSync, func() AddResponse {
		hashes := make([]string, 0)
		for i := range req.Data {
			hash := MakeHash()
			hashes = append(hashes, hash)
			Total[hash] = req.Data[i]
			Unsolved.Add(hash)
		}
		return AddResponse{
			Hashes: hashes,
		}
	})
}
