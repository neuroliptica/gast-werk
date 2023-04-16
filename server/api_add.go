package server

// POST /api/add schema.
type (
	AddResponse struct {
		Hash  string    `json:"hash"`
		Error ErrorBody `json:"error"`
	}

	AddRequest struct {
		Base64 string `json:"img_base64"`
		Token  string `json:"token"`
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
		hash := MakeHash()
		Unsolved.Add(hash) // also add base64 image somewhere
		return AddResponse{
			Hash: hash,
		}
	})
}
