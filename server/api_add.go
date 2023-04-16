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
	valid := Locker(&TokensSync, func() bool {
		return LocalTable.ValidToken(req.Token)
	})
	if !valid {
		return AddResponse{
			Error: ErrorBody{
				Failed: true,
				Reason: "unauthorized, invalid token",
			},
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
