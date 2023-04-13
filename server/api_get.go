package server

import "time"

// GET /api/get schema.
type (
	GetResponse struct {
		Base64 string `json:"img_base64"`
		Hash   string `json:"hash"`
		Empty  int8   `json:"empty"`
	}

	GetRequest struct{}
)

// /api/get request constructor.
func CreateGetRequest() GetRequest {
	return GetRequest{}
}

// Thread safe model to process /api/get requests.
func GetMaster(req GetRequest) GetResponse {
	return Locker[GetResponse](&DataSync, func() GetResponse {
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
	})
}
