package server

// GET /captcha/get endpoint.
type (
	GetResponse struct {
		Base64 string
		Hash   string
		Empty  int32
	}

	GetRequest struct {
		Chan chan GetResponse // Buffered.
	}
)

// /captcha/get request constructor.
func CreateGetRequest() *GetRequest {
	return &GetRequest{
		Chan: make(chan GetResponse, 1),
	}
}

// Send /captcha/get response.
func (req *GetRequest) Send(resp GetResponse) {
	req.Chan <- resp
}

// Accept /captcha/get response.
func (req *GetRequest) Accept() GetResponse {
	resp := <-req.Chan
	return resp
}

// POST /captcha/solve endpoint.
type (
	SolveResponse struct {
		Status string
	}

	SolveRequest struct {
		Hash  string
		Value string
		Chan  chan SolveResponse // Buffered.
	}
)

// /captcha/solve request constructor.
func CreateSolveRequest(hash, value string) *SolveRequest {
	return &SolveRequest{
		Hash:  hash,
		Value: value,
		Chan:  make(chan SolveResponse, 1),
	}
}

// Send /captcha/solve response.
func (req *SolveRequest) Send(resp SolveResponse) {
	req.Chan <- resp
}

// Accept /captcha/solve response.
func (req *SolveRequest) Accept() SolveResponse {
	resp := <-req.Chan
	return resp
}
