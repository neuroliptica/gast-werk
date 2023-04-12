package server

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

// Generic type for json payload.
type Payload interface {
	GetResponse | SolveResponse
}

// Index page default handler.
func IndexHandler(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/captcha/get", http.StatusFound)
}

// Response json data.
func JsonHandler[P Payload](w http.ResponseWriter, payload P) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(payload)
}

func CaptchaGetHandler(w http.ResponseWriter, r *http.Request) {
	resp := GetMaster(CreateGetRequest())
	JsonHandler(w, resp)
}

func CaptchaSolveHandler(w http.ResponseWriter, r *http.Request) {
	header := r.Header.Get("Content-Type")
	if header != "application/json" {
		JsonHandler(w, SolveResponse{
			Status: "error, invalid body provided, only json.",
		})
		return
	}
	cont, err := ioutil.ReadAll(r.Body)
	if err != nil {
		JsonHandler(w, SolveResponse{
			Status: fmt.Sprintf("error, %v", err),
		})
		return
	}
	var reqJson SolveRequest
	json.Unmarshal(cont, &reqJson)
	if reqJson.Hash == "" || reqJson.Value == "" {
		JsonHandler(w, SolveResponse{
			Status: "error, invalid data provided.",
		})
	}
	JsonHandler(w, SolveMaster(reqJson))
}
