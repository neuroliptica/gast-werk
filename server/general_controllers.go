package server

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
)

// Generic constraint for json payload.
type Payload interface {
	// GetResponse | SolveResponse | Heartbeat | AuthResponse
}

// Response json data.
func JsonController[P Payload](w http.ResponseWriter, payload P) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(payload)
}

func ReadJsonBody[P Payload](r *http.Request, payload *P) error {
	cont, err := ioutil.ReadAll(r.Body)
	if err != nil {
		return err
	}
	json.Unmarshal(cont, payload)
	return nil
}

// Get Content-Type header.
func ContentType(r *http.Request) string {
	return r.Header.Get("Content-Type")
}

// Check if Content-Type header is application/json.
func ApplicationJson(r *http.Request) bool {
	return ContentType(r) == "application/json"
}
