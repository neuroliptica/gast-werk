package server

import (
	"fmt"
	"net/http"
)

// /
func MainController(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/heartbeat", http.StatusFound)
}

// /api/auth
func ApiAuthController(w http.ResponseWriter, r *http.Request) {
	if !ApplicationJson(r) {
		JsonController(w, AuthResponse{
			Error: ErrorBody{
				Failed: true,
				Reason: "content-type should be application/json",
			},
		})
		return
	}
	var authBody AuthRequest
	err := ReadJsonBody(r, &authBody)
	if err != nil {
		JsonController(w, AuthResponse{
			Error: ErrorBody{
				Failed: true,
				Reason: fmt.Sprintf("%v", err),
			},
		})
		return
	}
	JsonController(w, AuthMaster(authBody))
}

// /api/get
func ApiGetController(w http.ResponseWriter, r *http.Request) {
	resp := GetMaster(CreateGetRequest())
	JsonController(w, resp)
}

// /heartbeat
func HeartbeatController(w http.ResponseWriter, r *http.Request) {
	JsonController(w, Heartbeat{
		Status: "alive",
	})
}

// /api/solve
func ApiSolveController(w http.ResponseWriter, r *http.Request) {
	if !ApplicationJson(r) {
		JsonController(w, SolveResponse{
			Status: "content-type should be application/json",
		})
		return
	}
	var solveBody SolveRequest
	err := ReadJsonBody(r, &solveBody)
	if err != nil {
		JsonController(w, SolveResponse{
			Status: fmt.Sprintf("error, %v", err),
		})
		return
	}
	if solveBody.Hash == "" || solveBody.Value == "" {
		JsonController(w, SolveResponse{
			Status: "error, invalid data provided.",
		})
	}
	JsonController(w, SolveMaster(solveBody))
}
