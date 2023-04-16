package server

import (
	"net/http"
)

// /
func MainController(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/heartbeat", http.StatusFound)
}

// /api/add
func ApiAddController(w http.ResponseWriter, r *http.Request) {
	if !ApplicationJson(r) {
		JsonController(w, InvalidContentType())
		return
	}
	var addBody AddRequest
	err := ReadJsonBody(r, &addBody)
	if err != nil {
		JsonController(w, InternalError(err))
		return
	}
	if addBody.Token == "" || addBody.Base64 == "" {
		JsonController(w, InvalidData())
		return
	}
	JsonController(w, AddMaster(addBody))
}

// /api/auth
func ApiAuthController(w http.ResponseWriter, r *http.Request) {
	if !ApplicationJson(r) {
		JsonController(w, InvalidContentType())
		return
	}
	var authBody AuthRequest
	err := ReadJsonBody(r, &authBody)
	if err != nil {
		JsonController(w, InternalError(err))
		return
	}
	if authBody.Data.Nickname == "" || authBody.Data.Password == "" {
		JsonController(w, InvalidData())
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
		JsonController(w, InvalidContentType())
		return
	}
	var solveBody SolveRequest
	err := ReadJsonBody(r, &solveBody)
	if err != nil {
		JsonController(w, InternalError(err))
		return
	}
	if solveBody.Hash == "" || solveBody.Value == "" {
		JsonController(w, InvalidData())
	}
	JsonController(w, SolveMaster(solveBody))
}
