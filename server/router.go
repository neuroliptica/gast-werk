package server

import (
	"log"
	"net/http"
)

func Route(address string) {
	http.HandleFunc("/", MainController)
	http.HandleFunc("/heartbeat", HeartbeatController)
	http.HandleFunc("/api/get", ApiGetController)
	http.HandleFunc("/api/solve", ApiSolveController)
	http.HandleFunc("/api/auth", ApiAuthController)

	log.Fatal(http.ListenAndServe(address, nil))
}
