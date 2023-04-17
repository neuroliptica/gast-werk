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
	http.HandleFunc("/api/add", ApiAddController)
	http.HandleFunc("/api/stats", ApiStatsController)

	go Checker()

	log.Fatal(http.ListenAndServe(address, nil))
}
