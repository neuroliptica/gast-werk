package server

import (
	"log"
	"net/http"
)

func Route(address string) {
	http.HandleFunc("/", IndexHandler)
	http.HandleFunc("/heartbeat", HeartbeatHandler)
	http.HandleFunc("/captcha/get", CaptchaGetHandler)
	http.HandleFunc("/captcha/solve", CaptchaSolveHandler)

	log.Fatal(http.ListenAndServe(address, nil))
}
