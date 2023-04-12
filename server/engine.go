package server

import (
	"log"
	"net/http"
)

func Run(address string) {

	// GET /captcha/get
	// Response:

	// {
	//		"img_base64": "base64val",
	//		"hash": "123qsdqsdq"
	// }

	// POST /captcha/solve
	// Request:

	// {
	//		"hash": "1231dsad",
	//		"value": "123"
	// }

	// Response:

	// {
	//		"status": "ok" | "failed"
	// }

	// /captcha/get

	http.HandleFunc("/", IndexHandler)
	http.HandleFunc("/captcha/get", CaptchaGetHandler)
	http.HandleFunc("/captcha/solve", CaptchaSolveHandler)

	log.Fatal(http.ListenAndServe(address, nil))
}
