package server

import "fmt"

// General Error response types.
type (
	ErrorBody struct {
		Failed bool   `json:"failed"`
		Reason string `json:"reason"`
	}

	ErrorResponse struct {
		Error ErrorBody `json:"error"`
	}
)

// Instance ErrorBody struct with provided message.
func MakeErrorBody(reason string) ErrorBody {
	return ErrorBody{
		Failed: true,
		Reason: reason,
	}
}

// Instance ErrorResponse struct with provided message.
func MakeErrorResponse(reason string) ErrorResponse {
	return ErrorResponse{
		Error: MakeErrorBody(reason),
	}
}

// If "Content-Type" header from request is non json.
func InvalidContentType() ErrorResponse {
	return MakeErrorResponse("content-type should be application/json")
}

// Bypass any error interface instance.
func InternalError(err error) ErrorResponse {
	return MakeErrorResponse(fmt.Sprintf("%v", err))
}

// If data in request body is invalid.
func InvalidData() ErrorResponse {
	return MakeErrorResponse("invalid data provided")
}
