package server

// GET /heartbeat response schema.
type Heartbeat struct {
	Status string `json:"status"`
}
