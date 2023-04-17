package server

// GET /api/stats
type (
	StatsResponse struct {
		Solved   []string  `json:"solved"`
		Unsolved []string  `json:"unsolved"`
		Queued   []string  `json:"queued"`
		Error    ErrorBody `json:"error"`
	}

	StatsRequest struct {
		Token string `json:"token"`
	}
)

// Model to process /api/stats requests.
func StatsMaster(req StatsRequest) StatsResponse {
	if !ValidToken(req.Token) {
		return StatsResponse{
			Error: MakeErrorBody("unauthorized, invalid token"),
		}
	}
	// Only reading, maybe shouldn't block.
	return Locker(&DataSync, func() StatsResponse {
		solved := make([]string, 0)
		for key := range Solved {
			solved = append(solved, key)
		}
		queue := make([]string, 0)
		for key := range Queue {
			queue = append(queue, key)
		}
		unsolved := make([]string, 0)

		var dfs func(*Tree)
		dfs = func(node *Tree) {
			if node == nil {
				return
			}
			unsolved = append(unsolved, node.Hash)
			dfs(node.Right)
			dfs(node.Left)
		}
		dfs(Unsolved)

		return StatsResponse{
			Solved:   solved,
			Unsolved: unsolved,
			Queued:   queue,
		}
	})
}
