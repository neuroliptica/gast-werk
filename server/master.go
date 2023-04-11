package server

func GetMaster(master chan *GetRequest) {
	for req := range master {
		// ...
	}
}

func SolveMaster(master chan *SolveRequest) {
	for req := range master {
		// ...
	}
}

func Checker() {
	// ...
}
