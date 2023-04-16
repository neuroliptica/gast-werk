package server

import "sync"

// TODO: should use a csrf tokens instead of this.

type UserData struct {
	Nickname string `json:"nickname"`
	Password string `json:"password"`
}

// Table datatype.
type Tokens struct {
	UserTokens  map[UserData]string
	ValidTokens map[string]any
}

// Tokens table constructor.
func MakeTokensTable() Tokens {
	tk := Tokens{
		UserTokens:  make(map[UserData]string),
		ValidTokens: make(map[string]any),
	}
	tk.AddToken(UserData{"123", "123"}, "test")
	return tk
}

// Add token to all tables.
func (t *Tokens) AddToken(data UserData, token string) {
	t.UserTokens[data] = token
	t.ValidTokens[token] = struct{}{}
}

// Delete token from all tables.
func (t *Tokens) DelToken(token string) {
	delete(t.ValidTokens, token)
	for key, value := range t.UserTokens {
		if value == token {
			delete(t.UserTokens, key)
			break
		}
	}
}

// Check if token exists.
func (t *Tokens) ValidToken(token string) bool {
	_, ok := t.ValidTokens[token]
	return ok
}

// Check if user exists.
func (t *Tokens) ValidUser(user UserData) bool {
	_, ok := t.UserTokens[user]
	return ok
}

var (
	LocalTable = MakeTokensTable()
	TokensSync sync.Mutex
)

// POST /api/auth schema.
type (
	AuthResponse struct {
		Token string    `json:"token"`
		Error ErrorBody `json:"error"`
	}

	AuthRequest struct {
		Data UserData `json:"data"`
	}
)

// Set auth token for nickname-password pair.
func (req AuthRequest) SetToken(tokens *Tokens) string {
	token := "token"
	tokens.AddToken(req.Data, token)
	return token
}

// Thread safe model to process /api/auth requests.
func AuthMaster(req AuthRequest) AuthResponse {
	return Locker(&TokensSync, func() AuthResponse {
		if !LocalTable.ValidUser(req.Data) {
			return AuthResponse{
				Error: ErrorBody{
					Failed: true,
					Reason: "user does not exist",
				},
			}
		}
		token := req.SetToken(&LocalTable)
		return AuthResponse{
			Token: token,
		}
	})
}
