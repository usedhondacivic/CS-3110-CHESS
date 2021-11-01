(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.) Returns
   [board, Legal] if the move is allowed*)
val attempt_move :
  Game_state.board ->
  Game_state.board_coord ->
  Game_state.board_coord ->
  Game_state.board * Game_state.result * Game_state.piece

