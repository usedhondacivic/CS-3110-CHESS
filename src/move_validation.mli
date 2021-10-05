type result
(**type result represents if a given board / move resulted in a legal
   move*)

val attempt_move : board -> board_coord -> board_coord -> board * result
(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.) Returns
   [board, Legal] if the move is allowed*)
