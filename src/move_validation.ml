(**type result represents if a given board / move resulted in a legal
   move*)
type result =
  | Legal
  | Illegal

(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.) Returns
   [board, Legal] if the move is allowed*)
let attempt_move board board_coord board_coord =
  failwith "attempt_move has not been implemented"
