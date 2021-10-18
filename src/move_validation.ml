open Game_state

type result =
  | Legal
  | Illegal

let friendly_fire aboard start_coard finish_coard =
  aboard.(start_coard.Game_state.rank).(start_coard.Game_state.file)
  == aboard.(finish_coard.Game_state.rank).(finish_coard.Game_state.file)

(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.)
   Returns[board, Legal] if the move is allowed*)
let attempt_move board start finish = failwith "not implemented"

let attempt_move_no_checks board start finish =
  let the_piece = Game_state.get_square board start in
  let take_piece = Game_state.set_square board start (Empty, White) in
  Game_state.set_square take_piece finish the_piece
