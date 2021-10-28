(**
Code to start gameplay will go here
*)

open Chess

let start_board = Game_state.get_board_from_FEN  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let start_state : Game_state.game_state = {
  board = start_board;
  white_taken = [];
  black_taken = [];
  time = (300, 300);
}

let _ = Ui.show_start

let turn_swap result = match result with
| (x, Game_state.Legal) -> Game_state.swap_turn x
| (x, Game_state.Illegal) -> (print_endline "Illegal move, please enter new move."; x)


let rec gameplay_loop (state : Game_state.game_state) =
  let _ = Ui.update_display state in
  let move = (Gameplay.take_move "") in
  let move_result = Move_validation.attempt_move state.board move.start move.next in
  let new_board = turn_swap move_result in
  let new_state = {state with board = new_board} in
  gameplay_loop new_state

let _ = gameplay_loop start_state