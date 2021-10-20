(**
Code to start gameplay will go here
*)

open Chess

let start_board = Game_state.get_board_from_FEN  "rnbq4/pppppppp/8/8/8/8/PPPPPPPP/4KBNR w KQkq - 0 1"

let start_state : Game_state.game_state = {
  board = start_board;
  white_taken = [];
  black_taken = [];
  time = (300, 300);
}

let _ = Ui.show_start

let rec gameplay_loop (state : Game_state.game_state) =
  let _ = Ui.update_display state in
  let move = (Gameplay.take_move "") in
  let new_board = Game_state.move_piece state.board move.start move.next in
  let new_board = Game_state.swap_turn new_board in
  let new_state = {state with board = new_board} in
  gameplay_loop new_state

let _ = gameplay_loop start_state