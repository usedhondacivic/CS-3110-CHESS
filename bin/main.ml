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

let _ = Ui.update_display start_state

let rec gameplay_loop () =
  let _ = (Gameplay.take_move "") in 
  let _ = Ui.update_display start_state in
  gameplay_loop ()

let _ = gameplay_loop ()