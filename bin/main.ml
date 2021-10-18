(**
Code to start gameplay will go here
*)

(*print_endline "bin/main.ml has yet to be implemented."*)

open ANSITerminal

open Chess

let start_board = Game_state.get_board_from_FEN  "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"

let start_state : Game_state.game_state = {
  board = start_board;
  white_taken = [];
  black_taken = [];
  time = (300, 300);
}

let _ = Ui.show_start

let _ = Ui.update_display start_state