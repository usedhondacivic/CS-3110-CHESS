(**
Code to start gameplay will go here
*)

(*print_endline "bin/main.ml has yet to be implemented."*)

open ANSITerminal

open Chess

let start_board = Game_state.get_board_from_FEN  "8/5k2/3p4/1p1Pp2p/pP2Pp1P/P4P1K/8/8"

let start_state : Game_state.game_state = {
  board = start_board;
  white_taken = [];
  black_taken = [];
  time = (300, 300);
} 

let _ = Ui.update_display start_state
