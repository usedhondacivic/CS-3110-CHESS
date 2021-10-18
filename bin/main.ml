(**
Code to start gameplay will go here
*)

(*print_endline "bin/main.ml has yet to be implemented."*)

open ANSITerminal

open Chess


let start_board = Game_state.get_board_from_FEN  "8/5k2/3p4/1p1Pp2p/pP2Pp1P/P4P1K/8/8"


let _ = Ui.draw_empty start_board