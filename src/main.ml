(*This is not actually the main file, I was just testing out types in here*)

let _ = print_endline "Hello world!"


type piece =  Pawn | Rook | Bishop | King | Queen | Knight | Empty

type time = int * int

type color = White | Black | Empty

type board = (piece * color) list list

type board_coord = {
  rank : int;
  file : int
}

type game_state = {
  board : board;
  current_turn: color;
  white_taken : piece list;
  black_taken : piece list;
  time : time
}

type result = Legal | Illegal

module type Test = sig
  val attempt_move : board -> board_coord -> board_coord -> board * result

  val get_moves : board -> board_coord -> board_coord list

  val update_board : board
end