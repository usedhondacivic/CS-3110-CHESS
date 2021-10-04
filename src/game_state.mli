type time = int * int

type color = White | Black | Empty

type board = (Piece.piece * color) list list

type board_coord = {
  rank : int;
  file : int
}

type game_state = {
  board : board;
  current_turn: color;
  white_taken : Piece.piece list;
  black_taken : Piece.piece list;
  time : time;
}

type result = Legal | Illegal

val game_over_check : board -> bool