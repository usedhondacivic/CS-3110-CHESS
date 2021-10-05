type time = int * int

type color =
  | White
  | Black
  | Empty

type board

type board_coord = {
  rank : int;
  file : int;
}

type game_state = {
  board : board;
  current_turn : color;
  white_taken : Piece.piece list;
  black_taken : Piece.piece list;
  time : time;
}

type result =
  | Legal
  | Illegal

val game_over_check : board -> bool

val from_location : board -> board_coord -> (Piece.piece * color)

val get_king : board -> color -> board_coord

val get_castle_availability : board -> color -> bool

val get_board_from_FEN : string -> board

val color_to_move : color