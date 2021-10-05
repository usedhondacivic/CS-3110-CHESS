(** *)

(** [piece] represents possible chess pieces]. *)
type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

(** [time] represents the current time of the game's chess clock. White first, then black. *)
type time = int * int

(** [color] represents the color of a piece on a given square. *)
type color =
  | White
  | Black
  | Empty

(** [board] is the abstract representation of a chess board. *)
type board

(** [board_coord] represents a location on a chess board, using integers to represent ranks and mapping files A-H to 1-8 *)
type board_coord = {
  rank : int;
  file : int;
}

(** [game_state] stores a board and information about the game, including the what pieces have been taken and the current time on the chess clock *)
type game_state = {
  board : board;
  white_taken : piece list;
  black_taken : piece list;
  time : time;
}

(** [result] is a flag to tell users if a board is legal or not*)
type result =
  | Legal
  | Illegal

(** [game_over_check] checks a board to see if  *)
val game_over_check : board -> bool

val from_location : board -> board_coord -> (piece * color)

val get_king : board -> color -> board_coord

val get_castle_availability : board -> color -> bool

val get_board_from_FEN : string -> board

val color_to_move : color