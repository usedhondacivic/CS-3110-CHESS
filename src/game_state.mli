(** [game_state] holds information about the board and the current state
    of the game. *)

(** [piece] represents possible chess pieces]. *)
type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

type castle_rights = {
  king_side : bool;
  queen_side : bool;
}

type time = int * int
(** [time] represents the current time of the game's chess clock. White
    first, then black. *)

(** [color] represents the color of a piece on a given square. *)
type color =
  | White
  | Black
  | NoPiece

val get_piece_str : piece * color -> string
(** [get_piece_str] Returns the string representation of a given piece.
    Example: (Pawn, White) -> "♟︎"*)

type board
(** [board] is the abstract representation of a chess board. *)

type board_coord = {
  rank : int;
  file : int;
}
(** [board_coord] represents a location on a chess board, using integers
    to represent ranks and mapping files A-H to 1-8 *)

type game_state = {
  board : board;
  white_taken : piece list;
  black_taken : piece list;
  time : time;
}
(** [game_state] stores a board and information about the game,
    including the what pieces have been taken and the current time on
    the chess clock *)

(** [result] is a flag to tell users if a board is legal or not*)
type result =
  | Legal
  | Illegal

val get_time : game_state -> time

val set_time : game_state -> time -> game_state

val set_square : board -> board_coord -> piece * color -> board

val get_square : board -> board_coord -> piece * color
(** [from_location] the piece that on the board at the given coordinate *)

val move_piece : board -> board_coord -> board_coord -> board
(** [move_piece] returns a board representing the result of a move *)

val swap_turn : board -> board
(** [swap_turn] returns a board with the opposite player to move *)

val get_castle_availability : board -> color -> castle_rights
(** [get_castle_availability] returns a boolean representing if a color
    is still able to castle*)

val set_castle_availability : board -> color -> castle_rights -> board
(** [set_castle_availability] returns a board with*)

val get_board_from_FEN : string -> board
(** [get_board_from_FEN] returns a board representing a FEN string. *)

val color_to_move : board -> color
(** [color_to_move] returns the color of the current player. *)
<<<<<<< HEAD
=======

val board_to_list : board -> string list
>>>>>>> a104f2b58ad33737e7bcf79d26ff3def924ddd08

val compare_game_board : board -> board -> bool