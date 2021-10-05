(** Representation of static chess piece data. This module represents
    the possible moves by each chess piece. *)

(** The abstarct type of values representing chess pieces *)
type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

exception UnknownPiece of piece
(** Raised when an unknown piece is encountered. *)

type move
(** The type of a move of a chess piece. *)

type brd
(** The type of the board. *)

type start
(** The type of the starting position of the board. *)

val get_moves : piece -> brd -> start -> move list
(** [get_moves] is the list of possible moves of piece[p] at starting
    position [start] on the board [brd]. Raises: [UnknownPiece piece] if
    piece [p] is not a valid piece name in [p]]. *)
