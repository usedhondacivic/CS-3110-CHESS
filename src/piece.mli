(** Representation of static chess piece data. This module represents
    the possible moves by each chess piece. *)

type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty  (** The abstarct type of values representing chess pieces *)

exception UnknownPiece of piece
(** Raised when an unknown piece is encountered. *)

type move
(** The type of a move of a chess piece. *)

val get_moves : piece -> move list
(** [get_moves] is the list of possible moves of piece[p]. Raises:
    [UnknownPiece piece] if piece [p] is not a valid piece name in
    [p]]. *)

(*TODO: decide if we want to return the move given a length and
  direction, or if we want to return a list of possible moves for that
  piece and then check if the input is valid. Leaning towards the
  first. *)
