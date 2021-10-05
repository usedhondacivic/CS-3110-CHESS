(* [type piece] represents possible chess pieces]. *)
type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

exception UnknownPiece of piece

(* [type direction] represents possible directions of a piece]. *)
type direction =
  | Vertical
  | Horizontal
  | Diagonal
  | LShape

(* [type move] represents a possible move]. *)
type move = {
  length : int;
  directions : direction;
}

(* [type moves] represents possible moves of a piece]. *)
type moves = {
  p : piece;
  validmove : move list;
}

type brd = Game_state.board

(*[start] retruns the starting position of the piece as a tuple
  [(x*y)] *)
type start = Gameplay.move

let move_vert len dir = failwith "move_vert not implemented"

let move_horiz len dir = failwith "move_horiz not implemented"

let move_lshape len dir = failwith "move_lshape not implemented"

let move_diag len dir = failwith "move_diag not implemented"

let get_moves p brd = failwith "get_moves not implemented"

(*evaluates to a position on the board givem a move length [len] and
  direction [dir] *)
let get_move len dir = failwith "get_move not implemented"
