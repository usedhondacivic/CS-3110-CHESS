exception UnknownPiece of Game_state.piece

(* [type move] represents a possible move [(x;y)]]. *)
type move = int * int

(*[start] is the starting position of the piece as a tuple [(x*y)] *)
type start = int * int

(*TODO: add starting coordinate from gameplay *)

(* [type brd] represents a list of move [(x;y)]]. *)
type brd = move list

(* [type direction] represents possible directions of a piece]. *)
type direction =
  | Vert
  | Horiz
  | Diag
  | LShape

(* [type piece_move] represents a possible move]. *)
type piece_move = {
  length : int;
  directions : direction;
}

(* [type moves] represents possible moves of a piece]. *)
type moves = {
  p : Game_state.piece;
  validmove : move list;
}


type pos_move_dir = {
  xPos : int;
  xNeg : int;
  yPos : int;
  yNeg : int;
}
(**[let_range] is the maximum lengh a piece can move in the x and y directions 
given a start position [start]. 
Example: [let_range (3;4) = {xPos = 3; xNeg = 5; yPos = 4; yNeg = 4}]*)
let len_range start =
  {
    xPos = fst start;
    xNeg = 8 - fst start;
    yPos = snd start;
    yNeg = 8 - snd start;
  }

(** [(--)] is a list from i to j. *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

(**[move_piece] is the position to which a piece can move given a start
   position on the board [start] , a direction to move [dir] and a
   length to move [len]*)
let move_piece start len dir =
  match dir with
  | Vert -> (fst start, snd start + len)
  | Horiz -> (fst start + len, snd start)
  | Diag -> (fst start + len, snd start + len)
  | LShape -> (fst start + 2, snd start + 1)
(*||(fst start + 1, snd start + 2) *)

let rec vert_moves start lenlist =
  match lenlist with
  | [] -> []
  | h :: t -> [ move_piece start h Vert ] :: vert_moves start t

let move_horiz len dir = failwith "move_horiz not implemented"

let move_lshape len dir = failwith "move_lshape not implemented"

let move_diag len dir = failwith "move_diag not implemented"

let get_moves p brd = failwith "get_moves not implemented"

(*evaluates to a position on the board givem a move length [len] and
  direction [dir] *)
let get_move len dir = failwith "get_move not implemented"
