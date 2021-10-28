exception UnknownPiece of Game_state.piece

(* [type move] represents a possible move [(x;y)]]. *)
type move = int * int

(*[start] is the starting position of the piece as a tuple [(x*y)] *)
type start = int * int

(*TODO: add starting coordinate from gameplay *)

(* [type brd] represents a list of move [(x;y)]]. *)
type brd = move list

type dir =
  | Up
  | Down
  | Left
  | Right

let mult_of_dir d =
  match d with Up -> 1 | Down -> -1 | Left -> 1 | Right -> -1

(* [type direction] represents possible directions of a piece]. *)
type direction =
  | Vert
  | Horiz
  | DiagQOne
  | DiagQTwo
  | DiagQThree
  | DiagQFour
  | LeftDiag
  | RightDiag
  | LShapeA
  | LShapeB
  | LShapeC
  | LShapeD

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
  xPos : int list;
  xNeg : int list;
  yPos : int list;
  yNeg : int list;
}

type range = {
  xp : int;
  xn: int;
  yp : int;
  yn : int;
}

(** [(--)] is a list from i to j where i > j *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

(**[let_range] is the maximum lengh a piece can move in the x and y
   directions given a start position [start]. Example:
   [let_range (3;4) = {xPos = 5; xNeg = -3; yPos = 4; yNeg = -4}]*)
let range s =
  {
    xp = 8 - fst s;
    xn = (fst s) -1;
    yp = 8 - fst s;
    yn = (fst s) -1;
  
  }
  
  let qI (s: int * int) = ( -- ) 0 (min (range s).xp (range s).yp )
  let qII (s: int * int) = ( -- ) 0 (min (range s).xn (range s).yp )
  let qIII (s: int * int) = ( -- ) 0 (min (range s).xn (range s).yn)
  let qIIII (s: int * int) = ( -- ) 0 (min (range s).xp (range s).yn)


(*let len_range start = { xPos = 8 - fst start; xNeg = 0 - fst start;
  yPos = 8 - snd start; yNeg = 0 - snd start; } *)

(**[make_dir_list] is the list of all positions a piece can move in the
   x and y directions given a start position [start]. Example:
   [let make_dir_list (3;4) = {xPos = \[1,2,3,4,5\]; xNeg = \[-1,-2\]; yPos = \[1,2,3,4\]; yNeg = \[-1,-2,-3\]}]*)
let make_dir_list start =
  {
    xPos = 0 -- (8 - fst start);
    xNeg = 1 - fst start -- 1;
    yPos = 0 -- (8 - snd start);
    yNeg = 1 - snd start -- 1;
  }

let xpos_list (start : int * int) = 0 -- (8 - fst start)

let xneg_list (start : int * int) = 1 - fst start -- 1

let ypos_list (start : int * int) = 0 -- (8 - snd start)

let yneg_list (start : int * int) = 1 - snd start -- 1

(**[move_piece] is the position to which a piece can move given a start
   position on the board [start] , a direction to move [dir] and a
   length to move [len]*)
let move_piece start len dir =
  match dir with
  | Vert -> (fst start, snd start + len)
  | Horiz -> (fst start + len, snd start)
  | DiagQOne -> (fst start + len, snd start + len)
  | DiagQTwo -> (fst start - len, snd start + len)
  | DiagQThree -> (fst start - len, snd start - len)
  | DiagQFour -> (fst start + len, snd start - len)
  | LeftDiag -> (fst start - len, snd start + len)
  | RightDiag -> (fst start + len, snd start + len)
  | LShapeA -> (fst start + (1*len), snd start + (2*len))
  | LShapeB -> (fst start + (1*len), snd start + (-2*len))
  | LShapeC -> (fst start + (2*len), snd start + (1*len))
  | LShapeD -> (fst start + (-2*len), snd start + (1*len))
(*||(fst start + 1, snd start + 2) *)

(**Quadrant I and III are odd, quadrant II and IIII are even.*)

(**Quadrant I has positive x and positive y*)
let min_quadI s = min
  (List.length (xpos_list s)) (List.length (ypos_list s))

  (**Quadrant II has negative x and positive y*)
let min_quadII s = min
(List.length (xneg_list s)) (List.length (ypos_list s))

(**Quadrant III has negative x and negative y*)
let min_quadIII s = min
  (List.length (xneg_list s)) (List.length (yneg_list s))

  (**Quadrant IIII has positive x and negative y*)
let min_quadIIII s = min
(List.length (xpos_list s)) (List.length (yneg_list s))

(**[vert_moves] is the list of vertical positions to which a piece can
   move starting from position [start] *)
let rec vert_moves start poslist =
  match poslist with
  | [] -> []
  | h :: t -> move_piece start h Vert :: vert_moves start t

(**[horiz_moves] is the list of horizontal positions to which a piece
   can move starting from position [start] *)
let rec horiz_moves start poslist =
  match poslist with
  | [] -> []
  | h :: t -> move_piece start h Horiz :: horiz_moves start t

let move_lshape (s : int * int) list  = 
  let lista = if ((range s).xp > 1 && (range s).yp > 0)  then (move_piece s 1 LShapeA) :: list else list in (**(4,4) to (5,6)*)
  let listb = if ((range s).xn > 1 && (range s).yn > 0)  then (move_piece s (-1) LShapeA) :: lista else lista in (**(4,4) to (3,2)*)
  let listc = if (range s).xp > 0 && (range s).yn > 1 then move_piece s (1) LShapeB :: listb else listb in (**(4,4) to (5,2)*)
  let listd = if (range s).xp > 0 && (range s).yp > 1 then move_piece s (-1) LShapeB :: listc else listc in (**(4,4) to (3,6)*)
  let liste = if (range s).xp > 1 && (range s).yp > 0 then move_piece s (1) LShapeC :: listd else listd in (**(4,4) to (6,5)*)
  let listf = if (range s).xn > 1 && (range s).yn > 0 then move_piece s (-1) LShapeC :: liste else liste in (**(4,4) to (2,3)*)
  let listg = if (range s).xp > 1 && (range s).yp > 0 then move_piece s (1) LShapeD :: listf else listf in(**(4,4) to (2,5)*)
  let listh = if (range s).xn > 1 && (range s).yn > 0 then  move_piece s (-1) LShapeD :: listg else listg  in listh (**(4,4) to (6,3)*)


let rec diag_move start poslist dir=
  match poslist with
  | [] -> []
  | h :: t ->
      move_piece start h dir
      :: diag_move start t dir

  let diag_moves s = diag_move s (qI s) DiagQOne @ diag_move s (qII s) DiagQTwo @ diag_move s (qIII s) DiagQThree @ diag_move s (qIIII s) DiagQFour

(*let rec diag_moves start poslist =
  match poslist with
  | [] -> []
  | h :: t ->
      move_piece start h DiagQOne :: move_piece start h DiagQTwo  :: move_piece start h DiagQThree :: move_piece start h DiagQFour
      :: diag_moves start t *)

let get_moves p s =
  match p with
  | Game_state.Pawn ->
      [
        move_piece s 1 Vert;
        move_piece s 2 Vert;
        move_piece s 1 LeftDiag;
        move_piece s 1 RightDiag;
      ]
  | Game_state.Rook -> (**Moves vertically and horizontally in all lengths*)
    horiz_moves s (xpos_list s)
  @ horiz_moves s (xneg_list s)
  @ vert_moves s (ypos_list s)
  @ vert_moves s (yneg_list s)
  | Game_state.Bishop -> (**Moves diagonally in all lengths*) diag_moves s
  | Game_state.King -> (**Moves vertically, horizontally and diagonally by 1*)
      [
        move_piece s 1 Vert;
        move_piece s (-1) Vert;
        move_piece s 1 Horiz;
        move_piece s (-1) Horiz;
        move_piece s 1 LeftDiag;
        move_piece s (-1) LeftDiag;
        move_piece s 1 RightDiag;
        move_piece s (-1) RightDiag;
      ]
  | Game_state.Queen -> (**Moves vertically, horizontally, diagoanlly in all lengths*)
      horiz_moves s (xpos_list s)
      @ horiz_moves s (xneg_list s)
      @ vert_moves s (ypos_list s)
      @ vert_moves s (yneg_list s)
      @diag_moves s
  | Game_state.Knight -> move_lshape s [] (**Moves in an LShape in all directions*)
  | Game_state.Empty -> []

(*evaluates to a position on the board givem a move length [len] and
  direction [dir] *)
let get_move len dir = failwith "get_move not implemented"

let pawnmoves start = vert_moves start []
