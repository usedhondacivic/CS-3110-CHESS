open Game_state
open Piece
type result =
  | Legal
  | Illegal

(**[double_first a] returns the first element of a tuple *)
let double_first a = match a with x, _ -> x

(**[double_second b] returns the second element of a tuple*)
let double_second a = match a with _, x -> x

(**[color_eq a b] returns true if two colors are the same *)
let color_eq a b =
  (a = Black && b = Black) || (a = White && b = Black)

(**[same_first a b] returns true if the first element of two tuples are the same*)
let same_first a b = double_first a = double_first b

(**[same_second a b] returns true if the second element of two tuples are the same*)
let same_second a b = double_second a = double_second b

(**[equal_tuple a b] returns true if the both elements of two tuples are the same*)
let equal_tuple a b =
  same_first a b && same_second a b

let rec pieces_in_between_same_rank_helper board s f =
  match (s.file,f.file) with
  |(a,b) when a>b -> get_square board s :: pieces_in_between_same_rank_helper board {rank = s.rank; file = s.file-1} f
  |(a,b) when a<b -> get_square board s :: pieces_in_between_same_rank_helper board {rank = s.rank; file = s.file+1} f
  |(a,b) when a = b -> []
  |(_,_) -> failwith("pieces_in_between_same_rank failure")

let pieces_in_between_same_rank board start finish= List.tl (pieces_in_between_same_rank_helper board start finish)


let rec pieces_in_between_same_file_helper board s f =
  match (s.rank,f.rank) with
  |(a,b) when a>b -> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank -1; file = s.file} f
  |(a,b) when a<b -> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank + 1; file = s.file} f
  |(a,b) when a = b -> []
  |(_,_) -> failwith("pieces_in_between_same_file failure")

let pieces_in_between_same_file board start finish= List.tl (pieces_in_between_same_file_helper board start finish)

let rec pieces_in_between_diagonal_helper board s f =
  match (s.rank,f.rank,s.file,f.file) with
  |(a,b,c,d) when a>b && c>d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank -1; file = s.file -1} f
  |(a,b,c,d) when a<b && c<d -> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank + 1; file = s.file + 1} f
  |(a,b,c,d) when a>b && c<d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank -1; file = s.file +1} f
  |(a,b,c,d) when a<b && c>d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank +1; file = s.file -1} f
  |(a,b,c,d) when a = b && c =d -> []
  |(_,_,_,_) -> failwith("pieces_in_between_diagonal failure")

let pieces_in_between_diagonal board start finish = List.tl (pieces_in_between_diagonal_helper board start finish)


(**[pieces_in_between board start finish] returns the pieces in between start and finish, including start, not including finish; if a knight is chosen as start, the empty list is returned, otherwise if a pieces is not on the same rank, file, or diagonal, a failure happens*)
let pieces_in_between board start finish =
  (*if start is a knight*)
  if double_first (get_square board start) = Knight then []
  (*if rank is same*)
  else if start.rank = finish.rank then pieces_in_between_same_rank board start finish
  (*if file is same*)
  else if start.file = finish.file then pieces_in_between_same_file board start finish
  (**diagonal*)
  else  pieces_in_between_diagonal board start finish

(**[clear_path pieces] returns true if pieces contains all Empty pieces of is the empty string*)
let rec clear_path pieces = 
  match pieces with
  |[] -> true
  |(a,b)::t when a = Empty -> true && clear_path t
  |(a,b)::t when a != Empty -> false
  |_ -> failwith("clear_path failure")

(**[friendly_fire board start finish] returns true if start and finish hold pieces of the same color*)
let friendly_fire board start finish =
  let the_piece = Game_state.get_square board start in
  let target_piece = Game_state.get_square board finish in
  color_eq (double_second the_piece) (double_second target_piece)


let piece_legality board start finish = 
  (*et moves_list = get_moves (double_first (get_square board start)) (start.rank,start.finish) in *)
  let possible_moves = Piece.get_moves (double_first (get_square board start))  (start.rank,start.file) in
  List.exists (equal_tuple (finish.rank,finish.file)) possible_moves

let move_is_legal board start finish = (piece_legality board start finish) && (not (friendly_fire board start finish)) && clear_path (pieces_in_between board start finish)

(** [attempt_move_no_checks board start finish] creates the board assuming the move is valid*)
let attempt_move_no_checks board start finish =
  let the_piece = Game_state.get_square board start in
  let board_with_piece_removed =
    Game_state.set_square board start (Empty, NoPiece)
  in
  (Game_state.set_square board_with_piece_removed finish the_piece,Legal)

(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.)
   Returns[board, Legal] if the move is allowed*)
let attempt_move board start finish = if move_is_legal board start finish then attempt_move_no_checks board start finish else (board,Illegal)



