open Game_state
open Piece

(**[double_first a] returns the first element of a tuple *)
let double_first a = match a with x, _ -> x

(**[double_second b] returns the second element of a tuple*)
let double_second a = match a with _, x -> x

(**[color_eq a b] returns true if two colors are the same *)
let color_eq a b =
  (a = Black && b = Black) || (a = White && b = White)

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
  |(a,b)::t when a <> Empty -> false
  |_ -> failwith("clear_path failure")

(**[friendly_fire board start finish] returns true if start and finish hold pieces of the same color*)
let friendly_fire board start finish =
  let the_piece = Game_state.get_square board start in
  let target_piece = Game_state.get_square board finish in
  if (double_first target_piece) = Empty then false else
    ((double_second the_piece) = Black && (double_second target_piece) = Black) || ((double_second the_piece) = White && (double_second target_piece) = White)

let pawn_legality board start finish =
  (*white goes up *)
  if double_second (Game_state.get_square board start) = White && finish.rank<start.rank then false 
  (*Black goes down *)
  else if double_second (Game_state.get_square board start) = Black && finish.rank>start.rank then false
  (*Can only move by two at start*)
  else if (finish.rank-start.rank = 2 && start.rank <> 2)  || (start.rank-finish.rank = 2 && start.rank <> 7) then false
  (*Can only move diagonally if theres a piece there*)
  else if start.file<>finish.file &&  double_first (Game_state.get_square board finish) = Empty then false
  (*Can't take pieces moving vertically *)
  else if start.file=finish.file && double_first (Game_state.get_square board finish) <> Empty then false
  else let possible_moves = Piece.get_moves (double_first (get_square board start))  (start.file,start.rank) in
    List.exists (equal_tuple (finish.file,finish.rank)) possible_moves

let piece_legality board start finish = 
  (*et moves_list = get_moves (double_first (get_square board start)) (start.rank,start.finish) in *)
  if  double_first (Game_state.get_square board start) = Pawn then pawn_legality board start finish else
    let possible_moves = Piece.get_moves (double_first (get_square board start))  (start.file,start.rank) in
    List.exists (equal_tuple (finish.file,finish.rank)) possible_moves

let right_color board start = 
  let the_piece = Game_state.get_square board start in
  (double_second the_piece) = Game_state.color_to_move board

let white_king_moved = ref false
let white_queen_rook_moved = ref false
let white_king_rook_moved = ref false
let black_king_moved = ref false
let black_queen_rook_moved = ref false
let black_king_rook_moved = ref false




let update_castle_availability_white board start finish=
  if Game_state.get_square board {rank = 1;file = 8} <> (Rook,White) then white_king_rook_moved := true;
  if Game_state.get_square board {rank = 1;file = 1} <> (Rook,White)  then white_queen_rook_moved := true;
  if Game_state.get_square board {rank = 1;file = 5} <> (King,White)  then white_king_moved := true;
  let b1 = Game_state.get_square board  {rank = 1;file = 2} in
  let c1 = Game_state.get_square board  {rank = 1;file = 3} in
  let d1 = Game_state.get_square board  {rank = 1;file = 4} in
  let check_castle_queen = (fst b1 = Empty) && (fst c1 = Empty) && (fst d1 = Empty) && not !white_king_moved && not !white_queen_rook_moved in
  let f1 = Game_state.get_square board  {rank = 1;file = 6} in
  let g1 = Game_state.get_square board  {rank = 1;file = 7} in
  let check_castle_king = (fst f1 = Empty) && (fst g1 = Empty) && not !white_king_moved && not !white_king_rook_moved in
  Game_state.set_castle_availability board White {king_side = check_castle_king; queen_side = check_castle_queen} 

let update_castle_availability_black board start finish=
  if Game_state.get_square board {rank = 8;file = 8} <> (Rook,Black) then black_king_rook_moved := true;
  if Game_state.get_square board {rank = 8;file = 1} <> (Rook,Black)  then black_queen_rook_moved := true;
  if Game_state.get_square board {rank = 8;file = 5} <> (King,Black)  then black_king_moved := true;
  let b8 = Game_state.get_square board  {rank = 8;file = 2} in
  let c8 = Game_state.get_square board  {rank = 8;file = 3} in
  let d8 = Game_state.get_square board  {rank = 8;file = 4} in
  let check_castle_queen = (fst b8 = Empty) && (fst c8 = Empty) && (fst d8 = Empty) && not !black_king_moved && not !black_queen_rook_moved in
  let f8 = Game_state.get_square board  {rank = 8;file = 6} in
  let g8 = Game_state.get_square board  {rank = 8;file = 7} in
  let check_castle_king = (fst f8 = Empty) && (fst g8 = Empty) && not !black_king_moved && not !black_king_rook_moved in
  Game_state.set_castle_availability board Black {king_side = check_castle_king; queen_side = check_castle_queen} 

let update_castle_availability board start finish = 
  if (Game_state.color_to_move board) = White then update_castle_availability_white board start finish
  else update_castle_availability_black board start finish

let detect_castle board start finish =
  let king = Game_state.get_square board start in
  if (start = {rank = 1;file = 5} && fst king = King && (finish = {rank = 1;file = 7} || finish = {rank = 1;file = 3})) ||
     (start = {rank = 8;file = 5} && fst king = King && (finish = {rank = 8;file = 7} || finish = {rank = 8;file = 3}))
  then true
  else false

let castle board start finish =
  if finish = {rank = 1;file = 7} && (get_castle_availability board White).king_side then
    let aboard = Game_state.set_square board {rank = 1;file = 5} (Empty, NoPiece) in
    let bboard = Game_state.set_square aboard {rank = 1;file = 8} (Empty, NoPiece) in
    let cboard = Game_state.set_square bboard {rank = 1;file = 7}  (King, White) in
    let dboard = Game_state.set_square cboard {rank = 1;file = 6}  (Rook, White) in
    (dboard,Game_state.Legal,(Empty,NoPiece))
  else if finish = {rank = 1;file = 7} && not (get_castle_availability board White).king_side then (board,Game_state.Illegal,(Empty,NoPiece))
  else if finish = {rank = 1;file = 3} && (get_castle_availability board White).queen_side then 
    let aboard = Game_state.set_square board {rank = 1;file = 5} (Empty, NoPiece) in
    let bboard = Game_state.set_square aboard {rank = 1;file = 1} (Empty, NoPiece) in
    let cboard = Game_state.set_square bboard {rank = 1;file = 3}  (King, White) in
    let dboard = Game_state.set_square cboard {rank = 1;file = 4}  (Rook, White) in
    (dboard,Game_state.Legal,(Empty,NoPiece))
  else if finish = {rank = 1;file = 3} && not (get_castle_availability board White).queen_side then (board,Game_state.Illegal,(Empty,NoPiece))

  else if finish = {rank = 8;file = 7} && (get_castle_availability board Black).king_side then
    let aboard = Game_state.set_square board {rank = 8;file = 5} (Empty, NoPiece) in
    let bboard = Game_state.set_square aboard {rank = 8;file = 8} (Empty, NoPiece) in
    let cboard = Game_state.set_square bboard {rank = 8;file = 7}  (King, Black) in
    let dboard = Game_state.set_square cboard {rank = 8;file = 6}  (Rook, Black) in
    (dboard,Game_state.Legal,(Empty,NoPiece))
  else if finish = {rank = 8;file = 7} && not (get_castle_availability board Black).king_side then (board,Game_state.Illegal,(Empty,NoPiece))
  else if finish = {rank = 8;file = 3} && (get_castle_availability board Black).queen_side then 
    let aboard = Game_state.set_square board {rank = 8;file = 5} (Empty, NoPiece) in
    let bboard = Game_state.set_square aboard {rank = 8;file = 1} (Empty, NoPiece) in
    let cboard = Game_state.set_square bboard {rank = 8;file = 3}  (King, Black) in
    let dboard = Game_state.set_square cboard {rank = 8;file = 4}  (Rook, Black) in
    (dboard,Game_state.Legal,(Empty,NoPiece))
  else if finish = {rank = 8;file = 3} && not (get_castle_availability board Black).queen_side then (board,Game_state.Illegal,(Empty,NoPiece))


  else failwith("castle error")

let detect_promotion board start finish =
  (Game_state.get_square board start = (Pawn,White) && finish.rank = 8) ||
  (Game_state.get_square board start = (Pawn,Black) && finish.rank = 1)




let move_is_legal board start finish = (piece_legality board start finish || detect_castle board start finish) && 
                                       (not (friendly_fire board start finish)) && 
                                       clear_path (pieces_in_between board start finish) &&
                                       right_color board start
(** [attempt_move_no_checks board start finish] creates the board assuming the move is valid*)
let attempt_move_no_checks board start finish =
  if detect_castle board start finish
  then castle board start finish else 
    let the_piece = Game_state.get_square board start in
    let board_with_piece_removed =
      Game_state.set_square board start (Empty, NoPiece)
    in
    (Game_state.set_square board_with_piece_removed finish the_piece,Game_state.Legal,Game_state.get_square board finish)

let attempt_move_no_checks_then_promote board start finish piece =
  if detect_castle board start finish
  then castle board start finish else 
    let the_piece = piece in
    let board_with_piece_removed =
      Game_state.set_square board start (Empty, NoPiece)
    in
    (Game_state.set_square board_with_piece_removed finish the_piece,Game_state.Legal,Game_state.get_square board finish)

let rec ask_promotion board =
  print_string  "You are attempting to promote a Pawn. Choose a piece: type Q, R, B, or N for a Queen, Rook, Bishop, or Knight";
  let promoted_string = read_line () in 
  match promoted_string with 
  |"Q" -> (Queen,Game_state.color_to_move board)
  |"R" -> (Rook,Game_state.color_to_move board)
  |"B" -> (Bishop,Game_state.color_to_move board)
  |"N" -> (Knight,Game_state.color_to_move board)
  |_ -> ask_promotion board

(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.)
   Returns[board, Legal] if the move is allowed*)
let attempt_move board start finish = 


  if move_is_legal board start finish 
  then 
    if detect_promotion board start finish 
    then let promote_piece = ask_promotion board in
      let board_castle_availablility_adjusted = update_castle_availability board start finish in
      attempt_move_no_checks_then_promote board_castle_availablility_adjusted start finish promote_piece
    else 
      let board_castle_availablility_adjusted = update_castle_availability board start finish in
      attempt_move_no_checks board_castle_availablility_adjusted start finish 
  else (board,Game_state.Illegal,(Empty,NoPiece))

(*let attempt_move = attempt_move_no_checks*)

