type input_move = { 
  start : Game_state.board_coord; 
  next : Game_state.board_coord
}

(** check if file in range*)
let check_file file = 
  let file_num = int_of_string file in
  if file_num - 65 >= 0 && file_num - 65 < 25 then true else false

(** check if rank in range*)
let check_rank rank_num = 
  let rank_num = int_of_string rank_num in 
  if rank_num >= 1 && rank_num < 8 then true else false
  
let check_file_rank place = 
  let file_place = String.sub place 0 1 in 
  let rank_place = String.sub place 1 1 in 
  if check_file file_place && check_rank rank_place
    then failwith "new input needed"
    else failwith "new input needed"

let check start_end =
  (** change to return move*)
  let sep = String.split_on_char ' ' start_end in 
  let start_end  = List.filter (fun x -> x <> "") sep in 
  List.map check_file_rank start_end