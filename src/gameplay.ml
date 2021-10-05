type board_coord = {
  rank : int;
  file : int
}

type move = { 
  start : board_coord; 
  next : board_coord
}

(** check if file in range*)
let check_file file = 
  let file_num = int_of_string file in
  if file_num - 65 >= 0 && file_num - 65 < 25 then true else false

(** check if rank in range*)
let check_rank rank = 
  let rank = int_of_string rank in 
  if rank >= 1 && rank < 8 then true else false
  
let check_file_rank place = 
  failwith "check_file_rank has not been implemented yet"


let check start_end =
  (** change to return move*)
  let sep = String.split_on_char ' ' start_end in 
  let start_end  = List.filter (fun x -> x <> "") sep in 
  List.map check_file_rank start_end