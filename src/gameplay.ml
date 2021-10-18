type move = {
  start : Game_state.board_coord;
  next : Game_state.board_coord;
}

(** check if file letter A-H*)
let check_file file_num =
  if file_num - 64 >= 1 && file_num - 64 < 8 then true else false

(** check if rank number in range 1-8*)
let check_rank rank_num =
  let rank_num = rank_num in
  if rank_num >= 1 && rank_num < 8 then true else false

(** checking file and rank in range, returns true if valid and false if
    not*)
let check_file_rank place =
  let file_place = Char.code place.[0] in
  let rank_place = int_of_string (String.sub place 1 1) in
  if check_file file_place && check_rank rank_place then
    Game_state.{ rank = rank_place; file = file_place - 64 }
  else failwith "new input needed"

(**if input valid constructs type move *)
let check start_end =
  let sep = String.split_on_char ' ' start_end in
  let start_end = List.filter (fun x -> x <> "") sep in
  let moves_list = List.map check_file_rank start_end in
  match moves_list with
  | [] -> failwith "not meet precondition"
  | [ hd; tl ] -> { start = hd; next = tl }
  | _ :: _ -> failwith "not meet precondition"

(**getter, encap: don't declare its a record*)
let get_start start_end =
  let input_move = check start_end in
  input_move.start
