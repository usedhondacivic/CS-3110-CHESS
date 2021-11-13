type move = {
  start : Game_state.board_coord;
  next : Game_state.board_coord;
}

(** check if file letter A-H*)
let check_file file_num =
  if file_num - 64 >= 1 && file_num - 64 <= 8 then true else false

(** check if rank number in range 1-8*)
let check_rank rank_num =
  let rank_num = rank_num in
  if rank_num >= 1 && rank_num <= 8 then true else false

(** checking file and rank in range, returns true if valid and false if
    not*)
let check_file_rank place =
  let file_place = Char.code place.[0] in
  let rank_place = int_of_string (String.sub place 1 1) in
  if check_file file_place && check_rank rank_place then
    Game_state.{ rank = rank_place; file = file_place - 64 }
  else failwith "new input needed"

let check start_end =
  let sep = String.split_on_char ' ' start_end in
  let start_end = List.filter (fun x -> x <> "") sep in
  let moves_list = List.map check_file_rank start_end in
  match moves_list with
  | [] -> failwith "new input needed"
  | [ hd; tl ] -> { start = hd; next = tl }
  | _ :: _ -> failwith "new input needed"

let get_start start_end =
  let input_move = check start_end in
  input_move.start

let rec take_move s =
  let _ = print_endline "Where would you like to move? (Ex: A4 A6): " in
  let s = read_line () in
  try check s
  with exc ->
    let _ = print_endline "Wrong input. Try again: " in
    take_move s

let print_time color state exec_time =
  match color with
  | Game_state.White -> (
      match Game_state.get_time state with a, b -> (a - exec_time, b))
  | Game_state.Black -> (
      match Game_state.get_time state with a, b -> (a, b - exec_time))
  | Game_state.NoPiece -> failwith "not meet procondition"
