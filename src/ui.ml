open ANSITerminal

let scale_factor = 2
let square_size = (5 * scale_factor, 5)

let rec string_of_length symb length = match length with
  | x when x = 0 -> ""
  | x -> symb ^ string_of_length symb (length - 1)

let update_display curr_state = 
  failwith "update_board has not been implemented"

let rec draw_square =
  let rec square_helper row = 
    if row > 0 then string_of_length " " (fst square_size) ^ "\n" ^ square_helper (row - 1) else ""
  in
  square_helper (snd square_size)

  
let rec row_helper col offset =
  if col > 0 then 
    let c = if (col + offset) mod 2 = 1 then on_white else on_black in
    let _ = print_string [c] (string_of_length " " (fst square_size)) in
    row_helper (col - 1) offset
else print_string [on_default] "\n"

let rec square_helper row offset =
  if row > 0 then
    let _ = row_helper 8 offset in
    square_helper (row - 1) offset

let draw_row offset=
  square_helper (snd square_size)  offset

let rec board_helper row =
  if row > 0 then
    let _ = draw_row row in
    board_helper (row - 1)

let draw_empty = 
  board_helper 8