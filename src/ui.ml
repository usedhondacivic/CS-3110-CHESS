open ANSITerminal

let square_size = (11, 5)

let light_color = on_white
let dark_color = on_yellow

let rec string_of_length symb length = match length with
  | x when x = 0 -> ""
  | x -> symb ^ string_of_length symb (length - 1)

let update_display curr_state = 
  failwith "update_board has not been implemented"
  
let rec row_helper col row offset board =
  if col > 0 then 
    let c = if (col + offset) mod 2 = 1 then dark_color else light_color in
    if row = (snd square_size) / 2 + 1 then
      let piece = Game_state.get_square board {rank = (offset - 1); file = (col - 1)} in
      let color = match snd piece with 
      | White -> Foreground White
      | Black -> Foreground Black
      in
      let _ = print_string [c] (string_of_length " " ((fst square_size) / 2)) in
      let _ = print_string [c ; color] (Game_state.get_piece_str piece) in
      let _ = print_string [c] (string_of_length " " ((fst square_size) / 2)) in
      row_helper (col - 1) row offset board
    else 
      let _ = print_string [c] (string_of_length " " (fst square_size)) in
      row_helper (col - 1) row offset board
  else print_string [on_default] "\n"

let rec square_helper row offset board =
  if row > 0 then
    let _ = row_helper 8 row offset board in
    square_helper (row - 1) offset board

let draw_row offset board =
  square_helper (snd square_size) offset board

let rec board_helper row board =
  if row > 0 then
    let _ = draw_row row board in
    board_helper (row - 1) board

let draw_empty = 
  board_helper 8