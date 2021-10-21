(*https://github.com/ix/ocaml-termcolors/blob/master/termcolor.ml*)
let foreground col text =
  let open Printf in
    sprintf "\x1B[38;5;%dm%s\x1B[0m" col text
  
let background col text =
  let open Printf in
    sprintf "\x1B[48;5;%dm%s\x1B[0m" col text

let print_color front back text = let open Printf in 
  let b = background back text in 
  print_string (foreground front b)

let square_size = (7, 3)

let light_color = 221
let dark_color = 94

let black_code = 16
let white_code = 255

let time_to_string ((w, b) : Game_state.time) (color : Game_state.color)  =  match color with White -> string_of_int w | Black -> string_of_int b | NoPiece -> failwith "Cannot get time of unknown color."

let reverse lst = List.fold_left ( fun lrev b -> b::lrev) [] lst

let get_sidebar_row state row =
  let sidebar = 
    "
    Black time remaining:
    " ^ time_to_string (Game_state.get_time state) Game_state.Black ^ "

    White time remaining:
    " ^ time_to_string (Game_state.get_time state) Game_state.White ^ "
    " in
    let split = reverse (String.split_on_char '\n' sidebar) in
    try List.nth split row with
    | _ -> ""

let rec string_of_length symb length = match length with
  | x when x = 0 -> ""
  | x -> symb ^ string_of_length symb (length - 1)
  
let rec row_helper col row offset board =
  if col > 0 then 
    let back = if (col + offset) mod 2 = 1 then dark_color else light_color in
    if row = (snd square_size) / 2 + 1 then
      let piece = Game_state.get_square board {rank = offset; file = 9 - col} in
      let front = match snd piece with 
      | White -> white_code
      | Black -> black_code
      | NoPiece -> white_code
      in
      let _ = print_color front back (string_of_length " " ((fst square_size) / 2)) in
      let _ = print_color front back (Game_state.get_piece_str piece) in
      let _ = print_color front back (string_of_length " " ((fst square_size) / 2)) in
      row_helper (col - 1) row offset board
    else 
      let _ = print_color 0 back (string_of_length " " (fst square_size)) in
      row_helper (col - 1) row offset board

let rec square_helper row offset (state : Game_state.game_state) =
  let board = state.board in
  if row > 0 then
    let row_disp = ("   " ^ (if row = (snd square_size) / 2 + 1 then string_of_int offset else " ") ^ "   ") in
    let _ = print_string row_disp in
    let _ = row_helper 8 row offset board in 
    let sidebar = get_sidebar_row state (((offset - 1) * snd square_size) + row) in
    let _ = print_string (row_disp ^ sidebar ^ "\n") in
    square_helper (row - 1) offset state

let draw_row offset state =
  square_helper (snd square_size) offset state

let rec board_helper row state =
  if row > 0 then
    let _ = draw_row row state in
    board_helper (row - 1) state

let update_display (curr_state : Game_state.game_state) = 
  let half_spacer = string_of_length " " ((fst square_size) / 2) in
  let spacer = string_of_length " " ((fst square_size) - 1) in
  let top_row = "       " ^ half_spacer ^ "A" ^ spacer ^ "B" ^ spacer ^ "C" ^ spacer ^ "D" ^ spacer ^ "E" ^ spacer ^ "F" ^ spacer ^ "G" ^ spacer ^ "H" ^ "\n" in
  let _ = print_endline top_row in
  let _ = board_helper 8 curr_state in
  let _ = print_endline ("\n" ^ top_row) in
  let turn = match Game_state.color_to_move curr_state.board with White -> "White" | Black -> "Black" | NoPiece -> failwith "Invalid player color" in 
  print_endline (turn ^ " to move!")

let show_start =
  let _ = print_endline "CS 3110 presents" in
  let _ = print_color light_color black_code "
          _____                    _____                    _____                    _____                    _____          
         /\\    \\                  /\\    \\                  /\\    \\                  /\\    \\                  /\\    \\         
        /::\\    \\                /::\\____\\                /::\\    \\                /::\\    \\                /::\\    \\        
       /::::\\    \\              /:::/    /               /::::\\    \\              /::::\\    \\              /::::\\    \\       
      /::::::\\    \\            /:::/    /               /::::::\\    \\            /::::::\\    \\            /::::::\\    \\      
     /:::/\\:::\\    \\          /:::/    /               /:::/\\:::\\    \\          /:::/\\:::\\    \\          /:::/\\:::\\    \\     
    /:::/  \\:::\\    \\        /:::/____/               /:::/__\\:::\\    \\        /:::/__\\:::\\    \\        /:::/__\\:::\\    \\    
   /:::/    \\:::\\    \\      /::::\\    \\              /::::\\   \\:::\\    \\       \\:::\\   \\:::\\    \\       \\:::\\   \\:::\\    \\   
  /:::/    / \\:::\\    \\    /::::::\\    \\   _____    /::::::\\   \\:::\\    \\    ___\\:::\\   \\:::\\    \\    ___\\:::\\   \\:::\\    \\  
 /:::/    /   \\:::\\    \\  /:::/\\:::\\    \\ /\\    \\  /:::/\\:::\\   \\:::\\    \\  /\\   \\:::\\   \\:::\\    \\  /\\   \\:::\\   \\:::\\    \\ 
/:::/____/     \\:::\\____\\/:::/  \\:::\\    /::\\____\\/:::/__\\:::\\   \\:::\\____\\/::\\   \\:::\\   \\:::\\____\\/::\\   \\:::\\   \\:::\\____\\ 
\\:::\\    \\      \\::/    /\\::/    \\:::\\  /:::/    /\\:::\\   \\:::\\   \\::/    /\\:::\\   \\:::\\   \\::/    /\\:::\\   \\:::\\   \\::/    / 
 \\:::\\    \\      \\/____/  \\/____/ \\:::\\/:::/    /  \\:::\\   \\:::\\   \\/____/  \\:::\\   \\:::\\   \\/____/  \\:::\\   \\:::\\   \\/____/ 
  \\:::\\    \\                       \\::::::/    /    \\:::\\   \\:::\\    \\       \\:::\\   \\:::\\    \\       \\:::\\   \\:::\\    \\     
   \\:::\\    \\                       \\::::/    /      \\:::\\   \\:::\\____\\       \\:::\\   \\:::\\____\\       \\:::\\   \\:::\\____\\    
    \\:::\\    \\                      /:::/    /        \\:::\\   \\::/    /        \\:::\\  /:::/    /        \\:::\\  /:::/    /    
     \\:::\\    \\                    /:::/    /          \\:::\\   \\/____/          \\:::\\/:::/    /          \\:::\\/:::/    /     
      \\:::\\    \\                  /:::/    /            \\:::\\    \\               \\::::::/    /            \\::::::/    /      
       \\:::\\____\\                /:::/    /              \\:::\\____\\               \\::::/    /              \\::::/    /       
        \\::/    /                \\::/    /                \\::/    /                \\::/    /                \\::/    /        
         \\/____/                  \\/____/                  \\/____/                  \\/____/                  \\/____/         "
  in let _ = print_endline "\n\nPress enter to begin..." in
  let _ = read_line () in
  print_endline "Welcome!"