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

let rec string_of_length symb length = match length with
  | x when x = 0 -> ""
  | x -> symb ^ string_of_length symb (length - 1)
  
let rec row_helper col row offset board =
  if col > 0 then 
    let back = if (col + offset) mod 2 = 1 then dark_color else light_color in
    if row = (snd square_size) / 2 + 1 then
      let piece = Game_state.get_square board {rank = (offset - 1); file = (col - 1)} in
      let front = match snd piece with 
      | White -> white_code
      | Black -> black_code
      in
      (*let _ = print_string [c] (string_of_length " " ((fst square_size) / 2)) in
      let _ = print_string [c ; color] (Game_state.get_piece_str piece) in
      let _ = print_string [c] (string_of_length " " ((fst square_size) / 2)) in*)
      let _ = print_color front back (string_of_length " " ((fst square_size) / 2)) in
      let _ = print_color front back (Game_state.get_piece_str piece) in
      let _ = print_color front back (string_of_length " " ((fst square_size) / 2)) in
      row_helper (col - 1) row offset board
    else 
      let _ = print_color 0 back (string_of_length " " (fst square_size)) in
      row_helper (col - 1) row offset board
  else print_string "\n"

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

let update_display (curr_state : Game_state.game_state) = 
  board_helper 8 curr_state.board

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
         \\/____/                  \\/____/                  \\/____/                  \\/____/                  \\/____/         
"
  in let _ = print_endline "\n\nPress enter to begin..." in
  let _ = read_line () in
  print_endline "Welcome!"