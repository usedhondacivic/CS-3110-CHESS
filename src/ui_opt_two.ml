(** Code to start gameplay will go here *)

(*print_endline "bin/main.ml has yet to be implemented."*)

open ANSITerminal

(*let _ = print_string [ blue; on_green ] "\n\n\n\ntest\n\n\n\n"*)

let style = Foreground Red

let _ = print_string [ red ] "hello world"

let termsize = size

let width t = match t with h, t -> h + 0

let print_size = print_int 1