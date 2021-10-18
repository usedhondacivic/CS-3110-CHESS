open Util

type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

type time = int * int

type color =
  | White
  | Black

let piece_str = [
  ((Pawn, Black), "♟︎"); 
  ((Knight, Black), "♞"); 
  ((Bishop, Black), "♝"); 
  ((Rook, Black), "♜"); 
  ((Queen, Black), "♛"); 
  ((King, Black), "♚");
  ((Pawn, White), "♟︎"); 
  ((Knight, White), "♞"); 
  ((Bishop, White), "♝"); 
  ((Rook, White), "♜"); 
  ((Queen, White), "♛"); 
  ((King, White), "♚");
  ((Empty, White), " ");
  ((Empty, Black), " ");
  ]

let get_piece_str piece = List.assoc piece piece_str

type board_coord = {
  rank : int;
  file : int;
}

type board = {
  game_board: (piece * color) list list;
  current_turn : color;
  castle_availability : bool * bool;
  en_passant_target : board_coord option;
  (*If a pawn was moved two spaces last move, [en_passant_target] is the square that an opposing pawn could move to in order to take en passant*)
  half_move_count : int;
  (*[half_move_count] is the number of moves each player has made since the last pawn advance or piece capture. When this number reaches 100 the game ends in a draw.*)
  full_move_count : int
  (*[full_move_count] is the number of completed turns in the game.*)
}

type game_state = {
  board : board;
  white_taken : piece list;
  black_taken : piece list;
  time : time;
}

type result =
  | Legal
  | Illegal

let game_over_check curr_board =
  failwith "attempt_move has not been implemented."

let get_moves curr_board =
  failwith "get_moves has not been implemented."

let get_square (curr_board : board) coord =
  let board = curr_board.game_board in 
  let rank = List.nth board coord.rank in 
  List.nth rank coord.file

let set_square (curr_board : board) coord =
  failwith "set_square had not been implemented."

let get_king curr_board color =
  failwith "get_king has not been implemented."

let get_castle_availability curr_board color = match color with
  | White -> let ( x , _ ) = curr_board.castle_availability in x
  | Black ->  let( _ , x ) = curr_board.castle_availability in x

let get_piece str = match str with
| 'p' -> (Pawn, Black)
| 'r' -> (Rook, Black)
| 'n' -> (Knight, Black)
| 'b' -> (Bishop, Black)
| 'q' -> (Queen, Black)
| 'k' -> (King, Black)
| 'P' -> (Pawn, White)
| 'R' -> (Rook, White)
| 'N' -> (Knight, White)
| 'B' -> (Bishop, White)
| 'Q' -> (Queen, White)
| 'K' -> (King, White)
| x -> failwith ("Invalid FEN string: " ^ Char.escaped x)

let rec build_row row lst = match lst with
  | x :: t when int_of_char x >= 49 && int_of_char x <= 56 -> build_row ((Util.build_list ((int_of_char x) - 48) (Empty, White)) @ row) t (*ewwwww*)
  | h :: t -> build_row ((get_piece h) :: row) t
  | [] -> row

(*Reverse taken from https://stackoverflow.com/questions/7382140/reversing-a-list-in-ocaml-using-fold-left-right*)
let reverse = List.fold_left ( fun lrev b -> b::lrev) [];;

let get_board_from_FEN fen_str =
  let pos_string = match String.split_on_char ' ' fen_str with 
  | h :: t -> h
  | [] -> failwith "Invalid FEN string"
  in
  let broken = String.split_on_char '/' pos_string in
  let exploded = List.map Util.explode broken in
  {
    game_board = List.map (build_row []) (reverse exploded);
    current_turn = White;
    castle_availability = (true, true);
    en_passant_target = None;
    half_move_count = 0;
    full_move_count = 0;
  }
  
let color_to_move =
  White