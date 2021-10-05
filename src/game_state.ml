type time = int * int

type color =
  | White
  | Black
  | Empty

type board = (Piece.piece * color) list list

type board_coord = {
  rank : int;
  file : int;
}

type game_state = {
  board : board;
  current_turn : color;
  white_taken : Piece.piece list;
  black_taken : Piece.piece list;
  time : time;
}

type result =
  | Legal
  | Illegal

let game_over_check curr_board =
  failwith "attempt_move has not been implemented."

let get_moves curr_board =
  failwith "get_moves has not been implemented."

let from_location curr_board coord =
  failwith "from_location has not been implemented."

let get_king curr_board color =
  failwith "get_moves has not been implemented."

let get_castle_availability curr_board color = 
  failwith "get_castle_availability has not been implemented."