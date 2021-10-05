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
  | Empty

type board = {
  board: (piece * color) list list;
  current_turn : color;
  castle_availability : bool * bool 
}

type board_coord = {
  rank : int;
  file : int;
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

let from_location curr_board coord =
  failwith "from_location has not been implemented."

let get_king curr_board color =
  failwith "get_king has not been implemented."

let get_castle_availability curr_board color = 
  failwith "get_castle_availability has not been implemented."

let get_board_from_FEN fen_str =
  failwith "get_board_from_FEN has not been implemented." 
let color_to_move =
  failwith "color_to_move has not been implemented"