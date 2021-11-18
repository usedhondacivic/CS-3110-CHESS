open Util

type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

type castle_rights = {
  king_side : bool;
  queen_side : bool;
}

type time = int * int

type color =
  | White
  | Black
  | NoPiece

let piece_str =
  [
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
    ((Empty, NoPiece), " ");
  ]

let get_piece_str piece = List.assoc piece piece_str

type board_coord = {
  rank : int;
  file : int;
}

type board = {
  game_board : (piece * color) list list;
  current_turn : color;
  castle_availability : castle_rights * castle_rights;
  en_passant_target : board_coord option;
  (*If a pawn was moved two spaces last move, [en_passant_target] is the
    square that an opposing pawn could move to in order to take en
    passant*)
  half_move_count : int;
  (*[half_move_count] is the number of moves each player has made since
    the last pawn advance or piece capture. When this number reaches 100
    the game ends in a draw.*)
  full_move_count : int;
      (*[full_move_count] is the number of completed turns in the
        game.*)
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

let get_time (curr_state : game_state) = curr_state.time

let set_time (curr_state : game_state) new_time =
  { curr_state with time = new_time }

let get_square (curr_board : board) coord =
  let board = curr_board.game_board in
  let rank = List.nth board (coord.rank - 1) in
  List.nth rank (coord.file - 1)

let set_square (curr_board : board) coord piece =
  {
    curr_board with
    game_board =
      List.mapi
        (fun rank lst ->
          if rank + 1 = coord.rank then
            List.mapi
              (fun file p -> if file + 1 = coord.file then piece else p)
              lst
          else lst)
        curr_board.game_board;
  }

let move_piece (curr_board : board) start_coord end_coord =
  let old = get_square curr_board start_coord in
  let new_board = set_square curr_board end_coord old in
  set_square new_board start_coord (Empty, White)

let swap_turn (curr_board : board) =
  {
    curr_board with
    current_turn =
      (match curr_board.current_turn with
      | White -> Black
      | Black -> White
      | NoPiece ->
          failwith "Invalid board: current_turn must be White or Black.");
  }

let get_king curr_board color =
  failwith "get_king has not been implemented."

let get_castle_availability curr_board color =
  match color with
  | White ->
      let x, _ = curr_board.castle_availability in
      x
  | Black ->
      let _, x = curr_board.castle_availability in
      x
  | NoPiece -> failwith "Can't get castle availability of NoPiece"

let set_castle_availability curr_board color new_rights =
  match color with
  | White ->
      {
        curr_board with
        castle_availability =
          (new_rights, get_castle_availability curr_board Black);
      }
  | Black ->
      {
        curr_board with
        castle_availability =
          (get_castle_availability curr_board White, new_rights);
      }
  | NoPiece -> failwith "Can't set castle availability of NoPiece"

let get_piece str =
  match str with
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

let get_piece_rep p =
  match p with
  | Pawn, Black -> "p"
  | Rook, Black -> "r"
  | Knight, Black -> "n"
  | Bishop, Black -> "b"
  | Queen, Black -> "q"
  | King, Black -> "k"
  | Pawn, White -> "P"
  | Rook, White -> "R"
  | Knight, White -> "N"
  | Bishop, White -> "B"
  | Queen, White -> "Q"
  | King, White -> "K"
  | x -> failwith "Cannot get string of invalid piece"

(*Reverse taken from
  https://stackoverflow.com/questions/7382140/reversing-a-list-in-ocaml-using-fold-left-right*)
let reverse lst = List.fold_left (fun lrev b -> b :: lrev) [] lst

let rec build_row row lst =
  match lst with
  | x :: t when int_of_char x >= 49 && int_of_char x <= 56 ->
      build_row
        (Util.build_list (int_of_char x - 48) (Empty, White) @ row)
        t
      (*ewwwww*)
  | h :: t -> build_row (get_piece h :: row) t
  | [] -> row

type split_fen = {
  position : string;
  turn : string;
  castle : string;
  en_passant : string;
  full_move : string;
  half_move : string;
}

let color_from_string = function
  | "w" -> White
  | "b" -> Black
  | _ -> failwith "Invalid FEN string"

let castle_rights_from_string str =
  ( {
      king_side = String.contains str 'K';
      queen_side = String.contains str 'Q';
    },
    {
      king_side = String.contains str 'k';
      queen_side = String.contains str 'q';
    } )

let en_passant_from_string str =
  match Util.explode str with
  | [ r; f ] ->
      Some { rank = int_of_char r - 96; file = int_of_char f - 48 }
  | [ '-' ] -> None
  | _ -> failwith "Illegal FEN string: malformed en_passant target"

let get_board_from_FEN fen_str =
  let split_fen =
    match String.split_on_char ' ' fen_str with
    | [ p; t; c; e; f; h ] ->
        {
          position = p;
          turn = t;
          castle = c;
          en_passant = e;
          full_move = f;
          half_move = h;
        }
    | _ -> failwith "Invalid FEN string"
  in
  let broken = reverse (String.split_on_char '/' split_fen.position) in
  let exploded = List.map Util.explode broken in
  let rev = List.map reverse exploded in
  {
    game_board = List.map (build_row []) rev;
    current_turn = color_from_string split_fen.turn;
    castle_availability = castle_rights_from_string split_fen.castle;
    en_passant_target = en_passant_from_string split_fen.en_passant;
    full_move_count = int_of_string split_fen.full_move;
    half_move_count = int_of_string split_fen.half_move;
  }

let color_to_move curr_board = curr_board.current_turn

let list_to_string lst =
  let str_lst = List.map get_piece_rep lst in
  List.fold_left ( ^ ) "" str_lst

let board_to_list board = List.map list_to_string board.game_board

let state_to_rep state = failwith "Not yet implemented."