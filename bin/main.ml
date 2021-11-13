(** Code to start gameplay will go here *)

open Chess
open Unix

let start_board =
  Game_state.get_board_from_FEN
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let start_state : Game_state.game_state =
  {
    board = start_board;
    white_taken = [];
    black_taken = [];
    time = (300, 300);
  }

let _ = Ui.show_start

let turn_swap result =
  match result with
  | x, Game_state.Legal, p -> (Game_state.swap_turn x, p)
  | x, Game_state.Illegal, p ->
      print_endline "Illegal move, please enter new move.";
      (x, p)

let update_state (state : Game_state.game_state) new_board taken =
  match taken with
  | Game_state.Empty, _ -> { state with board = new_board }
  | x -> (
      match x with
      | p, Game_state.White ->
          {
            state with
            board = new_board;
            white_taken = p :: state.white_taken;
          }
      | p, Game_state.Black ->
          {
            state with
            board = new_board;
            black_taken = p :: state.black_taken;
          }
      | _ -> failwith "Cannot add piece of invalid color to taken.")

let rec gameplay_loop (state : Game_state.game_state) =
  let _ = Ui.update_display state in
  let t = Unix.gettimeofday () in
  let move = Gameplay.take_move "" in
  let move_result =
    Move_validation.attempt_move state.board move.start move.next
  in
  let exec_time = Unix.gettimeofday () -. t in
  let new_time =
    Gameplay.print_time
      (Game_state.color_to_move state.board)
      state
      (int_of_float exec_time)
  in
  let result = turn_swap move_result in
  let new_board = fst result in
  let taken_piece = snd result in
  let new_state = update_state state new_board taken_piece in
  gameplay_loop (Game_state.set_time new_state new_time)

let _ = gameplay_loop start_state
