open OUnit2
open Chess
open Game_state
open Gameplay
open Move_validation
open Piece
open Ui

(**Test Plan: The test suite tests the modules: game_state, game_play,
   move_validation, piece and ui. Tested in OUnit: 1)game_state,
   2)game_play, 3)move_validation, 4)piece. Manually tested : 1)ui Piece
   is tested using Glass-Box testing. *)

(**Print tuple for testing *)
let print_tuples = function
  | {
      start = { rank = s_r; file = f_r };
      next = { rank = n_r; file = n_f };
    } ->
      Printf.sprintf "%i, %i, %i, %i;" s_r f_r n_r n_f

(** Construct OUnit tests for Game_state*)

(* http://www.lutanho.net/pgn/pgn2fen.html*)

(*https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let rec list_to_string b =
  match b with [] -> "" | h :: t -> h ^ "\n" ^ list_to_string t

let check_move curr_fen next_fen move =
  let m = Gameplay.check move in
  let curr_board = Game_state.get_board_from_FEN curr_fen in
  let next_board = Game_state.get_board_from_FEN next_fen in
  let after_move, _, _ =
    Move_validation.attempt_move curr_board m.start m.next
  in
  ( Game_state.compare_game_board next_board after_move,
    move,
    Game_state.board_to_list curr_board,
    Game_state.board_to_list after_move,
    Game_state.board_to_list next_board )

let check_move_print out =
  match out with
  | true, _, _, _, _ -> "true"
  | false, move, prev, actual, expected ->
      begin
        "\nStart board: \n\n" ^ list_to_string prev
        ^ "\nMove being made: \n" ^ move ^ "\n\nProgram output: \n\n"
        ^ list_to_string actual ^ "\nExpected output: \n\n"
        ^ list_to_string expected
      end

let test printer ?(cmp = ( = )) name expected_val actual_val =
  name >:: fun _ -> assert_equal ~printer ~cmp expected_val actual_val

let check_move_test =
  test check_move_print
    ?cmp:(Some (fun (a, _, _, _, _) (b, _, _, _, _) -> a = b))

let t_output = (true, "", [], [], [])

let rec get_checks data =
  match data with
  | [ before; move; after ] -> [ (move, before, after, move) ]
  | before :: move :: after :: t ->
      (move, before, after, move) :: get_checks (after :: t)
  | _ -> []

let generate_move_tests lst =
  lst
  |> List.map (fun (n, b, a, m) -> (n, check_move b a m))
  |> List.map (fun (n, m) -> check_move_test n t_output m)

let game_one =
  read_file "./test/data/game_one.csv"
  |> List.map (fun a -> String.split_on_char ',' a)
  |> List.flatten

let game_one_tests = game_one |> get_checks

let illegal_move_tests =
  [
    ( "Empty start square",
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
      "A3 A4" );
    ( "King side castle after king move",
      "rnbqkbnr/pppppppp/8/8/2B1P3/5N2/PPPPQPPP/RNB1K2R w Qkq - 0 1",
      "rnbqkbnr/pppppppp/8/8/2B1P3/5N2/PPPPQPPP/RNB1K2R w Qkq - 0 1",
      "E1 G1" );
    ( "Queen side castle after king move",
      "rnbqkbnr/pppppppp/8/8/2B1P3/2NP1N2/PPPBQPPP/R3K2R w kq - 0 1",
      "rnbqkbnr/pppppppp/8/8/2B1P3/2NP1N2/PPPBQPPP/R3K2R w kq - 0 1",
      "E1 C1" );
    ( "Queen side castle with piece in the way",
      "rnbqkbnr/pppppppp/8/8/2B1P3/3P1N2/PPPBQPPP/RN2K2R w kq - 0 1",
      "rnbqkbnr/pppppppp/8/8/2B1P3/3P1N2/PPPBQPPP/RN2K2R w kq - 0 1",
      "E1 C1" );
    ( "King side castle with piece in the way",
      "rnbqkbnr/pppppppp/8/8/1BB1P3/3PQ3/PPP2PPP/RN2KN1R w kq - 0 1",
      "rnbqkbnr/pppppppp/8/8/1BB1P3/3PQ3/PPP2PPP/RN2KN1R w kq - 0 1",
      "E1 G1" );
    ( "King side castle with check in the way",
      "rnb1kbnr/pppppppp/8/1q6/1B1PP3/1B2Q1N1/PPP2PPP/RN2K2R w KQkq - \
       0 1",
      "rnb1kbnr/pppppppp/8/1q6/1B1PP3/1B2Q1N1/PPP2PPP/RN2K2R w KQkq - \
       0 1",
      "E1 G1" );
    ( "Queen side castle with check in the way",
      "rnb1kbnr/pppppppp/8/8/1B1PP1q1/1B1NQ1N1/PPP2PPP/R3K2R w KQkq - \
       0 1",
      "rnb1kbnr/pppppppp/8/8/1B1PP1q1/1B1NQ1N1/PPP2PPP/R3K2R w KQkq - \
       0 1",
      "E1 C1" );
    ( "Move into check",
      "7k/6r1/8/8/8/8/7P/7K w - - 0 1",
      "7k/6r1/8/8/8/8/7P/7K w - - 0 1",
      "H1 G1" );
    ( "Move piece causes check",
      "7k/7r/8/8/8/8/7N/7K w - - 0 1",
      "7k/7r/8/8/8/8/7N/7K w - - 0 1",
      "H2 F3" );
  ]

let edge_case_moves =
  [
    ( "En passant",
      "7k/8/8/3pP3/8/8/8/K7 w - d6 0 1",
      "7k/8/3P4/8/8/8/8/K7 w - - 0 1",
      "E5 D6" );
    ( "Promotion to queen",
      "7k/3P4/8/8/8/8/8/K7 w - - 0 1",
      "3Q3k/8/8/8/8/8/8/K7 w - - 0 1",
      "D7 D8" );
  ]

let game_state_tests =
  List.flatten [ game_one_tests; illegal_move_tests; edge_case_moves ]
  |> generate_move_tests

(** Construct OUnit tests for Gameplay*)
let check_test
    (name : string)
    (input : string)
    (expected_output : Gameplay.valid) : test =
  name >:: fun _ ->
  assert_equal expected_output (check input) ~printer:print_tuples

let check_error_test (name : string) (input : string) =
  name >:: fun _ ->
  assert_raises (Failure "new input needed") (fun () -> check input)

let print_time_test
    (name : string)
    (color : color)
    (state : Game_state.game_state)
    (time : int)
    (expected_output : Game_state.time) : test =
  name >:: fun _ ->
  assert_equal expected_output (print_time color state time)

let board_example =
  Game_state.get_board_from_FEN
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let gameplay_tests =
  [
    check_test "'A4 A6' is on board" "A4 A6"
      { start = { rank = 4; file = 1 }; next = { rank = 6; file = 1 } };
    check_test "'A4 B8' is on board" "A4 B8"
      { start = { rank = 4; file = 1 }; next = { rank = 8; file = 2 } };
    check_test "'a4 b8' is on board" "a4 b8"
      { start = { rank = 4; file = 1 }; next = { rank = 8; file = 2 } };
    check_error_test "'A4 A9' is not on the board, throws Failure"
      "A4 A9";
    check_error_test
      "throws Failure because J is not a letter on boardd" "A4 J1";
    check_error_test
      "throws Failure because 44 is not a number on boardd" "A4 A44";
    check_error_test
      "throws Failure because 44 is not a number on boardd" "B66 B7";
    print_time_test
      "White's turn, started with 300 and takes 10 seconds" White
      {
        board = board_example;
        white_taken = [];
        black_taken = [];
        time = (300, 300);
      }
      10 (290, 300);
    print_time_test "Black's turn, starts with 300 and takes 20 seconds"
      Black
      {
        board = board_example;
        white_taken = [];
        black_taken = [];
        time = (300, 300);
      }
      20 (300, 280);
    print_time_test "White's turn, starts with 232 and takes 20 seconds"
      White
      {
        board = board_example;
        white_taken = [];
        black_taken = [];
        time = (232, 230);
      }
      20 (212, 230);
  ]

(** Construct OUnit tests for Move_validation*)
let move_validation_tests = [ (* TODO: add your tests here *) ]

(** Construct OUnit tests for Piece*)
let piece_tests = [ (* TODO: add your tests here *) ]

(** Construct OUnit tests for Ui*)

let rec print_tup = function a, b -> Printf.sprintf "%i, %i" a b

let rec print_tuple_list = function
  | [] -> ""
  | h :: t -> print_tup h ^ print_tuple_list t

(**OUnit test for [get_moves]*)
let moves_test
    (name : string)
    (inp_piece : Game_state.piece)
    (input_pos : Piece.start)
    (expected_output : Piece.move list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Piece.get_moves inp_piece input_pos)
    ~printer:print_tuple_list

let moves_tests =
  [
    (*Pawn Tests*)
    moves_test "Pawn moves with start (4,4) - Center Case"
      Game_state.Pawn (4, 4)
      [ (4, 3); (4, 2); (5, 3); (3, 3); (4, 5); (4, 6); (3, 5); (5, 5) ];
    moves_test "Pawn moves with start (1,2) - Edge Case" Game_state.Pawn
      (1, 2)
      [ (1, 1); (2, 1); (1, 3); (1, 4); (2, 3) ];
    moves_test "Pawn moves with start (1,7) - Top Case" Game_state.Pawn
      (1, 7)
      [ (1, 6); (1, 5); (2, 6); (1, 8); (2, 8) ];
    (*Rook Tests*)
    moves_test "Rook moves with start (4,4)" Game_state.Rook (4, 4)
      [
        (*xpos*)
        (*good*)
        (5, 4);
        (6, 4);
        (7, 4);
        (8, 4);
        (*xneg*)
        (1, 4);
        (2, 4);
        (3, 4);
        (*ypos*)
        (*good*)
        (4, 5);
        (4, 6);
        (4, 7);
        (4, 8);
        (*yneg*)
        (4, 1);
        (4, 2);
        (4, 3);
      ];
    moves_test "Rook moves with start (1,1)" Game_state.Rook (1, 1)
      [
        (*xpos*)
        (2, 1);
        (3, 1);
        (4, 1);
        (5, 1);
        (6, 1);
        (7, 1);
        (8, 1);
        (*ypos*)
        (1, 2);
        (1, 3);
        (1, 4);
        (1, 5);
        (1, 6);
        (1, 7);
        (1, 8);
      ];
    (*King Tests*)
    moves_test "King moves with start (4,4)" Game_state.King (4, 4)
      [ (4, 3); (4, 5); (3, 4); (5, 4); (3, 5); (5, 5); (3, 3); (5, 3) ];
    moves_test "King moves with start (1,1)" Game_state.King (1, 1)
      [ (1, 2); (2, 1); (2, 2) ];
    (*Empty*)
    moves_test "Empty" Game_state.Empty (1, 1) [];
    (*Bishop Tests*)
    moves_test "Bishop moves with start (2,2)" Game_state.Bishop (1, 1)
      [ (2, 2); (3, 3); (4, 4); (5, 5); (6, 6); (7, 7); (8, 8) ];
    moves_test "Bishop moves with start (4,4)" Game_state.Bishop (4, 4)
      [
        (5, 3);
        (6, 2);
        (7, 1);
        (3, 3);
        (2, 2);
        (1, 1);
        (3, 5);
        (2, 6);
        (1, 7);
        (5, 5);
        (6, 6);
        (7, 7);
        (8, 8);
      ];
    (*Queen Tests*)
    moves_test "Queen moves with start (1,1)" Game_state.Queen (1, 1)
      [
        (2, 1);
        (3, 1);
        (4, 1);
        (5, 1);
        (6, 1);
        (7, 1);
        (8, 1);
        (1, 2);
        (1, 3);
        (1, 4);
        (1, 5);
        (1, 6);
        (1, 7);
        (1, 8);
        (2, 2);
        (3, 3);
        (4, 4);
        (5, 5);
        (6, 6);
        (7, 7);
        (8, 8);
      ];
    moves_test "Queen moves with start (4,4)" Game_state.Queen (4, 4)
      [
        (5, 4);
        (6, 4);
        (7, 4);
        (8, 4);
        (1, 4);
        (2, 4);
        (3, 4);
        (4, 5);
        (4, 6);
        (4, 7);
        (4, 8);
        (4, 1);
        (4, 2);
        (4, 3);
        (5, 3);
        (6, 2);
        (7, 1);
        (3, 3);
        (2, 2);
        (1, 1);
        (3, 5);
        (2, 6);
        (1, 7);
        (5, 5);
        (6, 6);
        (7, 7);
        (8, 8);
      ];
    (*Knight Tests*)
    moves_test "Kight moves with start (1,1)" Game_state.Knight (1, 1)
      [ (3, 2); (2, 3) ];
    moves_test "Knight moves with start (4,4)" Game_state.Knight (4, 4)
      [ (2, 5); (6, 3); (2, 3); (6, 5); (3, 6); (5, 2); (3, 2); (5, 6) ];
  ]

let ui_tests = moves_tests

let suite =
  "test suite for Chess"
  >::: List.flatten
         [
           game_state_tests;
           gameplay_tests;
           move_validation_tests;
           piece_tests;
           ui_tests;
         ]

let _ = run_test_tt_main suite
