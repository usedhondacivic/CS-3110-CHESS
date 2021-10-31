open OUnit2
open Chess
open Game_state
open Gameplay
open Move_validation
open Piece
open Ui

(**Print tuple for testing *)
let print_tuples = function
  | {
      start = { rank = s_r; file = f_r };
      next = { rank = n_r; file = n_f };
    } ->
      Printf.sprintf "%i, %i, %i, %i;" s_r f_r n_r n_f

(** Construct OUnit tests for Game_state*)
let game_state_tests = [ (* TODO: add your tests here *) ]

(** Construct OUnit tests for Gameplay*)
let check_test
    (name : string)
    (input : string)
    (expected_output : Gameplay.move) : test =
  name >:: fun _ ->
  assert_equal expected_output (check input) ~printer:print_tuples

let get_start_test
    (name : string)
    (input : string)
    (expected_output : Game_state.board_coord) : test =
  name >:: fun _ -> assert_equal expected_output (get_start input)

let gameplay_tests =
  [
    check_test "'A4 A5' is on board" "A4 A6"
      { start = { rank = 4; file = 1 }; next = { rank = 6; file = 1 } };
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
      [ (4, 5); (4, 6); (3, 5); (5, 5) ];
    moves_test "Pawn moves with start (1,2) - Edge Case" Game_state.Pawn
      (1, 2)
      [ (1, 3); (1, 4); (2, 3) ];
    moves_test "Pawn moves with start (1,7) - Top Case" Game_state.Pawn
      (1, 7) [ (1, 8); (2, 8) ];
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
