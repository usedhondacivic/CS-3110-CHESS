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

(**OUnit test for [get_moves]*)
let moves_test
    (name : string)
    (inp_piece : Game_state.piece)
    (input_pos : Piece.start)
    (expected_output : Piece.move list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Piece.get_moves inp_piece input_pos)

let moves_tests =
  [
    moves_test "Pawn moves with start (4,4)" Game_state.Pawn (4, 4)
      [ (4, 5); (4, 6); (5, 5); (3, 5) ];
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
