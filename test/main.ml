open OUnit2
open Chess
open Game_state
open Gameplay
open Move_validation
open Piece
open Ui

(** Construct OUnit tests for Game_state*)
let game_state_tests = [ (* TODO: add your tests here *) ]

(** Construct OUnit tests for Gameplay*)
let check_test
    (name : string)
    (input : string)
    (expected_output : Gameplay.move) : test =
  name >:: fun _ -> assert_equal expected_output (check input)

let get_start_test
    (name : string)
    (input : string)
    (expected_output : Game_state.board_coord) : test =
  name >:: fun _ -> assert_equal expected_output (get_start input)

let gameplay_tests =
  [
    check_test "'A4 A5' is on board" "A4 A6"
      { start = { rank = 0; file = 4 }; next = { rank = 0; file = 6 } };
  ]

(** Construct OUnit tests for Move_validation*)
let move_validation_tests = [ (* TODO: add your tests here *) ]

(** Construct OUnit tests for Piece*)
let piece_tests = [ (* TODO: add your tests here *) ]

(** Construct OUnit tests for Ui*)
let ui_tests = [ (* TODO: add your tests here *) ]

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