open OUnit2
open Chess
open Game_state
open Gameplay
open Move_validation
open Piece
open Ui

(** Construct OUnit tests for Game_state*)
let game_state_tests =
  failwith "game_state_tests has not been implemented"

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

let gameplay_tests = failwith "gameplay_tests has not been implemented"

(** Construct OUnit tests for Move_validation*)
let move_validation_tests =
  failwith "move_validation_tests has not been implemented"

(** Construct OUnit tests for Piece*)
let piece_tests = failwith "piece_tests has not been implemented"

(** Construct OUnit tests for Ui*)
let ui_tests = failwith "ui_tests has not been implemented"
