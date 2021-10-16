open OUnit2
open Chess
open Game_state
open Gameplay
open Move_validation
open Piece
open Ui

(** Construct OUnit tests for Gameplay*)
let check_test
    (name : string)
    (input : string)
    (expected_output : Gameplay.move) : test =
  name >:: fun _ -> assert_equal expected_output (check input)
