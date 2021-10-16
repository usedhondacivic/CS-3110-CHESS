type move = {
  start : Game_state.board_coord;
  next : Game_state.board_coord;
}
(** The type of inputted move to show start and next square*)

val check : string -> move
(** Checks if inputted move is valid and returns move if valid*)

val get_start : string -> Game_state.board_coord
(** Returns starting square (board coordinate) of piece that will be
    moved*)
