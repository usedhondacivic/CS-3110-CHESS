type valid = {
  start : Game_state.board_coord;
  next : Game_state.board_coord;
}

type move =
  | Valid of valid
  | End

(** The type of inputted move to show start and next square*)

val check : string -> valid
(** Checks if inputted move is valid (square on board) and returns move
    if valid and raises Failure if invalid move*)

val get_start : string -> Game_state.board_coord
(** Returns starting square (board coordinate) of piece that will be
    moved*)

val take_move : string -> move
(** Asks for user's input and checks the user's input, returns user's
    input as type move if valid input but if invalid asks again for new
    input until correct*)

val print_time :
  Game_state.color -> Game_state.game_state -> int -> Game_state.time
(** Returns new time of both players based on how long it took for one
    player to move*)
