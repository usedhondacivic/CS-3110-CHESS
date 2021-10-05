type move = {
  start : Game_state.board_coord;
  next : Game_state.board_coord;
}

val check : string -> move
