type board_coord = {
  rank : int;
  file : int;
}

type move = {
  start : board_coord;
  next : board_coord;
}

val check : string -> move
