(** Ui handles displaying the board in the terminal *)

(** [update_display] updates the display to show a new board *)
val update_display : Game_state.game_state -> unit

val show_start : unit

val print_color : int -> int -> string -> unit