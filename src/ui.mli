(** Ui handles displaying the board in the terminal *)

(** [update_display] updates the display to show a new board *)
val update_display : Game_state.game_state -> unit

(** [show_start] displays the title screen of the game *)
val show_start : unit

(** [print_color] adds the relevant ANSI strings to display string with the passed color and background.*)
val print_color : int -> int -> string -> unit