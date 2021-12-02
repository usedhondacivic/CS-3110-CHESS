(** [Ui] handles displaying the board graphics and various screen in the
    terminal *)

val update_display : Game_state.game_state -> unit
(** [update_display] updates the display to show a new board *)

val show_start : unit
(** [show_start] displays the title screen of the game *)

val show_end : unit
(** [show_end] displays the closing screen of the game *)

val print_color : int -> int -> string -> unit
(** [print_color] adds the relevant ANSI strings to display text with
    the given color and background.*)
