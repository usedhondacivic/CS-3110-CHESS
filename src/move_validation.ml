type result =
  | Legal
  | Illegal

let ll_to_aa ll = Array.of_list (List.map Array.of_list ll)

let gboard_to_aboard gboard = ll_to_aa gboard

let friendly_fire aboard start_coard finish_coard =
  aboard.(start_coard.Game_state.rank).(start_coard.Game_state.file)
  == aboard.(finish_coard.Game_state.rank).(finish_coard.Game_state.file)

let attempt_move board start finish = failwith "not implemented"
