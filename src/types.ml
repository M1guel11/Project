module type Automata = sig
  type 'a t
  type state
  type label
  type transition = state * label * state

  val print_transition : transition -> unit
  val print : state t -> state t -> transition list -> unit
end

(* module DFA (M : Automata) = struct


   end *)
