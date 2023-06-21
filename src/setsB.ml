open Scanf
open Printf

type state = int
type symbol = char
type transition = state list * symbol list * state list

type automaton = {
  states : state list list;
  alphabet : symbol list;
  transitions : transition list;
  initial : state list list;
  finals : state list list;
}