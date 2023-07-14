open Printf

(*default type to set type*)
module States = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

module New_States = Set.Make (struct
  type t = States.t

  let compare = States.compare
end)

module Labels = Set.Make (struct
  type t = char

  let compare = Stdlib.compare
end)

module TransitionsSet = struct
  type t = New_States.elt * Labels.t * New_States.elt

  let compare a b =
    let x0, y0, z0 = a in
    let x1, y1, z1 = b in
    let r = States.compare x0 x1 in
    if r = 0 then
      let r1 = Labels.compare y0 y1 in
      if r1 = 0 then States.compare z0 z1 else r1
    else r
end

module Transitions = Set.Make (TransitionsSet)

type automaton = {
  states : New_States.t;
  alphabet : Labels.t;
  transitions : Transitions.t;
  initial : New_States.elt;
  finals : New_States.t;
}

let transform_automaton (def : Hopcroft.automaton) =
  let states = List.map States.of_list def.states |> New_States.of_list in
  let alphabet = Labels.of_list def.alphabet in
  let transitions =
    List.map
      (fun (x, y, z) -> (States.of_list x, Labels.of_list y, States.of_list z))
      def.transitions
    |> Transitions.of_list
  in
  let initial = List.map States.of_list def.initial |> List.hd in
  let finals = List.map States.of_list def.finals |> New_States.of_list in
  { states; alphabet; transitions; initial; finals }

(*print*)

let print_states s =
  States.iter
    (fun x ->
      print_int x;
      print_string " ")
    s

let print_newstates set_of_sets =
  New_States.iter
    (fun set ->
      States.iter
        (fun element ->
          print_int element;
          print_string ";")
        set;
      print_string " | ")
    set_of_sets;
  print_newline ()

let print_labels labels =
  Labels.iter
    (fun label ->
      print_char label;
      print_string " ")
    labels

let print_transitions transitions =
  Transitions.iter
    (fun (x, y, z) ->
      print_states x;
      printf "-> ";
      print_labels y;
      printf "-> ";
      print_states z;
      print_newline ())
    transitions

let print_automaton a =
  print_newstates a.states;
  print_labels a.alphabet;
  print_newline ();
  print_transitions a.transitions;
  print_newstates (New_States.singleton a.initial);
  print_newstates a.finals

(*output*)

let in_reach x aut =
  Transitions.fold
    (fun (a, _, z) acc ->
      if States.compare a x = 0 then New_States.add z acc else acc)
    aut.transitions New_States.empty

let new_s x aut =
  let l = in_reach x aut in
  let result =
    New_States.fold
      (fun a acc ->
        if
          (New_States.compare l (in_reach a aut)
           = 0 (*list of possibles equivalent*)
          || New_States.subset l aut.finals
             && New_States.subset (in_reach a aut) aut.finals)
          && New_States.cardinal l = New_States.cardinal (in_reach a aut)
          (*are not equivalent but both have final states*)
        then States.union a acc
        else acc)
      aut.states x
  in
  result

let update_transitions aut =
  let find_labels ini dest =
    Transitions.fold
      (fun (x, y, z) acc ->
        if States.subset x ini && States.subset z dest then Labels.union y acc
        else acc)
      aut.transitions Labels.empty
  in
  Transitions.fold
    (fun (x, _, z) acc ->
      let ni = new_s x aut in
      let nd = new_s z aut in
      let auz = (ni, find_labels ni nd, nd) in
      Transitions.add auz acc)
    aut.transitions Transitions.empty

let new_states param aut =
  New_States.fold
    (fun x acc ->
      let result = new_s x aut in
      New_States.add result acc)
    param New_States.empty

let hopcroft aut =
  {
    states = new_states aut.states aut;
    alphabet = aut.alphabet;
    transitions = update_transitions aut;
    initial = new_s aut.initial aut;
    finals =
      New_States.fold
        (fun x acc -> New_States.add (new_s x aut) acc)
        aut.finals New_States.empty;
  }
