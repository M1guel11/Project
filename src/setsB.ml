open Printf

(*deafult type to set type*)
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
  initial : New_States.t;
  finals : New_States.t;
}

let transform_automaton (def : Hopcroft.automaton) : automaton =
  let states = List.map States.of_list def.states |> New_States.of_list in
  let alphabet = Labels.of_list def.alphabet in
  let transitions =
    List.map
      (fun (x, y, z) -> (States.of_list x, Labels.of_list y, States.of_list z))
      def.transitions
    |> Transitions.of_list
  in
  let initial = List.map States.of_list def.initial |> New_States.of_list in
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
          print_string " ")
        set;
      print_string "| ")
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
  print_newstates a.initial;
  print_newstates a.finals

(*Determinization*)
let reach s l t =
  States.fold
    (fun x acc ->
      Transitions.fold
        (fun (a, b, c) acc ->
          if States.mem x a && Labels.mem l b then States.union c acc else acc)
        t acc)
    s States.empty

let new_t d a t =
  New_States.fold
    (fun x acc ->
      Labels.fold
        (fun l acc ->
          if not (States.is_empty (reach x l t)) then
            Transitions.add (x, Labels.of_list [ l ], reach x l t) acc
          else acc)
        a acc)
    d Transitions.empty

let determinization aut =
  let queue =
    New_States.fold (fun x acc -> States.union x acc) aut.initial States.empty
    |> New_States.singleton |> New_States.elements
  in

  let seen = New_States.empty in
  let rec calculate queue seen =
    match queue with
    | [] -> seen
    | x :: tl ->
        if New_States.mem x seen then calculate tl seen
        else
          let reachables =
            Labels.fold
              (fun l acc -> New_States.add (reach x l aut.transitions) acc)
              aut.alphabet New_States.empty
          in
          if not (New_States.is_empty reachables) then
            calculate
              (tl @ New_States.elements reachables)
              (New_States.add x seen)
          else calculate tl (New_States.add x seen)
  in
  calculate queue seen |> New_States.filter (fun x -> not (States.is_empty x))

let determinization aut =
  let d = determinization aut in

  {
    states = d;
    alphabet = aut.alphabet;
    transitions = new_t d aut.alphabet aut.transitions;
    initial =
      New_States.fold
        (fun x acc ->
          New_States.filter (fun y -> States.subset x y) d
          |> New_States.union acc)
        aut.initial New_States.empty;
    finals =
      New_States.fold
        (fun x acc ->
          New_States.filter (fun y -> States.subset x y) d
          |> New_States.union acc)
        aut.finals New_States.empty;
  }

(*Brzozowski alghorithm*)

let inv a =
  {
    initial = a.finals;
    finals = a.initial;
    alphabet = a.alphabet;
    states = a.states;
    transitions = Transitions.map (fun (x, y, z) -> (z, y, x)) a.transitions;
  }

let concat_trans a =
  {
    initial = a.initial;
    finals = a.finals;
    alphabet = a.alphabet;
    states = a.states;
    transitions =
      Transitions.fold
        (fun (x, _, z) acc ->
          let h =
            Transitions.filter
              (fun (a, _, c) ->
                States.compare x a = 0 && States.compare z c = 0)
              a.transitions
          in
          let new_l =
            Transitions.fold
              (fun (_, b, _) acc -> Labels.union b acc)
              h Labels.empty
          in

          Transitions.add (x, new_l, z) acc)
        a.transitions Transitions.empty;
  }

let brzozowski a =
  let rec det_until_Dfa v =
    if New_States.cardinal v.initial > 1 then
      determinization v |> concat_trans |> det_until_Dfa
    else v
  in
  inv a |> determinization |> inv |> concat_trans |> det_until_Dfa
