open Scanf
open Printf
open Set

(*default type of automaton*)
type state = int
type symbol = char
type transition = state list * symbol list * state list

type automaton_def = {
  states : state list list;
  alphabet : symbol list;
  transitions : transition list;
  initial : state list list;
  finals : state list list;
}

let automaton_def =
  let read_int_list () =
    read_line () |> String.split_on_char ' '
    |> List.map (fun s -> [ int_of_string s ])
  in
  let create_machine nTrans =
    let rec loop i acc =
      if i = 0 then List.rev acc
      else
        let str = read_line () in
        let transition =
          sscanf str " %d %c %d " (fun a b c -> ([ a ], [ b ], [ c ]))
        in
        loop (i - 1) (transition :: acc)
    in
    loop nTrans []
  in
  let calc_inter transitions finiS =
    let aux =
      List.fold_left (fun acc (a, _, c) -> a :: c :: acc) finiS transitions
    in
    List.sort_uniq compare aux
  in

  let calc_alpha transitions =
    let aux =
      List.fold_left
        (fun acc (_, b, _) ->
          let y = List.hd b in
          y :: acc)
        [] transitions
    in
    List.sort_uniq compare aux
  in

  let _nS = read_int () in
  let iniS = [ [ read_int () ] ] in
  let _nF = read_int () in
  let finiS = read_int_list () in
  let nTrans = read_int () in
  let trans = create_machine nTrans in
  let alphabet = calc_alpha trans in
  let states = calc_inter trans [] in

  { states; alphabet; transitions = trans; initial = iniS; finals = finiS }

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
    let r = Stdlib.compare x0 x1 in
    if r = 0 then
      let r1 = Stdlib.compare y0 y1 in
      if r1 = 0 then Stdlib.compare z0 z1 else r1
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

let transform_automaton (def : automaton_def) : automaton =
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
let print_automaton a =
  let print_states s =
    States.iter
      (fun x ->
        print_int x;
        print_string " ")
      s
  in
  let print_newstates set_of_sets =
    New_States.iter
      (fun set ->
        States.iter
          (fun element ->
            print_int element;
            
          )
          set;
        print_string " | ";
      )
      set_of_sets;
    print_newline ()
  
  in
  let print_labels labels =
    Labels.iter
      (fun label ->
        print_char label;
        print_string " ")
      labels
  in

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
  in
  print_newstates a.states;
  print_labels a.alphabet;
  print_newline ();
  print_transitions a.transitions;
  print_newstates (New_States.singleton a.initial);
  print_newstates a.finals

(*output*)
let hopcroft aut =
  let in_reach x trans =
    Transitions.fold
      (fun (a, _, z) acc ->
        if States.compare a x = 0 then New_States.add z acc else acc)
      trans New_States.empty
  in

  let new_s x trans states fin =
    let l = in_reach x trans in
    let result =
      New_States.fold
        (fun a acc ->
          if
            (New_States.compare l (in_reach a trans)
             = 0 (*list of possibles equivalent*)
            || New_States.subset l fin
               && New_States.subset (in_reach a trans) fin)
            && New_States.cardinal l = New_States.cardinal (in_reach a trans)
            (*are not equivalent but both have final states*)
          then States.union a acc
          else acc)
        states x
    in
    result
  in

  let new_l new_s trans =
    New_States.fold
      (fun state acc ->
        Transitions.fold
          (fun (x, y, _) labels ->
            if States.subset x state then Labels.union labels y else labels)
          trans acc)
      new_s Labels.empty
  in
  let update_transitions trans states fin =
    Transitions.fold
      (fun (x, _y, z) acc ->
        let updated_transition =
          ( new_s x trans states fin,
            new_l (New_States.singleton (new_s x trans states fin)) trans,
            new_s z trans states fin )
        in
        Transitions.add updated_transition acc)
      trans Transitions.empty
  in
  let new_states param finl trans =
    New_States.fold
      (fun x acc ->
        let result = new_s x trans param finl in
        New_States.add result acc)
      param
      New_States.empty
  
  in

  {
    states = new_states aut.states aut.finals aut.transitions;
    alphabet = aut.alphabet;
    transitions = update_transitions  aut.transitions aut.states aut.finals;
    initial = new_s (aut.initial) aut.transitions aut.states aut.finals;
    finals = New_States.singleton (new_s (New_States.choose aut.finals) aut.transitions aut.states aut.finals);

  }

  let aut = hopcroft (transform_automaton automaton_def) 
  let () =
  print_automaton aut