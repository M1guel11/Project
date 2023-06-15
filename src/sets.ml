open Scanf
open Printf
open Set

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

let create_maquina c =
  let i = ref 0 in
  let transitions = ref Transitions.empty in
  while !i < c do
    let line = read_line () in
    let str =
      sscanf line "%d %c %d" (fun a b c ->
          (States.of_list [ a ], Labels.of_list [ b ], States.of_list [ c ]))
    in
    transitions := Transitions.add str !transitions;
    i := !i + 1
  done;
  !transitions
  

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
      print_newline ())
    set_of_sets

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
      print_labels y;
      print_states z;
      print_newline ())
    transitions

let read_states () =
  read_line () |> String.split_on_char ' '
  |> List.map (fun x -> [ int_of_string x ])
  |> List.map States.of_list |> New_States.of_list

let in_reach x trans =
  Transitions.fold
    (fun (a, _, z) acc ->
      if States.compare a x = 0 then New_States.add z acc else acc)
    trans New_States.empty

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

let new_l new_s trans =
  New_States.fold
    (fun state acc ->
      Transitions.fold
        (fun (x, y, _) labels ->
          if States.subset x state then Labels.union labels y else labels)
        trans acc)
    new_s Labels.empty

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

(*let read_automata () =*)
let _nEst = read_int ()
let _init = read_int ()
let initl = read_states ()
let _nfinl = read_int ()
let finl = read_states ()
let ntrans = read_int ()
let trans = create_maquina ntrans
let () = print_transitions trans

(*Output*)
let new_states param finl =
  New_States.fold
    (fun x acc ->
      let result = new_s x trans param finl in
      New_States.add result acc)
    param New_States.empty

let uu = new_states (New_States.union initl finl) finl

let () =
  print_transitions
    (update_transitions trans (New_States.union initl finl) finl)
