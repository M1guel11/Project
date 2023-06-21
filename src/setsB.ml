open Scanf
open Printf

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
  let transitions = List.map (fun (x, y, z) -> (States.of_list x, Labels.of_list y, States.of_list z)) def.transitions |> Transitions.of_list in
  let initial = List.map States.of_list def.initial |> List.hd in
  let finals = List.map States.of_list def.finals |> New_States.of_list in
  { states; alphabet; transitions; initial; finals }



 (*print*) 
let  print_automaton a =

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
          print_string "|")
        set;
        )
    set_of_sets;
    print_newline ();
        in
let print_labels labels =
  Labels.iter
    (fun label ->
      print_char label;
      print_string " ")
    labels;
    
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
print_newstates  (New_States.singleton a.initial);
print_newstates a.finals;;




(*output*)

let () = print_automaton (transform_automaton automaton_def)