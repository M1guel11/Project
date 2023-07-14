open Printf
open Scanf

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

let automaton =
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
  let iniS = read_int_list () in
  let _nF = read_int () in
  let finiS = read_int_list () in
  let nTrans = read_int () in
  let trans = create_machine nTrans in
  let alphabet = calc_alpha trans in
  let states = calc_inter trans [] in

  { states; alphabet; transitions = trans; initial = iniS; finals = finiS }

(*default type to set type*)
module States = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

module New_States = Set.Make (struct
  type t = States.t

  let compare = States.compare
end)

module Partition = Set.Make (struct
  type t = New_States.t

  let compare = New_States.compare
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

let print_partition p =
  Partition.iter
    (fun nset ->
      New_States.iter
        (fun set ->
          States.iter
            (fun element ->
              print_int element;
              print_string ";")
            set;
          print_string " | ")
        nset;
      print_string " // ")
    p;
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

let initial_partition aut =
  New_States.fold
    (fun x (acc1, acc2) ->
      if New_States.exists (fun p -> States.compare p x = 0) aut.finals then
        (New_States.add x acc1, acc2)
      else (acc1, New_States.add x acc2))
    aut.states
    (New_States.empty, New_States.empty)
  |> fun (a, b) -> Partition.singleton a |> Partition.add b

let possibles state t =
  Transitions.fold
    (fun (a, _, c) ret ->
      if States.subset a state  then New_States.add c ret else ret)
    t New_States.empty

let compare_possibles pos1 pos2 aut =
  let compare_lists l1 l2 =
    if New_States.cardinal l1 <> New_States.cardinal l2 then -1
    else if New_States.compare l1 l2 = 0 then 0
    else if
      New_States.exists
        (fun x -> New_States.subset (New_States.singleton x) l1)
        aut.finals
      && New_States.exists
           (fun x -> New_States.subset (New_States.singleton x) l2)
           aut.finals
    then 0
    else -1
  in
  compare_lists pos1 pos2 = 0

let combinations set =
  let elements = New_States.elements set in
  let rec combine acc = function
    | [] -> acc
    | x :: xs ->
        let combined = List.map (fun y -> States.union x y) xs in
        let acc' = New_States.of_list combined in
        combine (New_States.union acc' acc) xs
  in
  combine New_States.empty elements





let split l1 aut =
  let aux = combinations l1 in
  let split_s state =
    let state1, state2 = 
    States.fold (fun elem (s1, s2) ->
       ( States.add s1 s,  States.add elem s2)
    ) state (States.empty, States.empty)
    in
    state1, state2
  in
  let helper =  New_States.filter(fun x -> let (a,b) = split_s x in  compare_possibles (possibles a aut.transitions) (possibles b aut.transitions) aut  )aux in let ()= print_newstates helper;
in
  if New_States.is_empty helper then New_States.fold (fun  x acc  -> let (a,b) = split_s x in New_States.add a acc |> New_States.add b   )l1 New_States.empty    
  else 
    let temp =
     New_States.filter (fun x -> New_States.mem x aux    ) l1 in
     temp
     
      



let h = transform_automaton automaton
let p = initial_partition h
let k = Partition.fold (fun x  acc -> New_States.union (split x h) acc )  p New_States.empty



