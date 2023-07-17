open Printf

(*default type to set type*)
module States = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

module New_States = Set.Make (struct
  type t = States.t


  let compare = compare
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
  initial : New_States.t;
  finals : New_States.t;
}

let transform_automaton (def : Hop2.automaton) : automaton =
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
  print_newstates a.initial;
  print_newstates a.finals

 (*auxiliary functions*) 
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


   (*hopcroft*) 
let initial_partition aut =
  New_States.fold
    (fun x (acc1, acc2) ->
      if New_States.exists (fun p -> States.compare p x = 0) aut.finals then
        (New_States.add x acc1, acc2)
      else (acc1, New_States.add x acc2))
    aut.states
    (New_States.empty, New_States.empty)
  |> fun (a, b) -> Partition.singleton a |> Partition.add b

let possibles state l t =
  Transitions.fold
    (fun (a, b, c) ret ->
      if States.subset a state && Labels.subset l b then New_States.add c ret
      else ret)
    t New_States.empty

let compare_two_possibles l1 l2 aut =
  if New_States.cardinal l1 <> New_States.cardinal l2 then -1
  else if New_States.compare l1 l2 = 0 then 0
  else
    let new_l1 =
      New_States.filter (fun x -> not (New_States.mem x aut.finals)) l1
    in
    let new_l2 =
      New_States.filter (fun x -> not (New_States.mem x aut.finals)) l2
    in
    New_States.compare new_l1 new_l2

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

let same_partition p1 p2 part =
  let get_partition k =
    Partition.filter (fun x -> New_States.subset k x) part
  in
  Partition.compare (get_partition p1) (get_partition p2) = 0

let split l1 aut part =
  let aux = combinations l1 in
  let split_s state =
    let state1, state2 =
      ( state |> States.elements |> List.hd |> States.singleton,
        state |> States.elements |> List.tl |> List.hd |> States.singleton )
    in
    (state1, state2)
  in
  let helper =
    New_States.filter
      (fun x ->
        let a, b = split_s x in
        Labels.for_all
          (fun l ->
            let possibles_a =
              possibles a (Labels.singleton l) aut.transitions
            in
            let possibles_b =
              possibles b (Labels.singleton l) aut.transitions
            in
            compare_two_possibles possibles_a possibles_b aut = 0)
          aut.alphabet
        && Labels.for_all
             (fun l ->
               let possibles_a =
                 possibles a (Labels.singleton l) aut.transitions
               in
               let possibles_b =
                 possibles b (Labels.singleton l) aut.transitions
               in
               same_partition possibles_a possibles_b part)
             aut.alphabet)
      aux
    (*in
      let () = print_string "helper-> " ;print_newstates helper*)
  in
  Partition.empty |> Partition.add helper

let hopcroft h =
  let init = initial_partition h in
  let new_s =
    let rec loop p =
      let c =
        Partition.fold
          (fun x acc -> Partition.union (split x h p) acc)
          init Partition.empty
      in
      let aux =
        Partition.fold
          (fun x acc ->
            New_States.fold
              (fun y acc2 ->
                States.fold
                  (fun z acc3 -> New_States.add (States.of_list [ z ]) acc3)
                  y acc2)
              x acc)
          c New_States.empty
      in
      let missing = New_States.diff h.states aux in
      let new_c = Partition.add missing c in
      (*let () = print_string "new_c-> " ;print_partition c  in*)
      if Partition.compare new_c p = 0 then p else loop new_c
    in
    loop init
  in

  let ns =
    Partition.fold (fun s acc -> New_States.union s acc) new_s New_States.empty
  in
  let new_state x new_s =
    let aux = New_States.filter (fun y -> States.subset x y) new_s in
    New_States.fold (fun y acc -> States.union y acc) aux States.empty
  in
  let update_transitions t new_s =
    Transitions.fold
      (fun (a, b, c) acc ->
        let new_a = new_state a new_s in
        let new_c = new_state c new_s in
        let new_b = b in
        Transitions.add (new_a, new_b, new_c) acc)
      t Transitions.empty
  in

  {
    states = ns;
    alphabet = h.alphabet;
    transitions = update_transitions h.transitions ns;
    initial =
      New_States.fold
        (fun x acc -> New_States.add (new_state x ns) acc)
        h.initial New_States.empty;
    finals =
      New_States.fold
        (fun x acc -> New_States.add (new_state x ns) acc)
        h.finals New_States.empty;
  } |> concat_trans

