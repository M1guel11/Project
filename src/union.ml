
open Scanf
open Printf
open Set

(*input*)

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

(*UnionFind Implementation*)
module UnionFind = struct
  type 'a union_find = {
    parent : 'a union_find option ref;
    rank : int ref;
    value : 'a;
  }

  let make x = { parent = ref None; rank = ref 0; value = x }

  let rec find x =
    match !(x.parent) with
    | None -> x
    | Some p ->
        let root = find p in
        x.parent := Some root;
        root

  let union x y =
    let root_x = find x in
    let root_y = find y in
    if root_x == root_y then ()
    else if !(root_x.rank) < !(root_y.rank) then root_x.parent := Some root_y
    else if !(root_x.rank) > !(root_y.rank) then root_y.parent := Some root_x
    else (
      root_y.parent := Some root_x;
      root_x.rank := !(root_x.rank) + 1)
end

(*input*)

type automaton = {
  states : int UnionFind.union_find list;
  alphabet : char list;
  transitions : (int list * char list * int list) list;
  initial : int UnionFind.union_find;
  finals : int UnionFind.union_find list;
}

let transform_to_union_find (automaton : automaton_def) : automaton =
  let initial = UnionFind.make (List.hd (List.concat automaton.initial)) in
  let possibles s =
    List.fold_right
      (fun (x, _, z) acc -> if List.mem s x then z @ acc else acc)
      automaton.transitions []
  in
  let result aux =
    let _ =
      List.fold_left
        (fun _ x ->
          let node = UnionFind.make x in
          let others = possibles x in
          let _ =
            List.fold_left
              (fun _ y ->
                let l = UnionFind.make y in
                UnionFind.union node l;
                aux := l :: !aux;
                ())
              () others
          in
          aux)
        aux
        (List.concat automaton.states)
    in
    List.sort
      (fun (x : int UnionFind.union_find) (y : int UnionFind.union_find) ->
        compare x.value y.value)
      !aux
  in

  let finals =
    List.fold_right
      (fun (x : int UnionFind.union_find) aux ->
        if List.mem x.value (List.concat automaton.finals) then x :: aux
        else aux)
      (result (ref [ initial ]))
      []
  in

  {
    states = result (ref [ initial ]);
    alphabet = automaton.alphabet;
    transitions = automaton.transitions;
    initial;
    finals;
  }

(*print*)
let print_automaton automaton =
  let rec print_states (s : int UnionFind.union_find list) =
    match s with
    | [] -> ()
    | x :: tl ->
        print_int x.value;
        print_string " | ";
        print_states tl
  in
  let print_labels labels =
    List.iter
      (fun label ->
        print_char label;
        print_string " ")
      labels
  in

  let print_transitions t =
    t
    |> List.iter (fun (a, b, c) ->
           List.iter
             (fun x ->
               print_int x;
               print_string " ")
             a;
           print_string "-> ";
           List.iter
             (fun x ->
               print_char x;
               print_string " ")
             b;
           print_string "-> ";
           List.iter
             (fun x ->
               print_int x;
               print_string " ")
             c;
           print_newline ())
  in

  print_states automaton.states;
  print_newline ();
  print_labels automaton.alphabet;
  print_newline ();
  print_transitions automaton.transitions;
  print_int automaton.initial.value;
  print_newline ();
  print_states automaton.finals;
  print_newline ()

(*hopcroft*)
let hopcroft a =
  let cmp_successors s1 s2 =
    let get_successors (s : int UnionFind.union_find) =
      List.fold_left
        (fun acc x ->
          if (UnionFind.find x).value = s.value then x.value :: acc else acc)
        [] a.states
    in
    let c1 = get_successors s1 in
    let c2 = get_successors s2 in
    let f =
      List.fold_left
        (fun acc (x : int UnionFind.union_find) -> x.value :: acc)
        [] a.finals
    in
    let isfinal state =
      List.for_all (fun sublist -> List.exists (fun f -> f = sublist) f) state
    in
    List.compare compare c1 c2 = 0
    || (isfinal c1 && isfinal c2 && List.length c1 = List.length c2)
  in
  let new_l s =
    let l =
      List.fold_left
        (fun acc (x, y, _) -> if x = s then y @ acc else acc)
        [] a.transitions
    in
    List.sort_uniq compare l
  in

  let rec new_states l aux rm  =
    match l with
    | [] -> aux
    | x :: tl ->
        if List.exists (fun y -> cmp_successors x y) tl then (
          let y = List.find (fun y -> cmp_successors x y) tl in
          let new_rm = y :: rm in
          let new_p = UnionFind.make (UnionFind.find x).value in
          let new_x = UnionFind.make x.value in
          UnionFind.union new_p new_x;

          let new_aux = new_x :: aux in
          new_states tl new_aux new_rm )
        else if List.mem x rm then new_states tl aux rm 
        else new_states tl (x :: aux) rm 
  in
  let s= new_states a.states [] []  
in
  {
    states = s;
    alphabet = a.alphabet;
    transitions = a.transitions;
    initial = a.initial;
    finals = a.finals;
  }

(*output*)

let a = hopcroft (transform_to_union_find automaton_def)
let () = print_automaton a
