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
  let iniS = [ [ read_int () ] ] in
  let _nF = read_int () in
  let finiS = read_int_list () in
  let nTrans = read_int () in
  let trans = create_machine nTrans in
  let alphabet = calc_alpha trans in
  let states = calc_inter trans [] in

  { states; alphabet; transitions = trans; initial = iniS; finals = finiS }

(*prints*)
let print_states a =
  a
  |> List.iter (fun x ->
         List.iter
           (fun y ->
             print_int y;
             print_string " ")
           x;
         print_newline ())

let print_transitions a =
  a
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

let print_automaton a =
  print_states a.states;
  print_transitions a.transitions;
  print_states a.initial;
  print_states a.finals;
  print_newline ()

let alghorithm a =
  let inv_trans trans = List.map (fun (x, y, z) -> (z, y, x)) trans in
  let reach states trans =
    let rec helper _ state =
      List.fold_left
        (fun acc (x, _, z) -> if List.mem state x then z @ acc else acc)
        [] trans
    in
    List.concat_map (helper []) states
  in
  let determinization a =
    let trans = inv_trans a.transitions in
    let queue = [ List.concat a.finals ] in
    let seen = [] in
    let rec calculate queue seen =
      match queue with
      | [] -> seen
      | x :: tl ->
          if List.mem (List.sort_uniq compare (reach x trans)) queue then
            calculate tl (x :: seen)
          else
            calculate (List.sort_uniq compare (reach x trans) :: tl) (x :: seen)
    in
    calculate queue seen
  in

  let new_s element lst =
    List.filter
      (fun sublist -> List.exists (fun x -> List.mem x sublist) element)
      lst
  in
  let new_l s trans =
    List.fold_right
      (fun (src, labels, _) acc ->
        if List.exists (fun x -> List.mem x src) s then labels @ acc else acc)
      trans []
  in

  let new_t trans =
    List.map
      (fun (x, _, z) ->
        ( List.concat (new_s x (determinization a)),
          List.sort_uniq compare ( new_l (List.concat (new_s x (determinization a))) trans),
          List.concat (new_s z (determinization a)) ))
      trans
  in

  {
    states = List.filter (fun sublist -> sublist <> []) (determinization a); (*tirar o estado vazio*)
    alphabet = a.alphabet;
    transitions =  List.sort_uniq compare  (new_t a.transitions);
    initial = new_s (List.concat a.initial) (determinization a);
    finals = new_s (List.concat a.finals) (determinization a);
  }

let automaton = alghorithm automaton
