open Printf
open Scanf

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

let print_automaton a =
  let print_states p =
    List.iter
      (fun set ->
        List.iter (fun element -> print_int element) set;
        print_string " | ")
      p
  in

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
  in
  print_states a.states;
  print_newline ();
  print_transitions a.transitions;
  print_states a.initial;
  print_newline ();
  print_states a.finals;
  print_newline ()

let brzozowski aut =
  let inv_trans = List.map (fun (x, y, z) -> (z, y, x)) aut.transitions in
  let reach s l =
    List.fold_left
      (fun acc x ->
        List.fold_left
          (fun acc (a, b, c) ->
            if List.mem x a && List.mem l b then c @ acc else acc)
          acc inv_trans)
      [] s
    |> List.sort_uniq compare
  in
  let determinization =
    let queue =  [List.concat aut.finals] in
    let seen = [] in
    let rec calculate queue seen =
      match queue with
      | [] -> seen
      | x :: tl ->
          let reachables =
            List.fold_left (fun acc l -> reach x l :: acc) [] aut.alphabet
            |> List.sort_uniq compare
            |> List.filter (fun x -> not (List.mem x queue))
            |> List.filter (fun x -> x <> [])
          in
          if reachables <> [] then calculate (tl @ reachables) (x :: seen)
          else calculate tl (x :: seen)
    in

    calculate queue seen |> List.sort_uniq compare
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
        ( List.concat (new_s x (determinization )),
          List.sort_uniq compare ( new_l (List.concat (new_s x (determinization ))) trans),
          List.concat (new_s z (determinization )) ))
      trans
  in
  {
    states = determinization;
    (*tirar o estado vazio*)
    alphabet = aut.alphabet;
    transitions = inv_trans;
    initial = aut.initial;
    finals = aut.finals;
  }

let x = brzozowski automaton
