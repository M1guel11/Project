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

let hopcroft automaton =
  let new_s x trans states fin =
    let possibles state trans =
      List.fold_left
        (fun ret (a, _, c) ->
          if List.compare compare state a = 0 then c :: ret else ret)
        [] trans
    in
    let l = possibles x trans in
    let result =
      let isfinal state fin =
        List.for_all
          (fun sublist -> List.exists (fun f -> f = sublist) fin)
          state
      in
      List.fold_left
        (fun acc a ->
          if
            List.compare compare l (possibles a trans) = 0
            || isfinal l fin
               && isfinal (possibles a trans) fin
               && List.length l = List.length (possibles a trans)
          then a @ acc
          else acc)
        [] states
    in
    result
  in
  let new_states states =
    List.map
      (fun x -> new_s x automaton.transitions automaton.states automaton.finals)
      states
    |> List.sort_uniq compare
  in
  let new_l states transitions =
    let labels =
      List.fold_left
        (fun acc _state ->
          let state_labels =
            List.fold_left
              (fun acc (sources, labels, _) ->
                if List.exists (fun s -> List.mem s states) sources then
                  List.append acc labels
                else acc)
              [] transitions
          in
          state_labels @ acc)
        [] states
    in
    List.sort_uniq compare labels
  in
  let new_transitions trans states fin =
    List.map
      (fun (a, _, c) ->
        (new_s a trans states fin , new_l (new_s a trans states fin) trans, new_s c trans states fin))
      trans
    |> List.sort_uniq compare
  
in

  {
    states = new_states automaton.states;
    alphabet = automaton.alphabet;
    transitions = new_transitions automaton.transitions automaton.states automaton.finals;
    initial = new_states automaton.initial;
    finals = new_states automaton.finals;
  }

let result = hopcroft automaton
let () = print_automaton result