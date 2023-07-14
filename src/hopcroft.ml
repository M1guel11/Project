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
  let iniS = read_int_list () in
  let _nF = read_int () in
  let finiS = read_int_list () in
  let nTrans = read_int () in
  let trans = create_machine nTrans in
  let alphabet = calc_alpha trans in
  let states = calc_inter trans [] in

  { states; alphabet; transitions = trans; initial = iniS; finals = finiS }

(*prints*)
let print_states p =
  List.iter
    (fun set ->
      List.iter
        (fun element ->
          print_int element;
          print_string ";")
        set;
      print_string " | ")
    p

let print_labels labels =
  List.iter
    (fun label ->
      print_char label;
      print_string " ")
    labels

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

let print_automaton a =
  print_states a.states;
  print_newline ();
  print_labels a.alphabet;
  print_newline ();
  print_transitions a.transitions;
  print_states a.initial;
  print_newline ();
  print_states a.finals;
  print_newline ()

(*algorithm*)

let new_s x =
  let possibles state =
    List.fold_left
      (fun ret (a, _, c) ->
        if List.compare compare state a = 0 then c :: ret else ret)
      [] automaton.transitions
  in
  let l = possibles x in
  let result =
    let isfinal state =
      List.for_all
        (fun sublist -> List.exists (fun f -> f = sublist) automaton.finals)
        state
    in
    List.fold_left
      (fun acc a ->
        if
          List.compare compare l (possibles a) = 0
          || isfinal l
             && isfinal (possibles a)
             && List.length l = List.length (possibles a)
        then a @ acc
        else acc)
      [] automaton.states
  in
  result |> List.sort_uniq compare

let new_states states =
  List.map (fun x -> new_s x) states |> List.sort_uniq compare

let new_transitions =
  let new_l s d =
    List.fold_left
      (fun acc (x, y, z) ->
        if List.compare compare x s = 0 && List.compare compare z d = 0 then
          y @ acc
        else acc)
      [] automaton.transitions
  in
  List.fold_left
    (fun acc x ->
      List.fold_left
        (fun acc2 y ->
          let aux = (new_s x, List.sort_uniq compare (new_l x y), new_s y) in
          if List.length (new_l x y) = 0 then acc2
          else
            match
              List.find_opt
                (fun (a, _, c) ->
                  List.compare compare (new_s x) a = 0
                  && List.compare compare c (new_s y) = 0)
                acc2
            with
            | Some (k, w, t) ->
                List.filter
                  (fun (a, _, c) ->
                    not
                      (List.compare compare (new_s x) a = 0
                      && List.compare compare c (new_s y) = 0))
                  acc2
                |> fun filtered_acc2 ->
                (k, List.sort_uniq compare (w @ new_l x y), t) :: filtered_acc2
            | None -> aux :: acc2)
        acc automaton.states)
    [] automaton.states

let hopcroft automaton =
  let n_s = new_states automaton.states in

  {
    states = n_s;
    alphabet = automaton.alphabet;
    transitions = List.sort_uniq compare new_transitions;
    initial = new_states automaton.initial;
    finals = new_states automaton.finals;
  }
