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

(*hopcroft*)

let initial_partition aut =
  List.fold_left
    (fun (acc1, acc2) x ->
      if List.exists (fun p -> List.compare compare p x = 0) aut.finals then
        (x :: acc1, acc2)
      else (acc1, x :: acc2))
    ([], []) aut.states
  |> fun (a, b) -> [ a; b ]

let possibles state =
  List.fold_left
    (fun ret (a, _, c) ->
      if List.compare compare state a = 0 then c :: ret else ret)
    [] automaton.transitions
  |> List.sort_uniq compare

let compare_possibles pos1 pos2 aut =
  let compare_lists l1 l2 =
    if List.compare_lengths l1 l2 <> 0 then -1
    else if List.compare compare l1 l2 = 0 then 0
    else if
      List.exists (fun x -> List.compare compare x l1 = 0) aut.finals
      && List.exists (fun x -> List.compare compare x l2 = 0) aut.finals
    then 0
    else -1
  in
  List.compare compare_lists pos1 pos2 = 0

let rec combinations l =
  match l with
  | [] -> []
  | x :: xs ->
      let f y = [ x; y ] in
      List.map f xs @ combinations xs

let split l1 aut =
  let aux = combinations l1 in
  let helper =
    List.fold_left
      (fun acc x ->
        if
          compare_possibles
            (possibles (List.hd x))
            (possibles (List.hd (List.tl x)))
            aut
        then List.sort_uniq compare x :: acc
        else acc)
      [] aux
    |> List.sort_uniq compare
  in
  if List.length helper <> 0 then
    let temp =
      List.filter (fun x -> not (List.mem x (List.concat helper))) l1
      |> List.sort_uniq compare
    in
    temp :: helper
  else
    List.fold_left (fun acc x -> [ x ] :: acc) [] l1
    |> List.filter (fun x -> List.length x <> 0)

let new_t t s =
  let new_s x =
    List.fold_left
      (fun acc y ->
        List.fold_left
          (fun acc2 z -> if List.mem y z then z @ acc2 else acc2)
          acc s)
      [] x
  in

  List.fold_left (fun acc (a, b, c) -> (new_s a, b, new_s c) :: acc) [] t
  |> List.sort_uniq compare

let concat_trans a =
  let rec aux acc = 
    function
    | [] -> acc
    | (x, _, z) :: tl ->
        let helper =
          List.filter
            (fun (a, _, c) ->
              List.compare compare x a = 0 && List.compare compare z c = 0)
            a.transitions
        in
        let new_l = List.fold_left (fun acc (_, b, _) -> b @ acc) [] helper in
        aux ((x, new_l |> List.sort_uniq compare, z) :: acc) tl
  in
  {
    initial = a.initial;
    finals = a.finals;
    alphabet = a.alphabet;
    states = a.states;
    transitions = List.rev (aux [] a.transitions) |> List.sort_uniq compare;
  }

let hopcroft h =
  let init = initial_partition h in
  let new_s =
    let rec loop p : state list list list =
      let c =
        List.fold_left (fun acc x -> split x h @ acc) [] p
        |> List.sort_uniq compare
        |> List.filter (fun x -> List.length x <> 0)
      in
      if List.compare compare c p = 0 then p else loop c
    in
    loop init
    |> List.fold_left (fun acc x -> List.concat x :: acc) []
    |> List.sort_uniq compare
  in

  {
    states = new_s;
    alphabet = h.alphabet;
    transitions = new_t h.transitions new_s;
    initial =
      List.fold_left
        (fun acc x ->
          List.fold_left
            (fun acc y ->
              List.fold_left
                (fun acc z -> if List.mem y z then z :: acc else acc)
                acc new_s)
            acc x)
        [] h.initial
      |> List.sort_uniq compare;
    finals =
      List.fold_left
        (fun acc x ->
          List.fold_left
            (fun acc y ->
              List.fold_left
                (fun acc z -> if List.mem y z then z :: acc else acc)
                acc new_s)
            acc x)
        [] h.finals
      |> List.sort_uniq compare;
  }
  |> concat_trans


