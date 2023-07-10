open Scanf

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

(*prints*)

let print_automaton (a : automaton_def) =
  let print_states p =
    List.iter
      (fun set ->
        List.iter
          (fun element ->
            print_int element;
            print_string "; ")
          set;
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

let determinization (aut : automaton_def) : automaton_def =
  let reach s l =
    List.fold_left
      (fun acc x ->
        List.fold_left
          (fun acc (a, b, c) ->
            if List.mem x a && List.mem l b then c @ acc else acc)
          acc aut.transitions)
      [] s
    |> List.sort_uniq compare
  in
  let determinization =
    let queue = [ List.sort_uniq compare (List.concat aut.initial) ] in
    let seen = [] in
    let rec calculate queue seen =
      match queue with
      | [] -> seen
      | x :: tl ->
          if List.mem x seen then calculate tl seen
          else
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
  let new_t =
    List.fold_left
      (fun acc x ->
        List.fold_left
          (fun acc l ->
            if reach x l <> [] then (x, [ l ], reach x l) :: acc else acc)
          acc aut.alphabet)
      [] determinization
  in

  {
    states = determinization;
    alphabet = aut.alphabet;
    transitions = new_t;
    initial =
      List.fold_left
        (fun acc x ->
          List.fold_left
            (fun acc y ->
              List.fold_left
                (fun acc z -> if List.mem y z then z :: acc else acc)
                acc determinization)
            acc x)
        [] aut.initial
      |> List.sort_uniq compare;
    finals =
      List.fold_left
        (fun acc x ->
          List.fold_left
            (fun acc y ->
              List.fold_left
                (fun acc z -> if List.mem y z then z :: acc else acc)
                acc determinization)
            acc x)
        [] aut.finals
      |> List.sort_uniq compare;
  }

let brzozowski (aut : automaton_def) : automaton_def =
  let inv (a : automaton_def) : automaton_def =
    {
      initial = a.finals;
      finals = a.initial;
      alphabet = a.alphabet;
      states = a.states;
      transitions = List.map (fun (x, y, z) -> (z, y, x)) a.transitions;
    }
  in
  let concat_trans (a : automaton_def) : automaton_def =
    let rec aux acc = function
      | [] -> acc
      | (x, _, z) :: tl ->
          let helper =
            List.filter
              (fun (a, _, c) ->
                List.compare compare x a = 0 && List.compare compare z c = 0)
              a.transitions
          in
          let new_l = List.fold_left (fun acc (_, b, _) -> b @ acc) [] helper in
          aux ((x, new_l, z) :: acc) tl
    in
    {
      initial = a.initial;
      finals = a.finals;
      alphabet = a.alphabet;
      states = a.states;
      transitions = List.rev (aux [] a.transitions) |> List.sort_uniq compare;
    }
  in

  let rec det_until_Dfa (v : automaton_def) : automaton_def =
    if List.length v.initial > 1 then determinization v |> concat_trans |> det_until_Dfa else v
  in

  inv aut |> determinization |> inv |> concat_trans|> det_until_Dfa

let x = automaton_def
let k = brzozowski automaton_def 

