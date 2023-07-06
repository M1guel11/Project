

(*prints*)

let print_automaton (a: Hopcroft.automaton) =
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

let determinization (aut : Hopcroft.automaton)  : Hopcroft.automaton=
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
    let queue = List.sort_uniq compare [ List.concat aut.initial ]  in
    let seen = [] in
    let rec calculate queue seen =
      match queue with
      | [] -> seen
      | x :: tl ->
        if List.mem x seen then
          calculate tl seen
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

let brzozowski (aut : Hopcroft.automaton) : Hopcroft.automaton =
  let inv (a:Hopcroft.automaton) : Hopcroft.automaton =
    {
      initial = a.finals;
      finals = a.initial;
      alphabet = a.alphabet;
      states = a.states;
      transitions = List.map (fun (x, y, z) -> (z, y, x)) a.transitions;
    }
  in
  let concat_trans (a:Hopcroft.automaton) : Hopcroft.automaton  =
    let rec aux acc = function
      | [] -> acc
      | (x, _, z) :: tl -> let helper = List.filter (fun (a, _, c) -> List.compare  compare x a = 0 && List.compare compare z c =0) a.transitions in 
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

  inv aut |> determinization |> inv |> concat_trans


