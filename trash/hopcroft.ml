(* 

(*algorithm*)

let new_s (x:Types.automaton) =
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
  } *)
