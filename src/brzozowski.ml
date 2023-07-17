
(*Determinization*)
let reach s l t =
  List.fold_left
    (fun acc1 x ->
      List.fold_left
        (fun acc2 (a, b, c) ->
          if List.mem x a && List.mem l b then c @ acc2 else acc2)
        acc1 t)
    [] s
  |> List.sort_uniq compare

let determinization (aut:Hop2.automaton)   =
  let queue = [ List.sort_uniq compare (List.concat aut.initial) ] in
  let seen = [] in
  let rec calculate queue seen =
    match queue with
    | [] -> seen
    | x :: tl ->
        if List.mem x seen then calculate tl seen
        else
          let reachables =
            List.fold_left
              (fun acc l -> reach x l aut.transitions :: acc)
              [] aut.alphabet
            |> List.sort_uniq compare
            |> List.filter (fun x -> not (List.mem x queue))
            |> List.filter (fun x -> x <> [])
          in
          if reachables <> [] then calculate (tl @ reachables) (x :: seen)
          else calculate tl (x :: seen)
  in

  calculate queue seen |> List.sort_uniq compare

let new_t (aut:Hop2.automaton) det =
  List.fold_left
    (fun acc x ->
      List.fold_left
        (fun acc l ->
          let aux = reach x l aut.transitions in
          if aux <> [] then (x, [ l ], aux) :: acc else acc)
        acc aut.alphabet)
    [] det

let determinization aut : Hop2.automaton =

  let s = determinization aut in

  {
    states = s;
    alphabet = aut.alphabet;
    transitions = new_t aut s;
    initial = [ List.sort_uniq compare (List.concat aut.initial) ];
    finals =
      List.fold_left
        (fun acc x ->
          List.fold_left
            (fun acc y ->
              List.fold_left
                (fun acc z -> if List.mem y z then z :: acc else acc)
                acc s)
            acc x)
        [] aut.finals
      |> List.sort_uniq compare;
  }

(*Brzozowski*)

let inv (a:Hop2.automaton)  : Hop2.automaton=
  {
    initial = a.finals;
    finals = a.initial;
    alphabet = a.alphabet;
    states = a.states;
    transitions = List.map (fun (x, y, z) -> (z, y, x)) a.transitions;
  }

let upd_aut  (a:Hop2.automaton)  : Hop2.automaton=
  let up_s =
    List.fold_left
      (fun (cont, acc1) x ->
        let cont' = cont + 1 in
        (cont', (x, [ cont ]) :: acc1))
      (0, []) a.states
  in

  let _, news = up_s in

  let get_replacement s =
    let _x, y =
      List.hd
        (List.filter
           (fun (antigo, _new) -> List.compare compare s antigo = 0)
           news)
    in
    y
  in
  let updated_transitions l =
    List.map
      (fun (start_state, transition_label, destination_state) ->
        ( get_replacement start_state,
          transition_label,
          get_replacement destination_state ))
      l
  in

  {
    initial = List.map (fun x -> get_replacement x) a.initial;
    finals = List.map (fun x -> get_replacement x) a.finals;
    alphabet = a.alphabet;
    states = List.map (fun x -> get_replacement x) a.states;
    transitions =
      updated_transitions a.transitions |> List.rev |> List.sort_uniq compare;
  }

let complete_aut  (a:Hop2.automaton)  : Hop2.automaton =
  let dead_state = [ List.length a.states ] in

  let calculate s =
    List.fold_left
      (fun acc x ->
        List.fold_left
          (fun acc2 l ->
            let aux = reach x l a.transitions in
            if List.length aux = 0 then (x, [ l ], dead_state) :: acc2 else acc2)
          acc a.alphabet)
      [] s
  in
let aux =  (dead_state,a.alphabet,dead_state ) ::calculate a.states  in

  {
    initial = a.initial;
    finals = a.finals;
    alphabet = a.alphabet;
    states = a.states;
    transitions = aux @ a.transitions |> List.sort_uniq compare;
  }

let brzozowski (aut:Hop2.automaton) :Hop2.automaton =
  let concat_trans  (a:Hop2.automaton)  : Hop2.automaton =
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

  let rec det_until_Dfa  (v:Hop2.automaton)  : Hop2.automaton =
    if
      List.length v.initial > 1
      || List.exists
           (fun (a, b, _) ->
             List.length
               (List.filter
                  (fun (x, y, _) ->
                    List.compare compare a x = 0 && List.compare compare b y = 0)
                  aut.transitions)
             > List.length aut.alphabet)
           aut.transitions
    then determinization v |> concat_trans |> det_until_Dfa
    else v
  in

  inv aut |> determinization |> upd_aut |> complete_aut  |> inv |> determinization |> concat_trans |> upd_aut  |> det_until_Dfa

