open Scanf
open Printf

let em_par a b c = (([ a ], [ b ]), [ c ])

let rec remove_duplicates l =
  let rec contains l n =
    match l with [] -> false | h :: t -> h = n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
      let acc = remove_duplicates t in
      if contains acc h then acc else h :: acc

let rec sublist sub lst =
  match (sub, lst) with
  | [], _ -> true
  | _, [] -> false
  | x :: xs, y :: ys -> if x = y then sublist xs ys else sublist sub ys

let is_sublist sub lst =
  let rec check_sublist sub lst =
    match (sub, lst) with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys ->
        if x = y then check_sublist xs ys else check_sublist sub ys
  in
  let sub_length = List.length sub in
  let rec check_lists lists =
    match lists with
    | [] -> false
    | hd :: tl ->
        if List.length hd >= sub_length && check_sublist sub hd then true
        else check_lists tl
  in
  check_lists lst

let print_list_of_lists l =
  List.iter
    (fun sublist ->
      List.iter (fun elem -> print_int elem) sublist;
      print_string ";")
    l

let print_trans l =
  List.iter
    (fun ((l1, l2), l3) ->
      List.iter (fun x -> print_int x) l1;
      print_string " ";
      List.iter (fun x -> print_char x) l2;
      print_string " ";
      List.iter (fun x -> print_int x) l3;
      print_newline ())
    l

let create_maquina c =
  let i = ref 0 in
  let input = ref [] in
  while !i < c do
    let str = read_line () in
    input := sscanf str " %d %c %d " em_par :: !input;
    i := !i + 1
  done;
  !input

let read_int_list () =
  List.map (fun x -> int_of_string x) (read_line () |> String.split_on_char ' ')

(*let read_automata () =*)
let _nEst = read_int ()
let _init = read_int ()
let initl = read_int_list ()
let _nfinl = read_int ()
let finl = read_int_list ()
let ntrans = read_int ()
let trans = create_maquina ntrans

(*new Part*)

let rec new_finl new_states finl ret =
  let a, b = ret in
  match new_states with
  | [] -> ret
  | hd :: tl ->
      if sublist hd finl then new_finl tl finl (a, hd :: b)
      else new_finl tl finl (hd :: a, b)

let rec possiveis label transicoes ret =
  match transicoes with
  | ((a, b), c) :: tl ->
      if List.mem label a then possiveis label tl (ret @ [ (b, c) ])
      else possiveis label tl ret
  | [] -> List.sort compare ret

let compare_lists_possibles l1 l2 finl =
  let compare_possibles poss1 poss2 finl =
    let a, b = poss1 in
    let x, y = poss2 in
    (List.compare compare a x == 0 && List.compare compare b y == 0)
    || (List.compare compare a x == 0 && sublist b finl && sublist y finl)
  in
  if List.length l1 = List.length l2 then
    List.fold_left2
      (fun acc x y -> if acc = false then false else compare_possibles x y finl)
      true (List.sort compare l1) (List.sort compare l2)
  else false

let rec combinacoes lista ret =
  let rec comb lista ret x =
    match lista with [] -> ret | y :: tl -> comb tl (ret @ [ (x, y) ]) x
  in
  match lista with [] -> ret | x :: tl -> combinacoes tl (ret @ comb tl [] x)

let rec combinacoes_to_state list ret finl transicoes =
  match list with
  | (a, b) :: tl ->
      if
        compare_lists_possibles
          (possiveis a transicoes [])
          (possiveis b transicoes [])
          finl
      then combinacoes_to_state tl ([ a; b ] :: ret) finl transicoes
      else if is_sublist [ a ] ret || is_sublist [ b ] ret then
        combinacoes_to_state tl ret finl transicoes
      else combinacoes_to_state tl (ret @ [ [ a ]; [ b ] ]) finl transicoes
  | [] -> List.sort compare (remove_duplicates ret)

let new_transitions trans states =
  (*Melhorar funcao*)
  let rec return_new_state state new_states =
    match new_states with
    | [] -> []
    | hd :: tl -> if sublist state hd then hd else return_new_state state tl
  in
  let rec return_new_label ini dest trans ret =
    match trans with
    | [] -> remove_duplicates (List.sort compare ret)
    | ((a, b), c) :: tl ->
        if sublist a ini && sublist c dest then
          return_new_label ini dest tl (b @ ret)
        else return_new_label ini dest tl ret
  in

  let rec update_trans trans new_states ret trans_aux =
    match trans with
    | [] -> ret
    | x :: tl ->
        let (a, _), c = x in
        update_trans tl new_states
          (( ( return_new_state a new_states,
               return_new_label
                 (return_new_state a new_states)
                 (return_new_state c new_states)
                 trans_aux [] ),
             return_new_state c new_states )
          :: ret)
          trans_aux
  in

  remove_duplicates (update_trans trans states [] trans)

(*output
  num estados
  num estados ini
  lista estados ini
  num estados fin
  lista estados fin
  lista transicoes(ini->label->destino)
*)
let new_states =
  combinacoes_to_state (combinacoes (initl @ finl) []) [] finl trans

let new_trans = new_transitions trans new_states
let new_order = new_finl new_states finl ([], [])
let new_initl, new_finl = new_order

let _print_result =
  print_string "-------------OUTPUT---------------\n";
  printf "%d\n" (List.length new_states);
  print_list_of_lists (List.sort compare new_initl);
  print_newline ();
  printf "%d\n" (List.length new_initl);
  print_list_of_lists (List.sort compare new_finl);
  print_newline ();
  print_trans new_trans
