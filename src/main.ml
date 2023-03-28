open Scanf
open Printf

let em_par a b c = ((a, b), c)
let em_par_2 a b c = (([ a ], [ b ]), [ c ])

let rec remove_duplicates l =
  let rec contains l n =
    match l with [] -> false | h :: t -> h = n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
      let acc = remove_duplicates t in
      if contains acc h then acc else h :: acc

let create_maquina c =
  let i = ref 0 in
  let input = ref [] in
  while !i < c do
    let str = read_line () in
    input := sscanf str " %d %c %d " em_par :: !input;
    i := !i + 1
  done;
  !input

let rec print_trans lst =
  match List.rev lst with
  | ((a, b), c) :: t ->
      let () = printf "%d %c %d\n" a b c in
      print_trans t
  | [] -> ()

let rec print_list lst =
  match lst with
  | [] -> ()
  | [ e ] -> Printf.printf "%d\n" e
  | h :: t ->
      Printf.printf "%d " h;
      print_list t

let nEst = read_int ()
let init = read_int ()

let initl =
  List.map (fun x -> int_of_string x) (read_line () |> String.split_on_char ' ')

let nfinl = read_int ()

let finl =
  List.map (fun x -> int_of_string x) (read_line () |> String.split_on_char ' ')

let ntrans = read_int ()
let trans = create_maquina ntrans

(*new Part*)

let rec possiveis label transicoes ret =
  match transicoes with
  | ((a, b), c) :: tl ->
      if a = label then possiveis label tl (ret @ [ (b, c) ])
      else possiveis label tl ret
  | [] -> List.sort compare ret

let check_2_poss poss1 poss2 finl =
  let a, b = poss1 in
  let x, y = poss2 in
  if a = x && b = y then true
  else if a = x && List.mem b finl && List.mem y finl then true
  else false

let rec check_poss_in_list poss lposs finl =
  match lposs with
  | x :: tl ->
      if check_2_poss poss x finl then true else check_poss_in_list poss tl finl
  | [] -> false

let rec check_2_lposs lposs1 lposs2 finl =
  match lposs1 with
  | x :: tl ->
      if check_poss_in_list x lposs2 finl then check_2_lposs tl lposs2 finl
      else false
  | [] -> true

let possiveis_equivaletes lposs1 lposs2 finl =
  if List.length lposs1 = List.length lposs2 then
    check_2_lposs lposs1 lposs2 finl
  else false

let estado_equi state_1 state_2 finl transicoes =
  if
    possiveis_equivaletes
      (possiveis state_1 transicoes [])
      (possiveis state_2 transicoes [])
      finl
  then true
  else false

let rec combinacoes_to_state combinacoes ret finl transicoes =
  match combinacoes with
  | (a, b) :: tl ->
      if estado_equi a b finl transicoes then [ a; b ] :: ret
      else combinacoes_to_state tl ret finl transicoes
  | [] -> ret

let rec combinacoes lista ret x =
  match lista with [] -> ret | y :: tl -> combinacoes tl (ret @ [ (x, y) ]) x

let rec combinacoes_list lista ret =
  match lista with
  | [] -> ret
  | x :: tl -> combinacoes_list tl (ret @ combinacoes tl [] x)

let rec exists x new_states =
  match new_states with
  | hd :: tl -> if List.mem x hd then true else exists x tl
  | [] -> false

let rec distinguish transicoes estados finl ret =
  match estados with
  | hd :: tl ->
      distinguish transicoes tl finl
        (ret @ combinacoes_to_state (combinacoes_list hd []) [] finl transicoes)
  | [] -> ret

let rec new_automato old_states new_states ret =
  match old_states with
  | hd :: tl ->
      if exists hd new_states then new_automato tl new_states ret
      else new_automato tl new_states ([ hd ] :: ret)
  | [] -> List.rev ret @ new_states

let rec get_buddy x new_states =
  match new_states with
  | hd :: tl -> if List.mem (List.hd x) hd then hd else get_buddy x tl
  | [] -> x

let rec new_trans old_trans ret =
  match old_trans with
  | ((a, b), c) :: tl -> new_trans tl (em_par_2 a b c :: ret)
  | [] -> ret

(* let rec check_list_list_of_lists list big_list =
   match big_list with
   | hd :: tl ->
       if compare hd list = 0 then true else check_list_list_of_lists list tl
   | [] -> false *)

let rec get_labels trans x ret =
  match trans with
  | k :: tl ->
      let (a, b), _ = k in
      if compare x a = 0 then get_labels tl x (ret @ b) else get_labels tl x ret
  | [] -> ret

let rec combine_labels trans ret current =
  match trans with
  | ((a, _), c) :: tl ->
      if not (List.mem a current) then
        combine_labels tl (((a, get_labels trans a []), c) :: ret) (a :: current)
      else combine_labels tl ret current
  | [] -> ret

let rec final_trans_list new_trans new_states ret =
  match new_trans with
  | ((a, b), c) :: tl ->
      let k = ((get_buddy a new_states, b), get_buddy c new_states) in
      final_trans_list tl new_states (k :: ret)
  | [] -> ret

(*output
  num estados
  num estados ini
  lista estados ini
  num estados fin
  lista estados fin
  lista transicoes(ini->label->destino)
*)
let _print_result =
  printf "%d\n" nEst;
  printf "%d\n" init;
  print_list initl;
  printf "%d\n" nfinl;
  print_list finl;
  print_trans trans

let minimized_automato =
  new_automato (initl @ finl) (distinguish trans [ initl; finl ] finl []) []

let _u =
  combine_labels
    (remove_duplicates
       (final_trans_list (new_trans trans []) minimized_automato []))
    [] []
