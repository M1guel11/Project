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
        match sub, lst with
        | [], _ -> true
        | _, [] -> false
        | x::xs, y::ys ->
            if x = y then sublist xs ys
            else sublist sub ys
      




      let is_sublist sub lst =
        let rec check_sublist sub lst =
          match sub, lst with
          | [], _ -> true
          | _, [] -> false
          | x::xs, y::ys -> if x = y then check_sublist xs ys else check_sublist sub ys
        in
        let sub_length = List.length sub in
        let rec check_lists lists =
          match lists with
          | [] -> false
          | hd::tl -> if List.length hd >= sub_length && check_sublist sub hd then true else check_lists tl
        in
        check_lists lst
      














let create_maquina c =
  let i = ref 0 in
  let input = ref [] in
  while !i < c do
    let str = read_line () in
    input := sscanf str " %d %c %d " em_par:: !input;
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

let read_int_list () = List.map (fun x -> int_of_string x) (read_line () |> String.split_on_char ' ')

(*let read_automata () =*)
  let nEst = read_int () 
  let init = read_int () 
  let initl = read_int_list () 
  let nfinl = read_int () 
  let finl = read_int_list ()  
  let ntrans = read_int () 
  let trans = create_maquina ntrans 




(*new Part*)

let rec possiveis label transicoes ret =
  match transicoes with
  | ((a, b), c) :: tl ->
      if List.mem label a then possiveis label tl (ret @ [ (b, c) ])
      else possiveis label tl ret
  | [] -> List.sort compare ret


let val1 = possiveis 3 trans []
let val2 = possiveis 5 trans []


let compare_possibles poss1 poss2 finl =
  let a, b = poss1 in
  let x, y = poss2 in
   (List.compare  compare a x == 0  && List.compare compare b y == 0 || List.compare compare a x == 0 && sublist b finl && sublist y finl ) 

  


let rec compare_lists_possibles l1 l2 finl =
   if List.length l1 = List.length l2 then
      List.fold_left2 (fun acc x y ->
        if acc = false then false
        else compare_possibles x y finl
      ) true (List.sort compare l1)  (List.sort compare l2)
    
   else false





let rec combinacoes lista ret =
  let rec comb lista ret x =
    match lista with [] -> ret | y :: tl -> comb tl (ret @ [ (x, y) ]) x
  in
    match lista with
      | [] -> ret
      | x :: tl -> combinacoes tl (ret @ comb tl [] x)


let sas= combinacoes (initl @ finl) []  


let rec combinacoes_to_state list ret finl transicoes =
  match list with
  | (a, b) :: tl ->
      if compare_lists_possibles (possiveis a transicoes []) (possiveis b transicoes []) finl then combinacoes_to_state tl ([a; b] :: ret) finl transicoes 
      else 
        begin 
          if is_sublist [a] ret || is_sublist [b] ret then combinacoes_to_state tl ( ret) finl transicoes  
          else combinacoes_to_state tl (ret @ [ [a]; [b] ]) finl transicoes 
        end 
  | [] -> List.sort compare  (remove_duplicates ret)
   

let yyy= combinacoes_to_state sas [] finl trans

let transform_list states trans =
  let new_st = List.concat b in
  List.map (fun ((st, sym), tr) ->
    if List.mem (List.hd st) new_st then
      ((new_st, sym), tr)
    else
      ((st, sym), tr)) a



let asdas= transform_list trans yyy

let rec get_buddy x new_states =
  match new_states with
  | hd :: tl -> if List.mem (List.hd x) hd then hd else get_buddy x tl
  | [] -> x


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
  print_list finl
  (*print_trans trans*)


(*
let minimized_automato =
  new_automato (initl @ finl) (distinguish trans [ initl; finl ] finl []) []

let _u =
  combine_labels
    (remove_duplicates
       (final_trans_list (new_trans trans []) minimized_automato []))
    [] []
*)