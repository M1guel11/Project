
open Scanf
open Printf




let em_par a b c =  ((a,b),c)



let is_final a listf=
if List.mem a listf then true else false

let rec check_trans1 lista =
      match lista with
      | [] -> 0
      | ((_,e),_)::t ->  if (e == '_') then 1 else check_trans1 t


let rec is_eps_trans ltrans aqui destino=
    match ltrans with
    |[] -> false
    |((a,b),c)::t -> if a == aqui && b = '_' && c == destino then true else is_eps_trans t aqui destino

let create_maquina c=
  let i = ref 0 in
  let input = ref [] in
  while (!i<c) do
      let str = read_line() in
      input:= (sscanf str " %d %c %d "em_par )::!input ;
      i:=!i+1
    done;
    !input


(*maybe *)
    (*let rec next ltrans estado letra =
      let aux= ref [] in
      for i=0 to (List.length ltrans)-1 do
        let ((a,b),c) = List.nth ltrans i in
       if ((a==estado && b==letra) || (a==estado && b=='_')) then (aux:= c :: !aux);
      done;
      List.sort compare !aux
    
    let step ltrans aqui destino palavra cont caminho =
          if is_eps_trans ltrans aqui destino then
            let new_caminho = (aqui)::caminho in
            let aux = (destino,cont,new_caminho) in
            aux
          else
            let new_caminho = (aqui)::caminho in
            let aux = (destino,cont+1,new_caminho) in
            aux
*)


            let rec print_trans lst =
              match List.rev lst with
              | ((a,b),c) :: t  -> let () = printf "%d %c %d\n" a b c in print_trans t
              | [] -> () 
              
            

              let rec print_list lst =
                match lst with
                | [] -> ()
                | [e] -> Printf.printf "%d\n" e
                | h::t -> Printf.printf "%d " h; print_list t




    let nEst = read_int ()
    let init= read_int ()
    let initl =  List.map (fun x -> int_of_string x) ((read_line()) |> String.split_on_char ' ')
    let nfinl= read_int ()
    let finl = List.map (fun x -> int_of_string x) ((read_line ()) |> String.split_on_char ' ')
    let ntrans= read_int ()
    let trans=create_maquina ntrans



    (*new Part*)

  
let rec get_alfabeto transicoes ret= 
  match transicoes with
  |((a,b),c) :: tl -> if not (List.mem b ret) then get_alfabeto tl (ret @ [b]) else get_alfabeto tl ret
  | [] -> List.sort compare ret 
    
let rec get_destino ini label transicoes=
match transicoes with 
| ((a,b),c) :: tl -> if a=ini && (compare b label = 0) then c else get_destino ini label tl
| [] -> 0


let rec possiveis label transicoes ret  =
  match transicoes with 
  | ((a,b),c) ::tl -> if a = label then possiveis label tl (ret @ [(b,c)]) else possiveis label tl ret 
  | [] -> List.sort compare ret


let check_2_poss poss1 poss2 finl =
  let (a,b)  = poss1 in
  let(x,y) = poss2 in 
  if a=x && b=y then true else begin if a=x && List.mem b finl && List.mem y finl then true else false end


let rec check_poss_in_list poss lposs finl =
    match lposs with
    |x::tl -> if check_2_poss poss x finl then true else check_poss_in_list poss tl finl
    |[]-> false

let rec check_2_lposs lposs1 lposs2 finl =
    match lposs1 with
    |x::tl -> if check_poss_in_list x lposs2 finl then check_2_lposs tl lposs2 finl else false 
    |[] -> true  

let rec possiveis_equivaletes lposs1 lposs2 finl =
  if List.length lposs1 =  List.length lposs2 then  check_2_lposs lposs1 lposs2 finl else false


let rec estado_equi state_1 state_2 finl transicoes =
  if possiveis_equivaletes (possiveis state_1 transicoes []) (possiveis state_2 transicoes []) finl then true else false  

let rec combinacoes_to_state combinacoes ret finl transicoes=
  match combinacoes with
  | (a,b) :: tl -> if estado_equi a b finl transicoes then ret @ [(a,b)] else combinacoes_to_state tl ret finl transicoes
  | [] -> ret


let rec combinacoes lista ret x = 
  match lista with
  | [] -> ret
  | y ::tl -> combinacoes tl (ret @ [(x,y)]) x

let rec combinacoes_list lista ret = 
  match lista with 
  | [] -> ret
  | x::tl -> combinacoes_list tl (ret @ combinacoes tl [] x )




let rec exists   x new_states =
  match new_states with
  | (a,b) :: tl -> if  a = x || b = x then true else exists x tl
  |[] -> false  



let rec distinguish transicoes estados finl ret =
  match estados with
  | hd :: tl -> distinguish transicoes tl  finl ( ret @ combinacoes_to_state (combinacoes_list hd []) [] finl transicoes)
  | [] -> ret
  
let rec new_automato  old_states new_states  ret =
    match old_states with
    | hd ::tl -> if exists hd new_states then new_automato tl new_states ret else new_automato tl new_states (ret @ [hd])
    | [] -> ret


let rec get_trans trans x ret= 
  match trans with 
  |((a,b),c) ::tl -> if x=a then get_trans tl x (ret@ [((a,b),c)]) else get_trans tl x ret
  |[] -> ret


let rec new_trans old_trans new_states ret =
  match new_states with
  | (a,b) :: tl -> let x1 = get_trans old_trans a [] in let x2 = get_trans old_trans b [] in [x1;x2]
  | [] -> ret


(*output 
num estados 
num estados ini
lista estados ini
num estados fin
lista estados fin
lista transicoes(ini->label->destino)   
*)
let () = 
  printf "%d\n" nEst;
  printf "%d\n" init;
  print_list initl;  
  printf "%d\n" nfinl;
  print_list finl;
  print_trans trans

let k = distinguish trans [initl;finl] finl []

let y = (new_automato (initl @ finl) k [])

let o = new_trans trans k []

let u = (y,k)