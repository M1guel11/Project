open Scanf
open Printf

let em_par a b c =  ((a,b),c)

let rec check_trans1 lista =
      match lista with
      | [] -> 0
      | ((_,e),_)::t ->  if (e == '_') then 1 else check_trans1 t

let check_dfa list ninit=
    let a =check_trans1 list in
    if (a==1 || ninit > 1) then printf("NDFA\n") else printf("DFA\n")

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

let rec next ltrans estado letra =
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

let is_accepted estados_fin palavra cont aqui=
    if( (String.length(palavra) -cont)==0 && List.mem aqui estados_fin) then
      true
    else
      false

let is_final a listf=
if List.mem a listf then true else false

let rec print_list lst =
    match lst with
    | [] -> ()
    | [e] -> Printf.printf "%d\n" e
    | h::t -> Printf.printf "%d " h; print_list t


let out fim =
    let (a,b)= fim in
    if a then
      let () = Printf.printf "YES\n";
   print_list (List.rev b) in ()
   else Printf.printf"NO\n"

  let nEst = read_int ()
  let ninit= read_int ()
  let initl =  List.map (fun x -> int_of_string x) ((read_line()) |> String.split_on_char ' ')
  let nfinl= read_int ()
  let finl = List.map (fun x -> int_of_string x) ((read_line ()) |> String.split_on_char ' ')
  let ntrans= read_int ()
  let trans=create_maquina ntrans
  let dados = read_line()
  let check_aut = check_dfa trans ninit

let rec executa palavra cont caminho aqui possiveis =
    match possiveis with
    [] -> if is_accepted finl palavra cont aqui then (true,(aqui::caminho)) else (false,[])
    | h::t ->
      let (a,b,c)  = step trans aqui h palavra cont caminho in
          if b >=(String.length palavra) then
            if List.exists (fun x -> x = a ) finl then (true,a::aqui::caminho)
              else begin
                let aux_poss_next = next trans a '_' in
                if   List.exists (fun y -> is_final y finl) aux_poss_next then
                  let new_a = List.find (fun z -> is_final z finl) aux_poss_next in
                    executa palavra b (a::aqui::caminho) new_a t
                    else begin
                      if List.length (aux_poss_next) <> 0  then
                        executa palavra b (aqui::caminho) a aux_poss_next
                        else
                          executa palavra b (caminho) a t
              end
            end
          else
              let aux_next = next trans a (palavra.[b]) in
              let (d,e) = executa palavra b (aqui::caminho) a aux_next in
              if d then
                (d,e)
               else executa palavra cont caminho aqui t

let resolve=
  List.fold_left (fun (a,b) x -> if a then (a,b) else executa dados 0 [] x (next trans x dados.[0])) (false,[]) initl

let h = out resolve