open List
open Scanf
open Str
open Printf




let em_par a b c =  ((a,b),c)

(*check if its DFA or NDFA*)
let rec check_trans1 lista =
      match lista with
      | [] -> 0
      | ((_,e),_)::t ->  if (e == '_') then 1 else check_trans1 t

let check_dfa list ninit=
    let a =check_trans1 list in
    if (a==1 || ninit > 1) then printf("NDFA\n") else printf("DFA\n") 
 
let  rec is_eps_trans  ltrans aqui destino=
    match ltrans with
    |((a,b),c)::t -> if a==aqui && destino==c && b='_' then true else is_eps_trans t aqui destino
    |[] -> false  
    
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
  let aux= ref[] in
  for i=0 to (List.length ltrans)-1 do
    let ((a,b),c) = List.nth ltrans i in 
   if ((a==estado && b==letra)||(a==estado && b=='_')) then (aux:= c :: !aux);
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


let rec principal_DFA estados_ini estados_fin ltrans palavra cont caminho aqui=
      if (is_accepted estados_fin palavra cont aqui)  then
          let new_caminho = (aqui)::caminho in
          let () = printf ("YES\n") in
          List.iter (fun e -> Printf.printf "%d " e) (List.rev new_caminho) ;
      else begin
          if ((String.length(palavra)-cont)==0) then 
           printf ("NO\n")
        else
          let poss= next (ltrans) (aqui) (palavra.[cont]) in
          if List.length poss ==0 then 
          printf("NO\n")
        else
          let destino = List.nth poss 0 in
          let (a,b,c) = step ltrans aqui destino palavra cont caminho in
          principal_DFA estados_ini estados_fin ltrans palavra b c a
          end

let rec principal_NDFA biglist cont_biglist estados_fin ltrans palavra cont caminho aqui=
  let biglista = ref biglist in
  let ini = List.nth biglist cont_biglist in
  let poss = next ltrans (ini) palavra.[cont] in
  biglista:= poss :: !biglista in
  List.fold_left (poss) biglista 
  let ini_prox= List.nth poss 
  let prox = step ltrans ini_prox   
  





  let nEst = read_int ()
  let ninit= read_int ()
  let initl =  map (fun x -> int_of_string x) ((read_line()) |> String.split_on_char ' ') 
  let nfinl= read_int ()
  let finl = map (fun x -> int_of_string x) ((read_line ()) |> String.split_on_char ' ')
  let ntrans= read_int ()
  let trans=create_maquina ntrans
  let dados = read_line()
  let check_aut = check_dfa trans ninit
  let h = principal_DFA initl finl trans dados 0 [] (List.nth initl 0)
  let g= printf"\n"     
   
 