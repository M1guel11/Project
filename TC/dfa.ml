open List
open Str
open Scanf
open Printf

(**
ExecuÃ§Ã£o de autÃ³matos nÃ£o deterministas com (potencialmente) transiÃ§Ãµes epsilon

*)


(**
O tipo simbolo representa o tipo das letras (o alfabeto - presentes nas fitas mas tambÃ©m nas transiÃ§Ãµes). Por isso devemos considerar o alfabeto standard (aqui utilizamos o tipo char) mas tambÃ©m considerar a palavra/letra vazia: o epsilon.

Neste caso concreto optamos pelo tipo char option. Assim o valor Some x representa o caractere x, e o valor None representa o epsilon.

*)


type simbolo = char option


(** a fita de entrada Ã© simplesmente uma lista de simbolos *)

type fita =  simbolo list

(**
Escolhemos representar os estados por inteiros. Em particular tentaremos respeitar o invariante seguinte sobre a representaÃ§Ã£o dos estados: Se houver n estados entÃ£o estes sÃ£o 0 1 2 3 .. n-1.

*)


type estado = int


(**
As transiÃ§Ãµes q1 --a--> q2 sÃ£o representadas como ((q1,a),q2) ao detrimento da representaÃ§Ã£o mais natural (q1,a,q2).

Esta pequena nuance permite uma pesquisa melhorada de uma transiÃ§Ã£o no conjunto das possÃ­veis transiÃ§Ãµes (de tipo hash table, em que a chave Ã© (q1,a) e o conteudo Ã© estado destino q2)

*)


type transicao =  ((estado*simbolo)*estado)


(**
Neste cenÃ¡rio, um autÃ³mato (ou mÃ¡quina) Ã© dado pela relaÃ§Ã£o de transiÃ§Ã£o (a listas de adjacÃªncia, ou seja a lista das transiÃ§Ãµes), o conjuntos dos estados iniciais e o conjunto dos estados finais. Sendo que o alfabeto Ã© o tipo char e conjunto dos estados se deduz automaticamente dos dados anteriores. *)


type maquina = (transicao list * estado list * estado list)


(**
As configuraÃ§Ãµes da mÃ¡quina (nÃ£o determinista), aqui designada de memÃ³ria, Ã© simplesmente o par dos estados actualmente activos (onde se encontra no momento a execuÃ§Ã£o) e o buffer ainda por processar

*)


type memoria = (estado list * fita)


(** uma excepÃ§Ã£o para assinalar o fim de uma execuÃ§Ã£o *)

exception FIM of memoria



(**
Duas funÃ§Ãµes simples mas naturais que codificam as noÃ§Ãµes de subconjuntos e igualdade de conjunto (sendo estes codificados como listas)

*)


let subset c1 c2 =
  for_all  (function x -> (mem x c2)) c1;;

let  equal c1 c2 =
  (subset c1 c2) && (subset c2 c1);;


(** As operaÃ§Ãµes sobre transiÃ§Ãµes, estados ou configuraÃ§Ãµes podem levar a que se gere muitos duplicados nas listas que os represnetam. A funÃ§Ã£o normalize de forma simples (hÃ¡ melhor....) mas natural permite remover estes elementos duplicados *)

let normalize c =
  fold_left
    (fun res x ->
      if (mem x res)
      then res
      else (x::res) )
    [] c


(** uniÃ£o de dois conjuntos *)

let union c1 c2 =
  normalize  (c1@c2);;



(**
Dado uma mÃ¡quina maq e um estado state, a funÃ§Ã£o epsilon_trans_aux calcula que estados se consegue atingir a partir de state e de *uma* transiÃ§Ã£o epsilon.

*)


let epsilon_trans_aux state maq =
  let transicoes,b,c = maq in
  map (function (a,b) -> b)
    (filter (function (x,y) -> x=(state,None)) transicoes)

(**
Generaliza a funÃ§Ã£o anterior. A funÃ§Ã£o epsilon_trans calcula todos os estados atingÃ­veis, por uma ou mais transiÃ§Ãµes epsilon, a partir dos estados em lstate (lista de estado). O calculo Ã© feito por "ponto fixo". enquanto aparecer estados novos calcula-se, mal deixe de aparecer novidades... para-se.

*)


let rec epsilon_trans lstate maq =

  	(** res = todos os estados atingÃ­veis por uma transiÃ§Ã£o epsilon a partir dos estados de lstate *)

  let res =
    (normalize (flatten (map (fun x -> epsilon_trans_aux x maq) lstate))) in


  	(** junta-se estes estados ao lstate*)

  let resultado = (union res lstate) in


  	(** se esta uniÃ£o nÃ£o traz nada de novo... ponto fixo. Devolve se entÃ£o resultado *)

  if (equal lstate resultado)
  then resultado


  	(** senÃ£o, tenta-se mais uma volta*)

  else (epsilon_trans resultado  maq)


(** select devolve todas os estados alvo de transiÃ§Ãµes que partem de est com o label simb *)

let  select est simb tabela =
  map (function (a,b) -> b) (filter (function  ((a,b),c) -> a=est && b=simb) tabela)


(**
next calcula os estados atingÃ­veis a partir dos estados em lesta com o simbolo simb, combinado com os estados atingÃ­veis por transsiÃ§Ãµes epsilon a partir daÃ­.

se nÃ£o houver estados atingidos, damos a indicaÃ§Ã£o de que se terminou a execuÃ§Ã£o

*)


let next simb maquina memo =
  let (lesta, restante) = memo in
  let (transicoes,b,c) = maquina in

  	(** tr = os estados atingÃ­veis pela transiÃ§Ã£o de label simb a partir dos estado de lesta*)

  let tr = (fold_left
              (fun x y -> (select y simb transicoes)@x )
              [] lesta)
  in

  	(** estende-se tr com os estados atingÃ­dos por transiÃ§Ãµes epsilon *)

  let res =  epsilon_trans  (normalize tr) maquina  in
  if (res = []) then raise (FIM memo) else res









(** step realiza um passo de execuÃ§Ã£o do autÃ³mato maq a partir da configuraÃ§Ã£o memo. *)

let step memo maq =
  let (laqui, restante) = memo in

  	(** se o buffer de entrada for vazio, entÃ£o acabou, senÃ£o tratamos do primeiro caracter do buffer. Ou seja, vamos ver que novos estados atingimos com ele a partir dos estados actualmente activos (onde a execuÃ§Ã£o actualmente se encontra) que estÃ£o em laqui. Chamanos aqui a funÃ§Ã£o next que trata deste cÃ¡lculo. *)

  match restante with
      [] ->  raise (FIM memo)
    | el::li ->  (next el maq memo,li)


(** is_accepted Ã© um predicado que detecta se uma configuraÃ§Ã£o memo da execuÃ§Ã£o do autÃ³mato maq prefigura a aceitaÃ§Ã£o. Ou seja o buffer de entrada encontra-se vazio e hÃ¡ pelo menos um estado final na configuraÃ§Ã£o *)





let is_accepted memo maq =
  let (laqui, restante) = memo in
  let (trans,init,accept)= maq in
  (exists (fun x -> mem x accept) laqui) && restante=[]



(** to_fita Ã© uma funÃ§Ã£o de traduÃ§Ã£o de char para simbolo. O caracter '_' Ã© entendido como o epsilon *)

let to_fita c =  if c='_' then None else (Some c)

(** traduÃ§Ã£o para o formato transiÃ§Ã£o*)

let em_par a b c =  ((a, to_fita b),c)



(** funÃ§Ã£o utilitÃ¡ria simples*)

let char_of_string s = s.[0]

let create_maquina a b c=     
  let i = ref 0 in 
  let input = ref [] in
  try
    while (!i<c-1) do
      input:= (scanf " %d %c %d " em_par)::!input ;
      i:=!i+1
    done; 
    (!input,a,b)
    with _ -> 
       (!input,a,b)

(** LÃª no formato texto o autÃ³mato por executar, e a palavra por reconhecer. A leitura devolve as estruturas correspondentes. *)





(** a funÃ§Ã£o print_output analisa a configuraÃ§Ã£o final e imprime na saÃ­da standard o veredicto. *)

let print_output memo maq=
  if (is_accepted memo maq)
  then printf "YES\n"
  else printf"NO\n"




  
let output maquina dados=
  let (a,b,c) = maquina in
  try
    let memor = ref ((epsilon_trans b maquina),dados)  in
    while true do
      memor := (step !memor maquina)
    done
  with
      FIM x -> print_output x maquina

let check_list lista num =
  if length(lista)<>num then raise(invalid_arg "Erro") 


let  check_trans maquina num=
let (a,b,c) = maquina in
if length(!a) <> num  then raise(invalid_arg "Erro")
 
  
let get_list maquina=
let (a,b,c)=maquina in a 

let rec check_trans1 lista =
match lista with
| [] -> 0
| ((_,e),_)::t ->  if (e == None) then 1 else check_trans1 t

let check_dfa maquina ninit=
let list = get_list maquina in
let a =check_trans1 list in
if (a==1 || ninit > 1) then printf("NDFA\n") else printf("DFA\n") 


(** funÃ§Ã£o principal *)

  let nEst = int_of_string(read_line())
  let ninit= int_of_string(read_line()) 
  let initl =  map (fun x -> int_of_string x) ((read_line()) |> String.split_on_char ' ') 
  let () = check_list initl ninit
  let nfinl= int_of_string(read_line()) 
  let finl = map (fun x -> int_of_string x) ((read_line ()) |> String.split_on_char ' ') 
  let () = check_list finl nfinl
  let ntrans= int_of_string(read_line()) 
  let maquina= create_maquina (initl) (finl) (ntrans)   
  let dados =  map (fun x -> to_fita (char_of_string x)) ((read_line()) |> String.split_on_char ' ')
  let c = check_dfa maquina ninit

  let fim = output maquina dados 


(** executa. *)



(** exemplo de entrada:
aaba 0 1 2 0 a 0 0 b 1 0 a 3 1 a 2 2 a 3 3 a 1 3 a 2

*)

