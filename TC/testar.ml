

let listchar_to_str l = String.concat "" (List.map (String.make 1) l)


let liststring_to_str l =
  List.filter (fun s -> s <> "") l
  |> String.concat ""


let rec list_x2_Str_tolist lst ret=
       match lst with
       |[] -> ret
       | x::tl -> let newL= x@ret in list_x2_Str_tolist tl newL


let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let unique xs = List.fold_right uniq_cons xs []




   




  let rec string_to_tuplo s i l =
    if(i != (String.length s)) then
      begin
        if(s.[i]=='-' || s.[i]=='>' || s.[i]==' ') then
            string_to_tuplo s (i+1) (l)
        else
            string_to_tuplo s (i+1) (s.[i]::l)
      end
  
    else
      match (List.rev l) with
      |[] -> (" ", " ") 
      |h::tl -> ( Char.escaped h,listchar_to_str tl)
  
  
  
  let create_linguagem c=
    let i = ref 0 in
    let input = ref [] in
    while (!i<c) do
        let str = read_line() in
        let aux = string_to_tuplo str 0 [] in 
        input:= (aux)::!input;
        i:=!i+1
      done;
      !input







let rec gerador a l ret=
match l with                                            (*devolve todos os valores que produzem uma string (second) que contem o carater a *)
| h::tl -> let (first,second)= h in
            if (String.equal second a) then
              gerador a tl (first::ret)
            else
              gerador a tl ret
|[] -> List.rev ret





let rec combinacao_list a b ret=                                           (*Gera combinacoes de duas Listas*)
  match a with 
  |[] -> List.rev ret
  | h::tl -> let aux= List.append (List.map (fun x -> (h^x) ) b) ret  in  combinacao_list tl b aux




  let rec print topo=
  match topo with 
  |[] -> Printf.printf "NO\n"
  |[" "] -> Printf.printf "NO\n"
  | x::tl -> if String.equal x "S" then
    Printf.printf "YES\n"
   else
    print tl



(*inputs*)
let palavra = read_line()

let matrix = Array.make_matrix (String.length palavra + 1 ) (String.length palavra) [" "]

let m = read_int()


let gramatica = create_linguagem m
    
(*___________________________________________________________*)
(*calcular output*)

 (*cria a cadeia onde se vai gerar o algoritmo CYK*)


let () =
for i=0 to (Array.length matrix.(0) -1) do 
  matrix.(0).(i) <-   [listchar_to_str  [palavra.[i]]]             (*introduz caracter a caracter a palavra metida na primeira linha da matriz*)
done

let () =
for i=0 to (Array.length matrix.(1) -1) do
  let aux =gerador (listchar_to_str [palavra.[i]]) gramatica [] in
  matrix.(1).(i) <- aux                                            (*produz a segunda linha da cadeia CYK*)
done   

(*   2,0
    (i,j)= [(i-1, j ) * (i-2,j + k - 0)]* 
           [(i-2, j ) * (i-1,j + k - 1)] *  k=(i-1)
        
         ((1,0)(1,)
         
         *)

let calcPos i j =
  let ret = ref [] in
  let k = i-1 in
  for cont = 1 to i - 1 do
    let a = matrix.(i-cont).(j) in
    let b =  matrix.(i-(i-cont)).(j + k - ( cont-1)) in
    let aux = combinacao_list  a b [] in
    let newa = (aux) in                                            (*devolve a lista de combinacoes para preencher determinada posicao*)
    ret :=  newa :: !ret 
  done;
  List.rev !ret



  let rec listaCombtolistaGera lst ret= 
  match lst with
  | [] -> ret
  | x::tl -> let newret  = ((gerador x gramatica []) @ ret)
in 
    listaCombtolistaGera tl (newret)





(*let () = match testar with
         |[] -> Printf.printf " "
         |(a) :: t -> match a with 
                   |[] -> Printf.printf " "
                   | x :: t-> Printf.printf "%s," x*)




(*let () = match testar2 with
         |[] -> Printf.printf " "
         |(a) :: t -> Printf.printf "%s," a 
 *)


let finalmatrix = 
  for i=2 to (Array.length matrix.(0))  do 
    for j=0 to  Array.length matrix.(i) -(i-1) -1 do 
        
          let aux = calcPos i j in
          let aux2 = let aux3 = list_x2_Str_tolist aux [] in  listaCombtolistaGera aux3 [] in
          (*organizar*)
          if List.length aux2 == 0 then
            matrix.(i).(j) <- [""]
          else
            let aux4 = unique aux2 in
            matrix.(i).(j) <-   List.sort String.compare aux4
    done
  
  
done


let revmatrix = Array.make_matrix (String.length palavra + 1 ) (String.length palavra) [" "] 
let ()= 

    let size = Array.length matrix.(0) in
    for i = 0 to size do
            revmatrix.(size-i) <- matrix.(i)
    done 






let () =
let () = print (matrix.(String.length palavra).(0)) in
                  
for i=0 to (Array.length matrix.(0) )  do
    for j=0 to (Array.length matrix.(i) - 1 )  do
   
       (List.iter (Printf.printf "%s ")   revmatrix.(i).(j));
       Printf.printf "\t\t"
 
     
    done;
    Printf.printf "\n";
done

(*let () = (List.iter (Printf.printf "[%s]")   matrix.(7).(0) )*)