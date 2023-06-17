open Scanf
open Printf

type state = int
type symbol = char
type transition = state list * symbol list * state list

type automaton = {
  states : state list;
  alphabet : symbol list;
  transitions : transition list;
  initial : state list;
  finals : state list;
}

let automaton =
  let read_int_list () =
    List.map int_of_string (read_line () |> String.split_on_char ' ')
  in
  let create_machine nTrans =
    let rec loop i acc =
      if i = 0 then List.rev acc
      else
        let str = read_line () in
        let transition =
          sscanf str " %d %c %d " (fun a b c -> ([ a ], [ b ], [ c ]))
        in
        loop (i - 1) (transition :: acc)
    in
    loop nTrans []
  in
  let calc_inter transitions finiS =
    let aux = List.fold_left (fun acc (a, _, c) ->
      let x = List.hd a in
      let y = List.hd c in
       x :: y :: acc
    ) finiS transitions
    in
    List.sort_uniq compare aux
  in
 
  let calc_alpha transitions =
    let aux = List.fold_left (fun acc (_, b, _) ->
      let y = (List.hd b) in
      y :: acc
    ) [] transitions
    in
    List.sort_uniq compare aux 
  
      in  
  let _nS = read_int () in
  let iniS = read_int () in
  let _nF = read_int () in
  let finiS = read_int_list () in
  let nTrans = read_int () in
  let trans = create_machine nTrans in
  let alphabet = calc_alpha trans  in
  let states = calc_inter trans finiS in


  { states; alphabet; transitions = trans; initial = [ iniS ]; finals = finiS }

(*prints*)
let print_states a =
  a
  |> List.iter (fun x ->
         print_int x;
         print_string " ")

let print_transitions a =
  a
  |> List.iter (fun (a, b, c) ->
         List.iter
           (fun x ->
             print_int x;
             print_string " ")
           a;
         print_string "-> ";
         List.iter
           (fun x ->
             print_char x;
             print_string " ")
           b;
         print_string "-> ";
         List.iter
           (fun x ->
             print_int x;
             print_string " ")
           c;
         print_newline ())

let print_automaton a =
  print_states a.states;
  print_newline ();
  print_transitions a.transitions;
  print_states a.initial;
  print_newline ();
  print_states a.finals;
  print_newline ()

let alghorithm a =
  let inv_trans trans = List.map (fun (x, y, z) -> (z, y, x)) trans in
  let _reach state label trans =
    List.fold_left
      (fun acc (x, y, z) ->
        if List.mem state x && List.mem label y then z @ acc else acc)
      [] trans
  in
  let determinization ini trans =
    let queue= ini in
    let seen = [] in
    
    1
  in

  {
    states = a.states;
    alphabet = a.alphabet;
    transitions = inv_trans a.transitions;
    initial = a.finals;
    finals = a.initial;
  }

let automaton = alghorithm automaton
