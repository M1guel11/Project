



let aut = Hopcroft.automaton

let x =aut

let () = 

(*Hopcroft Lists*)
let t0_hl = Benchmark.make 0L in

let _hop_list= Hopcroft.hopcroft x in

let  time_hl = Benchmark.sub (Benchmark.make 0L) t0_hl in
Hopcroft.print_automaton _hop_list;
print_endline "Benchmark results Hopcroft lists:";
print_endline (Benchmark.to_string time_hl) ;


(*_____________________________________________________________*)

(*Hopcroft Sets*)
let hop_set = SetsH.transform_automaton  x in


let t0_hs = Benchmark.make 0L in

let _hop_set_alg = SetsH.hopcroft hop_set in

 
let  time_hs = Benchmark.sub (Benchmark.make 0L) t0_hs in
SetsH.print_automaton _hop_set_alg;
print_endline "Benchmark results Hopcroft Sets:";
print_endline (Benchmark.to_string time_hs) ;
(*_____________________________________________________________*)


(*_____________________________________________________________*)

(*Hopcroft UnionFind*)

(*_____________________________________________________________*)