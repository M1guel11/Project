



let aut = Hop2.automaton

let x =aut

let () = 

(*Hopcroft Lists*)
 let t0_hl = Benchmark.make 0L in

let _hop_list=  Hop2.hopcroft x in

let  time_hl = Benchmark.sub (Benchmark.make 0L) t0_hl in
Hop2.print_automaton _hop_list;
print_endline "Benchmark results Hopcroft lists:";
print_endline (Benchmark.to_string time_hl) ; 


(*_____________________________________________________________*)

(*Hopcroft Sets*)
let _hop_set = SetsH2.transform_automaton  x in


let t0_hs = Benchmark.make 0L in

(*let _hop_set_alg = SetsH2.hopcroft hop_set in*)

 
let  time_hs = Benchmark.sub (Benchmark.make 0L) t0_hs in

print_endline "Benchmark results Hopcroft Sets:";
print_endline (Benchmark.to_string time_hs) ;
(*_____________________________________________________________*)




(*Brzozowski lists*)
let t0_bl = Benchmark.make 0L in

let brz_list_alg =  Brzozowski.brzozowski x 

in
let  time_bl = Benchmark.sub (Benchmark.make 0L) t0_bl in
Hop2.print_automaton brz_list_alg;
print_endline "Benchmark results Brzozwski Lists:";
print_endline (Benchmark.to_string time_bl) ;

(*_____________________________________________________________*)

(*Brzozoeski Sets*)
 let brz_set = SetsB.transform_automaton  x in
let t0_bs = Benchmark.make 0L in

let brz_set_alg =  SetsB.brzozowski brz_set 

in
 
let  time_bs = Benchmark.sub (Benchmark.make 0L) t0_bs in
SetsB.print_automaton brz_set_alg;
print_endline "Benchmark results Brzozwski Sets:";
print_endline (Benchmark.to_string time_bs) ; 
(*_____________________________________________________________*)

