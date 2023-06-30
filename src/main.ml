

let m = Hopcroft.automaton

let () = 

let t0 = Benchmark.make 0L in

let _hopcroft= Hopcroft.hopcroft m in

let b = Benchmark.sub (Benchmark.make 0L) t0 in
print_endline "Benchmark results:";
print_endline (Benchmark.to_string b)   

