
let t0 = Benchmark.make 0L in
(* do something here *)
let () = Printf.printf "Running %s\n" Sys.argv.(0) in
let b = Benchmark.sub (Benchmark.make 0L) t0 in
print_endline "Benchmark results:";
print_endline (Benchmark.to_string b)         

