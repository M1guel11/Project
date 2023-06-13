
open Scanf
open Printf
open Set


(*UnionFind Implementation*)
module UnionFind = struct
  type t = int array
  let make n = Array.init n (fun i -> i)
  let rec find (arr : t) (i : int) =
    if arr.(i) = i then i else find arr arr.(i)
  let union (arr : t) (i : int) (j : int) =
    let root_i = find arr i in
    let root_j = find arr j in
    arr.(root_i) <- root_j
end


