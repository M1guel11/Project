open Scanf
open Printf
open Set



(*UnionFind Implementation*)
module UnionFind = struct
  type 'a union_find = {
    parent: 'a union_find option ref;
    rank: int ref;
    value: 'a;
  }

  let make x = { parent = ref None; rank = ref 0; value = x }

  let rec find x =
    match !(x.parent) with
    | None -> x
    | Some p ->
        let root = find p in
        x.parent := Some root;
        root

  let union x y =
    let root_x = find x in
    let root_y = find y in
    if root_x == root_y then ()
    else if !(root_x.rank) < !(root_y.rank) then root_x.parent := Some root_y
    else if !(root_x.rank) > !(root_y.rank) then root_y.parent := Some root_x
    else begin
      root_y.parent := Some root_x;
      root_x.rank := !(root_x.rank) + 1
    end
end

(*input*)


let read_states () =
  read_line () |> String.split_on_char ' '
  |> List.map (fun x -> int_of_string x)
  |> List.map UnionFind.make


  



let root = read_int () |> UnionFind.make 
let u = read_states () 




