open Graph
open Traverse

open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
  end


module GD = Imperative.Digraph.Concrete(Int)
module GG = Imperative.Graph.Concrete(Int)

module TD = Dfs(GD)
module TG = Dfs(GG)

let print_list (b, l) =  Format.printf "[ " ; List.iter (fun e -> Format.printf "%d " e ) l ; Format.printf "] : %b@." b

(*Complete, connected digraph with no cycles *)
let () = 
  let g = GD.create () in 
  GD.add_vertex g 1 ; 
  GD.add_vertex g 2 ; 
  GD.add_vertex g 3 ; 
  GD.add_vertex g 4 ;
  GD.add_vertex g 5 ;
  GD.add_edge g 1 2 ;  
  GD.add_edge g 1 3 ;  
  GD.add_edge g 1 4 ;  
  GD.add_edge g 1 5 ;
  GD.add_edge g 5 2 ;
  GD.add_edge g 5 3 ;
  GD.add_edge g 5 4 ;
  GD.add_edge g 4 3 ;
  GD.add_edge g 2 3 ;
  GD.add_edge g 2 4 ;
  print_list (TD.has_cycle g)  

(* Digraph with self-cycle *)
let () = 
  let g = GD.create () in 
  GD.add_vertex g 1 ; 
  GD.add_vertex g 2 ; 
  GD.add_edge g 1 1 ;
  GD.add_edge g 1 2 ;
  print_list (TD.has_cycle g)  

(* Digraph with paralel-edges *)
let () = 
  let g = GD.create () in 
  GD.add_vertex g 1 ; 
  GD.add_vertex g 2 ; 
  GD.add_edge g 2 1 ;
  GD.add_edge g 1 2 ;
  print_list (TD.has_cycle g)  


(*Simple Graph with nested cycles*)
let () = 
  let g = GG.create () in 
  GG.add_vertex g 1 ; 
  GG.add_vertex g 2 ; 
  GG.add_vertex g 3 ; 
  GG.add_vertex g 4 ;
  GG.add_vertex g 5 ;
  GG.add_edge g 1 2 ;  
  GG.add_edge g 2 3 ;
  GG.add_edge g 2 5 ;
  GG.add_edge g 3 4 ;
  GG.add_edge g 4 5 ;
  GG.add_edge g 4 2 ; 
  GG.add_edge g 5 1 ;
  print_list (TG.has_cycle g)  

(*Disjoint simple digraph, with cycles in both *)
let () = 
  let g = GD.create () in 
  GD.add_vertex g 1 ; 
  GD.add_vertex g 2 ; 
  GD.add_vertex g 3 ; 
  GD.add_vertex g 4 ;
  GD.add_vertex g 5 ;
  GD.add_vertex g 6 ;
  GD.add_vertex g 7 ;
  GD.add_vertex g 8 ;
  GD.add_edge g 1 2 ;  
  GD.add_edge g 2 3 ;
  GD.add_edge g 3 4 ;
  GD.add_edge g 4 2 ;
  GD.add_edge g 8 6 ; 
  GD.add_edge g 6 7 ;
  GD.add_edge g 7 8 ;
  GD.add_edge g 7 5 ;
  GD.add_edge g 6 5 ;
  print_list (TD.has_cycle g)  


(*Disjoint simple graph, with cycles in one *)
let () = 
  let g = GG.create () in 
  GG.add_vertex g 1 ; 
  GG.add_vertex g 2 ; 
  GG.add_vertex g 3 ; 
  GG.add_vertex g 4 ;
  GG.add_vertex g 5 ;
  GG.add_vertex g 5 ;
  GG.add_vertex g 6 ;
  GG.add_vertex g 7 ;
  GG.add_vertex g 8 ;
  GG.add_vertex g 9 ;
  GG.add_vertex g 10 ;
  GG.add_vertex g 11 ;
  GG.add_edge g 1 4 ;  
  GG.add_edge g 1 3 ;
  GG.add_edge g 1 6 ;
  GG.add_edge g 3 2 ;
  GG.add_edge g 3 5 ;
  GG.add_edge g 7 9 ; 
  GG.add_edge g 8 9 ;
  GG.add_edge g 10 9 ;
  GG.add_edge g 11 9 ;
  GG.add_edge g 10 11 ;
  print_list (TG.has_cycle g)  

(*Complex joint graph with one cycle *)
let () = 
  let g = GD.create () in 
  GD.add_vertex g 1 ; 
  GD.add_vertex g 2 ; 
  GD.add_vertex g 3 ; 
  GD.add_vertex g 4 ;
  GD.add_vertex g 5 ;
  GD.add_vertex g 6 ;
  GD.add_vertex g 7 ;
  GD.add_vertex g 8 ;
  GD.add_vertex g 9 ;
  GD.add_vertex g 10 ;
  GD.add_vertex g 11 ;
  GD.add_vertex g 12 ;
  GD.add_vertex g 13 ;
  GD.add_vertex g 14 ;
  GD.add_edge g 1 2 ; 
  GD.add_edge g 1 6 ; 
  GD.add_edge g 1 7 ; 
  GD.add_edge g 2 14 ; 
  GD.add_edge g 3 4 ; 
  GD.add_edge g 3 8 ; 
  GD.add_edge g 4 13 ; 
  GD.add_edge g 4 5 ; 
  GD.add_edge g 5 2 ; 
  GD.add_edge g 6 2 ; 
  GD.add_edge g 6 8 ; 
  GD.add_edge g 7 12 ; 
  GD.add_edge g 8 4 ; 
  GD.add_edge g 9 6 ; 
  GD.add_edge g 10 7 ; 
  GD.add_edge g 10 1 ; 
  GD.add_edge g 10 9 ; 
  GD.add_edge g 11 9 ; 
  GD.add_edge g 12 11 ; 
  GD.add_edge g 14 7 ; 
  print_list (TD.has_cycle g)  

(*Complex disjoint graph with no cycles*)
let () = 
let g = GG.create () in 
  GG.add_vertex g 1 ; 
  GG.add_vertex g 2 ; 
  GG.add_vertex g 3 ; 
  GG.add_vertex g 4 ;
  GG.add_vertex g 5 ;
  GG.add_vertex g 6 ;
  GG.add_vertex g 7 ;
  GG.add_vertex g 8 ;
  GG.add_vertex g 9 ;
  GG.add_vertex g 10 ;
  GG.add_vertex g 11 ;
  GG.add_vertex g 12 ;
  GG.add_vertex g 13 ;
  GG.add_vertex g 14 ;
  GG.add_vertex g 15 ;
  GG.add_vertex g 16 ;
  GG.add_vertex g 17 ;
  GG.add_vertex g 18 ;
  GG.add_vertex g 19 ;
  GG.add_vertex g 20 ;
  GG.add_vertex g 21 ;
  GG.add_vertex g 22 ;
  GG.add_vertex g 23 ;
  GG.add_vertex g 24 ;
  GG.add_edge g 1 2 ; 
  GG.add_edge g 3 2 ; 
  GG.add_edge g 13 2 ; 
  GG.add_edge g 13 12 ; 
  GG.add_edge g 9 6 ; 
  GG.add_edge g 5 6 ; 
  GG.add_edge g 5 4 ; 
  GG.add_edge g 5 7 ; 
  GG.add_edge g 10 7 ; 
  GG.add_edge g 8 7 ; 
  GG.add_edge g 15 19 ; 
  GG.add_edge g 15 17 ; 
  GG.add_edge g 16 17 ; 
  GG.add_edge g 18 17 ; 
  GG.add_edge g 18 14 ; 
  GG.add_edge g 22 21 ; 
  GG.add_edge g 23 21 ; 
  GG.add_edge g 23 20 ; 
  GG.add_edge g 24 21 ; 
  print_list (TG.has_cycle g)  
