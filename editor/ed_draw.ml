
open Graph
open Ed_hyper
open Ed_graph

let make_subgraph l =
  let gl = G.create () in
  List.iter (fun v -> G.add_vertex gl v) l;
  List.iter 
    (fun v -> List.iter (fun w -> if edge v w then G.add_edge gl v w) l) 
   l; 
  (* TODO: efficacite *)
  gl

let order_children l =
  let gl = make_subgraph l in
  let scc = Components.scc_list gl in
  let order_component c =
    let gc = make_subgraph c in
    let v = match c with
      | v :: l ->
	  List.fold_left 
	    (fun m v -> if G.out_degree gc v < G.out_degree gc m then v else m)
	    v l
      | [] -> 
	  assert false
    in 
    let l = ref [] in
    Dfs.prefix_component (fun w -> l := w :: !l) gc v;
    !l
  in
  let scc = List.map order_component scc in
  List.flatten scc

let rlimit = 0.90 
let rlimit_sqr = rlimit *. rlimit

