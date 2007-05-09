
open Graph
open Ed_hyper
open Ed_graph

let make_subgraph l =
  let gl = G.create () in
  List.iter (fun v -> G.add_vertex gl v) l;
  List.iter 
    (fun v ->
       List.iter (fun w -> 
		    if edge v w 
		    then G.add_edge gl v w) 
	 l) 
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
	    (fun m v -> 
	       if G.out_degree gc v < G.out_degree gc m 
	       then v 
	       else m)
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


(* Limit of visibility for nodes *)
let rlimit = 0.90 
let rlimit_sqr = rlimit *. rlimit


(* Depth First Search drawing *)

let rec draw_dfs depth node turtle =
  let lab = G.V.label node in
  lab.turtle <- turtle;
  lab.depth <- depth;
  if hspace_dist_sqr turtle <= rlimit_sqr then begin
    lab.visible <- Visible;
    let l = G.succ !graph node in 
    let l = List.filter (fun x -> (G.V.label x).visible = Hidden) l in
    List.iter (fun w -> (G.V.label w).visible <- BorderNode) l;
    let l = order_children l in
    let n = List.length l in
    if n > 0 then begin
      let distance = step_from (max 3 n)
      and angle = (if depth = 0 then 2. else 1.) *. pi /. (float_of_int n) in
      let turtle = 
	if depth = 0 then turtle else turn_right turtle ((pi -. angle) /. 2.) 
      in
      let _ = draw_edges_dfs node (depth+1) turtle distance angle l in
      ()
    end
  end

and draw_edges_dfs node depth turtle distance angle = function
  | [] -> 
      []
  | v :: l -> 
      let steps = 10 in
      let tv = advance_many turtle distance steps in 
      let turtle = turn_left turtle angle in
      let l = (v,tv) :: draw_edges_dfs node depth turtle distance angle l in
      draw_dfs depth v tv;
      l



(* Breadth First Search drawing *)



(* Drawing graph function *)
let draw_graph root turtle =
  G.iter_vertex (fun v -> let l = G.V.label v in l.visible <- Hidden) !graph;
  draw_dfs 0 root turtle

