(* module type OrderedType = *)
(* sig *)
(*   type t *)
(*   val compare : t -> t -> int *)
(*   val hash : t -> int *)
(*   val equal : t -> t -> bool *)
(* end *)

(* module type VertexType = *)
(* sig *)
(*   (\* For hash tables and sets *\) *)
(*   include OrderedType *)

(*   (\* For dot files *\) *)
(*   val label : t -> string *)
(* end *)


(* (\* This functor yields a simple instance of the abstract model class. *)
(*    It only needs compare, equal, hash and label on vertices. *)
(* *\) *)
(* module Make(V : VertexType) = struct *)

(*   (\* Node module : comparison, equality, hash, label, layout *\) *)
(*   module N = struct *)
(*     type t = { *)
(*       vertex : V.t; *)
(*       mutable layout : node_layout option *)
(*     } *)
(*     let compare n n' = V.compare n.vertex n'.vertex *)
(*     let equal n n' = V.equal n.vertex n'.vertex *)
(*     let hash n = V.hash n.vertex *)

(*     let label n = V.label n.vertex *)
(*     let layout n = n.layout *)

(*     let set_layout n l = n.layout <- Some l *)
(*     let get_layout n = match n.layout with *)
(*       | Some l -> l *)
(*       | None   -> raise Not_found *)
(*   end *)

(*   (\* Edge module : comparison, equality, hash, label, layout *\) *)
(*   module E = *)
(*     struct *)

(*       type t = { *)
(* 	v1 : V.t; *)
(* 	v2 : V.t; *)
(* 	mutable layout : edge_layout option *)
(*       } *)

(*       let compare e e' = *)
(* 	let cmp = V.compare e.v1 e'.v1 in *)
(* 	if cmp <> 0 then cmp *)
(* 	else V.compare e.v2 e'.v2 *)

(*       let equal e e' = *)
(* 	V.equal e.v1 e'.v1 && V.equal e.v2 e'.v2 *)

(*       let hash e = Hashtbl.hash (V.hash e.v1, V.hash e.v2) *)
(*       let layout e = e.layout *)
(*       let set_layout e l = e.layout <- Some l *)
(*       let get_layout e = match e.layout with *)
(* 	| Some l -> l *)
(* 	| None   -> raise Not_found *)
(*     end *)

(*   (\* Node Set, Node Hashtbl *\) *)
(*   module NS = Set.Make(N) *)
(*   module NH = Hashtbl.Make(N) *)

(*   (\* Concrete class *\) *)
(*   class model graph = object(self) *)
(*     inherit [N.t, E.t] virtual_model *)

(*     val g = graph *)

(*     method copy = new model (NH.copy g) *)

(*     method label = N.label *)

(*     (\** Membership functions *\) *)

(*     method mem_node n = NH.mem g n *)

(*     method mem_edge n1 n2 = *)
(*       try NS.mem n2 (NH.find g n1) *)
(*       with Not_found -> false *)

(*     method mem_edge_e (n1,n2) = self#mem_edge n1 n2 *)

(*     method find_edge n1 n2 = *)
(*       if self#mem_edge n1 n2 then (n1,n2) *)
(*       else raise Not_found *)

(*     (\** Iterators *\) *)

(*     method iter_nodes f = NH.iter (fun n _ -> f n) g *)
(*     method iter_edges f = NH.iter (fun n s -> NS.iter (f n) s) g *)
      
(*     method iter_succ f n = NS.iter f (NH.find g n) *)
(*     method iter_succ_e f n = self#iter_succ (fun n' -> f (n,n')) n *)

(*     method iter_pred f n = *)
(*       if not (self#mem_node n) then invalid_arg "[dgraph] iter_pred"; *)
(*       self#iter_edges (fun n1 n2 -> if N.equal n n2 then f n1) *)

(*     method iter_pred_e f n = self#iter_pred (fun n' -> f (n',n)) n *)
 
(*     (\** Insertion and removal *\) *)

(*     method add_node n = NH.add g n NS.empty *)

(*     method remove_edge n1 n2 = *)
(*       NH.replace g n1 (NS.remove n2 (NH.find g n1)) *)

(*     method remove_node n = *)
(*       self#iter_succ (fun n' -> self#remove_edge n n') n; *)
(*       self#iter_pred (fun n' -> self#remove_edge n' n) n; *)
(*       NH.remove g n *)

(*     (\* Layout *\) *)

(*     method set_node_layout = N.set_layout *)
(*     method get_node_layout = N.get_layout *)

(*     method get_edge_layout = E.get_layout *)
(*     method set_edge_layout = E.set_layout *)

(*   end *)

(*   let model ?(size=97) () = new model (NH.create size) *)

(* end *)
