open Util

module type G = sig
  type t 
  module V : Sig.COMPARABLE 
  module E : sig 
    type t 
    type label 
    val label : t -> label 
    val dst : t -> V.t 
    val src : t -> V.t
    val compare : t -> t -> int
  end 
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t ->  unit
end

module Make
  (G: G)
  (W : Sig.ORDERED_TYPE with type t = G.E.label) =
struct

  module H = Hashtbl.Make(G.V)

  module Elt = struct
    type t = W.t * G.V.t

    (* weights are compared first, and minimal weights come first in the
       queue *)
    let compare (w1,v1) (w2,v2) =
      let cw = W.compare w2 w1 in
      if cw != 0 then cw else G.V.compare v1 v2
  end

  module Q = Heap.Imperative(Elt)


  let tmpfonc x = Printf.printf "%d\n" x; ()
  
  let spanningtree_from g r =   
    assert false (* TODO *)

  let spanningtree g =
    let r = ref None in
    try
      G.iter_vertex (fun v -> r := Some v; raise Exit) g;
      invalid_arg "spanningtree"
    with Exit ->
      spanningtree_from g !r


    (* let vertices = G.fold_vertex (fun v a -> v :: a) g [] in *)
    (* let v_visit = ref [] in *)

    (* let htable = Hashtbl.create 0 in *)
    

    (* let first_v = List.hd vertices in (\* choisi un premier sommet au hasard*\) *)


    (* let rempli_tbl x = *)
    (*   let edges = G.succ_e g x in (\* Toutes les arêtes*\) *)
    (*   let rec remp y = match y with  *)
    (* 	| [] -> () *)
    (* 	| e::l -> Hashtbl.add htable x e; remp y *)
    (*   in *)
    (*   remp edges *)
    (* in  *)
    (* rempli_tbl first_v; *)
(*
    let cover = ()
      cover
    in
*)

(*


    let vertices = G.fold_vertex (fun v a -> v :: a) g [] in
    let uf = UF.init vertices in

    let first_v = List.hd vertices in (* choisi un premier sommet au hasard*)
    let s = ref []  in (* liste des sommets visités initialisé au premier sommet choisi*)
    let first_edges = G.succ_e g first_v in (* Toutes les arrétes du premier sommet*)

    let v_min = ref first_v in
    let e_min = ref (List.hd first_edges) in (* arréte minimale initialisée par défaut à la première arréte trouvé*)
    


    let rec cover v = 
      let comp e = 
	if e <> !e_min then 
	  if G.E.compare e !e_min  < 0 then
	    if G.V.compare (UF.find !v_min uf) (UF.find (G.E.dst e) uf) <> 0 then begin (* si cet arc ne mène pas à un sommet déjà visité *)
	      e_min := e;
	      s := e::!s;
		
	    end

      in
      let list_e = G.succ_e g v in
      List.iter comp list_e;
      v_min := G.E.dst !e_min;
      List.iter cover (G.E.dst !e_min)    
    in

    cover !v_min;
    !s
*)
(*
    let cover e =
      let u, v = G.E.src e, G.E.dst e in
      if G.V.compare (UF.find u uf) (UF.find v uf) <> 0 then begin
	UF.union u v uf; 
	s := e :: !s
      end
    in
    List.iter cover edges;
    !s
*)
end


(*
Procédure PRIM
 Paramètres locaux : entier s, graphe G
 Paramètres globaux : graphe T
 Variables : 
  entier i, m, y
  réel : v
  ensemble : M
  TvectNent : pp
  TvectNReel : d 
Début 
1  T ← graphe_vide
2  M ← ensemble_vide
3  Pour i ← 0 jusqu'à N Faire
4    d[i] ←coût(s, i, G)
5    pp[i] ← s 
6    M ← Ajouter (i,M)
7  Fin pour
8  M ← Supprimer (s,M)
9  Tant que M <> Ensemble_vide Faire
10   m ← Choisir (M,d)
11   M ← Supprimer (m,M)
12   z ← pp[m]
13   v ← coût (m,z,G)
14   T ← Ajout arête <m,z> de coût v à T
15   Pour i ← 1 jusqu'à d° m dans G Faire
16     y ← i ième_succ_de m dans G
17     Si y  M et (cout(m,y,G) < d[y]) alors
18       d[y] ← coût(m,y,G)
19       pp[y] ← m
20     Fin Si
21   Fin Pour
22 Fin Tant que
Fin algo
*)



(*
  Local Variables: 
  compile-command: "make -C .. src/prim.cmo"
  End: 
*)
