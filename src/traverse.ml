(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Graph traversal *)

module type G = sig
  val is_directed : bool
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

(* depth-first search *)
module Dfs(G : G) = struct
  module H = Hashtbl.Make(G.V)

  let fold f acc g =
    let h = H.create 97 in
    let s = Stack.create () in
    let rec loop acc =
      if not (Stack.is_empty s) then
        let v = Stack.pop s in
        if not (H.mem h v) then begin
          H.add h v ();
          let acc = f v acc in
          G.iter_succ (fun w -> Stack.push w s) g v;
          loop acc
        end else
          loop acc
      else
        acc
    in
    G.fold_vertex (fun v acc -> Stack.push v s; loop acc) g acc

  let iter ?(pre=fun _ -> ()) ?(post=fun _ -> ()) g =
    let h = H.create 97 in
    let rec visit v =
      if not (H.mem h v) then begin
        H.add h v ();
        pre v;
        G.iter_succ visit g v;
        post v
      end
    in
    G.iter_vertex visit g

  let postfix post g = iter ~post g

  let fold_component f acc g v0 =
    let h = H.create 97 in
    let s = Stack.create () in
    Stack.push v0 s;
    let rec loop acc =
      if not (Stack.is_empty s) then
        let v = Stack.pop s in
        if not (H.mem h v) then begin
          H.add h v ();
          let acc = f v acc in
          G.iter_succ (fun w -> Stack.push w s) g v;
          loop acc
        end else
          loop acc
      else
        acc
    in
    loop acc

  let iter_component ?(pre=fun _ -> ()) ?(post=fun _ -> ()) g v =
    let h = H.create 97 in
    let rec visit v =
      H.add h v ();
      pre v;
      G.iter_succ (fun w -> if not (H.mem h w) then visit w) g v;
      post v
    in
    visit v

  let postfix_component post g = iter_component ~post g

  module Tail = struct

    let has_cycle g =
      let h = H.create 97 in
      let stack = Stack.create () in
      let loop () =
        while not (Stack.is_empty stack) do
          let v = Stack.top stack in
          if H.mem h v then begin
            (* we are now done with node v *)
            (* assert (H.find h v = true); *)
            H.replace h v false;
            ignore (Stack.pop stack)
          end else begin
            (* we start DFS from node v *)
            H.add h v true;
            G.iter_succ
              (fun w ->
                 try if H.find h w then raise Exit
                 with Not_found -> Stack.push w stack)
              g v;
          end
        done
      in
      try
        G.iter_vertex
          (fun v ->
             if not (H.mem h v) then begin Stack.push v stack; loop () end)
          g;
        false
      with Exit ->
        true

    let has_cycle_undirected g =
      let h = H.create 97 in
      let father = H.create 97 in
      let is_father u v = (* u is the father of v in the DFS descent *)
        try G.V.equal (H.find father v) u with Not_found -> false
      in
      let stack = Stack.create () in
      let loop () =
        while not (Stack.is_empty stack) do
          let v = Stack.top stack in
          if H.mem h v then begin
            (* we are now done with node v *)
            (* assert (H.find h v = true); *)
            H.remove father v;
            H.replace h v false;
            ignore (Stack.pop stack)
          end else begin
            (* we start DFS from node v *)
            H.add h v true;
            G.iter_succ
              (fun w ->
                 try if H.find h w && not (is_father w v) then raise Exit
                 with Not_found -> H.add father w v; Stack.push w stack)
              g v;
          end
        done
      in
      try
        G.iter_vertex
          (fun v ->
             if not (H.mem h v) then begin Stack.push v stack; loop () end)
          g;
        false
      with Exit ->
        true

    let has_cycle g =
      if G.is_directed then has_cycle g else has_cycle_undirected g

    let iter f g =
      let h = H.create 97 in
      let stack = Stack.create () in
      let loop () =
        while not (Stack.is_empty stack) do
          let v = Stack.pop stack in
          if not (H.mem h v) then begin
            H.add h v ();
            f v;
            G.iter_succ
              (fun w -> if not (H.mem h w) then Stack.push w stack) g v
          end
        done
      in
      G.iter_vertex
        (fun v ->
           if not (H.mem h v) then begin Stack.push v stack; loop () end)
        g

    let iter_component f g v0 =
      let h = H.create 97 in
      let stack = Stack.create () in
      Stack.push v0 stack;
      while not (Stack.is_empty stack) do
        let v = Stack.pop stack in
        if not (H.mem h v) then begin
          H.add h v ();
          f v;
          G.iter_succ (fun w -> if not (H.mem h w) then Stack.push w stack) g v
        end
      done

  end

  let prefix = Tail.iter
  let has_cycle = Tail.has_cycle
  let prefix_component = Tail.iter_component

  (* step-by-step iterator *)
  module S = Set.Make(G.V)

  type iterator = S.t * G.V.t list * G.t
  (** (h, st, g) where h is the set of marked vertices and st the stack
      invariant: the first element of st is not in h i.e. to be visited *)

  let start g =
    let st = G.fold_vertex (fun v st -> v :: st) g [] in
    S.empty, st, g

  let get (_,st,_) = match st with
    | [] -> raise Exit
    | v :: _  -> v

  let step (s,st,g) = match st with
    | [] ->
      raise Exit
    | v :: st ->
      let s' = S.add v s in
      let st' = G.fold_succ (fun w st -> w :: st) g v st in
      let rec clean = function
        | w :: st when S.mem w s' -> clean st
        | st -> st
      in
      (s', clean st', g)

end

(* breadth-first search *)
module Bfs(G : G) = struct
  module H = Hashtbl.Make(G.V)

  let fold f i (g : G.t) =
    let h = H.create 97 in
    let q = Queue.create () in
    (* invariant: [h] contains exactly the vertices which have been pushed *)
    let push v =
      if not (H.mem h v) then begin H.add h v (); Queue.add v q end
    in
    let rec loop s =
      if not (Queue.is_empty q) then
        let v  = Queue.pop q in
        let ns = f v s in               (* Sticking to OCamlGraph's fold conv *)
        G.iter_succ push g v;
        loop ns
      else
        s
    in
    G.fold_vertex (fun v s -> push v; loop s) g i

  let iter f = fold (fun v () -> f v) ()

  let fold_component f i g v0 =
    let h = H.create 97 in
    let q = Queue.create () in
    (* invariant: [h] contains exactly the vertices which have been pushed *)
    let push v =
      if not (H.mem h v) then begin H.add h v (); Queue.add v q end
    in
    push v0;
    let rec loop s =
      if not (Queue.is_empty q) then
        let v  = Queue.pop q in
        let ns = f v s in
        G.iter_succ push g v;
        loop ns
      else
        s
    in
    loop i

  let iter_component f = fold_component (fun v () -> f v) ()

  (* with distance from the source

     instead of using a queue, we use two bags
     (`todo` with vertices at distance `d`
      and `next` with vertices at distance `d+1`*)

  let fold_component_dist f acc g v0 =
    let h = H.create 97 in
    (* invariant: [h] contains exactly the vertices
       which have been pushed *)
    let push v next =
      if H.mem h v then next
      else (H.add h v (); v :: next) in
    let rec loop acc d next = function
      | [] -> if next = [] then acc
              else loop acc (d+1) [] next
      | v :: todo ->
        let acc = f v d acc in
        let next = G.fold_succ push g v next in
        loop acc d next todo in
    loop acc 0 [] (push v0 [])

  let iter_component_dist f =
    fold_component_dist (fun v d () -> f v d) ()

  (* step-by-step iterator *)

  (* simple, yet O(1)-amortized, persistent queues *)
  module Q = struct
    type 'a t = 'a list * 'a list
    exception Empty
    let empty = [], []
    let is_empty = function [], [] -> true | _ -> false
    let push x (i,o) = (x :: i, o)
    let pop = function
      | i, y :: o -> y, (i,o)
      | [], [] -> raise Empty
      | i, [] -> match List.rev i with
        | x :: o -> x, ([], o)
        | [] -> assert false
    let peek q = fst (pop q)
  end

  module S = Set.Make(G.V)

  (* state is [(s,q,g)] : [s] contains elements never been pushed in [q] *)
  type iterator = S.t * G.V.t Q.t * G.t

  let start g =
    let s = G.fold_vertex S.add g S.empty in
    s, Q.empty, g

  let get (s,q,_) =
    if Q.is_empty q then
      if S.is_empty s then raise Exit else S.choose s
    else
      Q.peek q

  let step (s,q,g) =
    let push v (s,q as acc) =
      if S.mem v s then
        S.remove v s, Q.push v q
      else
        acc
    in
    let v,s',q' =
      if Q.is_empty q then begin
        if S.is_empty s then raise Exit;
        let v = S.choose s in
        v, S.remove v s, q
      end else
        let v,q' = Q.pop q in
        v, s, q'
    in
    let s'',q'' = G.fold_succ push g v (s',q') in
    (s'',q'',g)

end


(* Graph traversal with marking. *)

module type GM = sig
  type t
  module V : sig type t end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  module Mark : sig
    val clear : t -> unit
    val get : V.t -> int
    val set : V.t -> int -> unit
  end
end

module Mark(G : GM) = struct

  let dfs g =
    G.Mark.clear g;
    let n = ref 0 in
    let rec visit v =
      if G.Mark.get v = 0 then begin
        incr n;
        G.Mark.set v !n;
        G.iter_succ visit g v
      end
    in
    G.iter_vertex visit g

  (* invariant: [h v = 0] means not visited at all; [h v = 1] means
     already visited in the current component; [h v = 2] means
     already visited in another tree *)
  let has_cycle g =
    G.Mark.clear g;
    let rec visit v =
      G.Mark.set v 1;
      G.iter_succ
        (fun w ->
           let m = G.Mark.get w in
           if m = 1 then raise Exit;
           if m = 0 then visit w)
        g v;
      G.Mark.set v 2
    in
    try G.iter_vertex (fun v -> if G.Mark.get v = 0 then visit v) g; false
    with Exit -> true

end

