
(* Word Graph

   Given a number of letters n and a file containing words (one per line),
   this program builds the undirected graph where
   - vertices are words of length n;
   - two words are connected with an edge if they differ by exactly one letter.

   Then the program computes and prints the components.

   Finally, it repeatedly queries a word from the user and displays
   its component.
*)

open Format
open Graph
module H = Hashtbl.Make(String)

module G = Imperative.Graph.Abstract(String)
let g = G.create ()

let words : G.V.t H.t = H.create 16

let add_word w =
  let v = G.V.create w in H.add words w v; G.add_vertex g v

let rec read_words n c =
  match input_line c with
  | s -> if String.length s = n then add_word s; read_words n c
  | exception End_of_file -> ()

let () =
  try match Sys.argv with [| _; n; f |] ->
  let n = int_of_string n in
  let c = open_in f in
  read_words n c;
  close_in c;
  | _ -> raise Exit
  with _ -> eprintf "%s <int> <file>@." Sys.argv.(0); exit 1

let () = printf "%d words@." (G.nb_vertex g)

let diff1 s1 s2 =
  let n = String.length s1 in
  assert (String.length s2 = n);
  let rec scan d i =
     i = n && d = 1 ||
     i < n && if s1.[i] = s2.[i] then scan d (i+1) else d = 0 && scan 1 (i+1) in
  scan 0 0

let () =
  G.iter_vertex (fun v1 ->
  G.iter_vertex (fun v2 ->
  if diff1 (G.V.label v1) (G.V.label v2) then G.add_edge g v1 v2
  ) g) g

let () = printf "%d edges@." (G.nb_edges g)

module C = Components.Undirected(G)
let comp = C.components_array g

let histogram a =
  let h = Hashtbl.create (Array.length a) in
  let incr v =
    Hashtbl.replace h v (1 + try Hashtbl.find h v with Not_found -> 0) in
  Array.iter incr a;
  let l = Hashtbl.fold (fun v n acc -> (v, n) :: acc) h [] in
  List.sort (fun (v1, _) (v2, _) -> compare v1 v2) l

let () =
  printf "%d components@." (Array.length comp);
  Array.sort (fun l1 l2 -> Stdlib.compare (List.length l1) (List.length l2))
    comp;
  let print1 v = printf "@ %s" (G.V.label v) in
  let print c =
    printf "@[<hov 2>%d:" (List.length c); List.iter print1 c; printf "@]@." in
  Array.iter print comp;
  let hist = histogram (Array.map List.length comp) in
  List.iter (fun (v, n) -> printf "%d component(s) of size %d@." n v) hist

module D = Traverse.Dfs(G)

let () =
  while true do
    printf "start: @?";
    let s = read_line () in
    try
      let v = H.find words s in
      printf "@[<hov 2>component:";
      let m = ref 0 in
      let visit v = printf "@ %s" (G.V.label v); incr m in
      D.prefix_component visit g v;
      printf "@]@.";
      printf "%d word(s)@." !m
    with Not_found ->
      printf "not a vertex@."
  done
