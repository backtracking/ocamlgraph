
(** A quick hack to visualize search algorithms (see module Search)

   The graph is a grid, where colors meaning is as follows:
   - gray : empty cells i.e. graph vertices
   - red  : start point (always 0,0)
   - blue : target points (possibly many)
   - black: blocked points i.e. not graph vertices

   Edit the graph by clicking on cells, which rotate Empty->Blocked->Target.

   Each cell is connected to its 8 neighbors.

   Run a search by typing:
   - 'd' for DFS
   - 'b' for BFS
   - 'i' for IDS (typically takes too much time)
   - 'j' for Dijkstra
   - 'a' for A*
*)

open Graphics
open Graph

let n = ref 20
let m = ref 20

let () =
  Arg.parse [
      "-n", Arg.Set_int n, "<int> set width (default 20)";
      "-m", Arg.Set_int m, "<int> set height (default 20)";
    ]
    (fun _ -> raise (Arg.Bad ""))
    "show_search [options]"
let n = !n
let m = !m

let step = 600 / max n m
let () = open_graph " 800x600"

let lightgray = rgb 200 200 200
let draw i j c =
  set_color c;
  let y = step * j and x = step * i in
  fill_rect (x+1) (y+1) (step-2) (step-2)

type cell = Empty | Start | Target | Blocked
let color = function
  | Start -> red
  | Empty -> lightgray
  | Blocked -> black
  | Target -> blue
let rotate = function
  | Start -> Start
  | Empty -> Blocked
  | Blocked -> Target
  | Target -> Empty

let grid = Array.make_matrix n m Empty
let draw_cell i j = draw i j (color grid.(i).(j))
let redraw () =
  for i = 0 to n-1 do for j = 0 to m-1 do draw_cell i j done done

let show (i,j) =
  draw i j magenta;
  Unix.sleepf 0.01

module G = struct
  module I = struct include Int let hash x = x end
  include Imperative.Digraph.Concrete(Util.CMPProduct(I)(I))
  let fold_succ_e f g v acc = show v; fold_succ_e f g v acc
  let success _ (i,j) = grid.(i).(j) = Target
end
module C = struct
  include Int
  type edge = G.E.t
  let weight _e = 1
end
module H = struct
  let heuristic (si,sj) =
    let h = ref (n*m) in
    for i = 0 to n-1 do
      for j = 0 to m-1 do
        if grid.(i).(j) = Target then
          h := min !h (abs (i - si) + abs (j - sj))
      done
    done;
    Format.eprintf "h(%d,%d) = %d@." si sj !h;
    !h
end

let g = G.create ()
let add_succ (i,j as v) =
  if G.mem_vertex g v then (
    for di = -1 to +1 do for dj = -1 to +1 do
      if (di <> 0 || dj <> 0) && G.mem_vertex g (i+di,j+dj) then
        G.add_edge g (i,j) (i+di,j+dj)
    done done
  )
let () =
  for i = 0 to n-1 do for j = 0 to m-1 do G.add_vertex g (i,j) done done;
  for i = 0 to n-1 do for j = 0 to m-1 do add_succ (i,j) done done

module Dfs = Search.DFS(G)
module Bfs = Search.BFS(G)
module Ids = Search.IDS(G)
module Dij = Search.Dijkstra(G)(C)
module Ast = Search.Astar(G)(C)(H)

let set i j k =
  grid.(i).(j) <- k;
  draw_cell i j;
  match k with
  | Blocked -> G.remove_vertex g (i,j)
  | _ -> G.add_vertex g (i,j);
         add_succ (i-1,j); add_succ (i,j-1); add_succ (i,j)

let () = set 0 0 Start
let () = set (n-1) (m-1) Target

let run search =
  (try let _ = search g (0,0) in ()
   with Not_found -> Format.eprintf "no solution@.");
  ignore (read_key ());
  redraw ()

let () =
  redraw ();
  while true do
    let st = wait_next_event [Button_down; Key_pressed] in
    if st.keypressed then match st.key with
      | 'q' -> close_graph (); exit 0
      | 'b' -> run Bfs.search
      | 'd' -> run Dfs.search
      | 'i' -> run Ids.search
      | 'j' -> run Dij.search
      | 'a' -> run Ast.search
      | _   -> ()
    else if st.button then (
      let i = st.mouse_x / step in
      let j = st.mouse_y / step in
      set i j (rotate grid.(i).(j))
    )
  done
