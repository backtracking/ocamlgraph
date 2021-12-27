
open Format
open Graph

open Pack.Graph

let print_vertex fmt v =
  fprintf fmt "%d" (V.label v)
let print_edge fmt e =
  fprintf fmt "%a->%a" print_vertex (E.src e) print_vertex (E.dst e)
let print_path fmt p =
  List.iter (fun e -> fprintf fmt "%a " print_edge e) p

module G = Pack.Graph

let exists_path g =
  try ignore (Eulerian.path g); true with Invalid_argument _ -> false
let exists_cycle g =
  try ignore (Eulerian.cycle g); true with Invalid_argument _ -> false

let g = create ()
let add_vertex i = let v = V.create i in add_vertex g v; v
let path_length g = let p, _ = Eulerian.path g in List.length p

let v0 = add_vertex 0
let () = assert (exists_path g)
let () = assert (exists_cycle g)

let v1 = add_vertex 1
let () = assert (exists_path g)
let () = assert (exists_cycle g)

let () = add_edge g v0 v1
let () = assert (exists_path g)
let () = assert (not (exists_cycle g))
let () = assert (path_length g = 1)

let v2 = add_vertex 2
let () = add_edge g v1 v2
let () = assert (exists_path g)
let () = assert (not (exists_cycle g))
let () = assert (path_length g = 2)

let () = add_edge g v2 v0
let p, c = Eulerian.path g
let () = assert (exists_path g)
let () = assert (exists_cycle g)
let () = assert (path_length g = 3)

let () = add_edge g v0 v0
let () = assert (exists_cycle g)

let v3 = add_vertex 3
let () = add_edge g v2 v3
let () = assert (exists_path g)
let () = assert (not (exists_cycle g))
let () = assert (path_length g = 5)

let v4 = add_vertex 4
let () = add_edge g v3 v4
let () = add_edge g v2 v4
let () = assert (exists_cycle g)
let () = assert (path_length g = 7)

let () = remove_edge g v2 v4
let v5 = add_vertex 5
let () = add_edge g v4 v5
let () = add_edge g v5 v3
let () = assert (exists_path g)
let () = assert (not (exists_cycle g))
let () = assert (path_length g = 8)

let () = remove_edge g v2 v3 (* not connected anymore *)
let () = assert (not (exists_path g))

let () =
  for n = 2 to 5 do
    let g = Classic.full ~self:false  (2*n) in
    assert (not (exists_path g));
    let g = Classic.full ~self:true  (2*n) in
    assert (not (exists_path g));
    let g = Classic.full ~self:false (2*n+1) in
    let p, c = Eulerian.path g in
    assert c;
    assert (List.length p = n*(2*n+1));
    let g = Classic.full ~self:true (2*n+1) in
    let p, c = Eulerian.path g in
    assert c;
    assert (List.length p = (n+1)*(2*n+1))
   done
