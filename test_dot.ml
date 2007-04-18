open Graph.Pack.Graph

let g = parse_dot_file Sys.argv.(1)
let () = dot_output g "tmp.dot"
let _ = Sys.command "dot -Tps tmp.dot | gv -"

