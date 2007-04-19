#load "../graph.cma";;
#directory "..";;
open Graph.Pack.Graph;;

let g = Rand.graph ~v:50 ~e:300 ()
let () = print_gml_file g "tests/rand_50_300.gml";;

(*
let g = Classic.full 50;;
let () = print_gml_file g "tests/full50.gml";;
*)


