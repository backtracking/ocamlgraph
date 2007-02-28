#load "../graph.cma";;
#directory "..";;
open Graph.Pack.Graph;;
let g = Classic.de_bruijn 4;;
let () = print_gml_file g "tests/de_bruijn4.gml";;



