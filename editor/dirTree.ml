open Unix ;;

type t = {
    name : string ;
    children : t list Lazy.t
  } ;;

type label = string ;;

let children t = Lazy.force t.children ;;
let label t = t.name ;;
let string_of_label s = s ;;

let is_dir path =
  try (lstat path).st_kind = S_DIR
  with Unix_error _ -> false ;;

let less t1 t2 = t1.name <= t2.name ;;

let rec tree_list_from_path path =
  let opt_handle =
    try Some(opendir path)
    with Unix_error _ -> None in
  match opt_handle with
  | None -> []
  | Some handle ->
      let list = Sort.list less (tree_list_from_handle path handle) in
      closedir handle ; list

and tree_list_from_handle path handle =
  let opt_name =
    try Some(readdir handle)
    with End_of_file -> None in
  match opt_name with
  | None -> []
  | Some name ->
      let path' = path ^ "/" ^ name in
      if name <> "." && name <> ".." && is_dir path' then
	{ name = name ; children = lazy (tree_list_from_path path') } ::
	tree_list_from_handle path handle
      else
	tree_list_from_handle path handle ;;

let from_dir path name =
  try
    let path' = path ^ "/" ^ name in
    if is_dir path' then
      { name = name ;
	children = lazy (tree_list_from_path path') }
    else invalid_arg "DirTree.from_dir"
  with _ -> failwith "DirTree.from_dir" ;;
