type t
val id : t -> int

type label 

val children : t -> t list ;;
val label : t -> label ;;
val string_of_label : label -> string ;;

val from_dir : string -> string -> t ;;
