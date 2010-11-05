(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph

let ($) f x = f x
let get_some = function None -> assert false | Some t -> t

type cluster = string
type status = Global | Tree | Both

(* ABSTRACT CLASS *)

class type
    ['vertex, 'edge, 'cluster, 'tree_vertex, 'tree_edge, 'tree_cluster]
      view_container_type =
object
  method content : GPack.paned
  method global_view :
    ('vertex, 'edge, 'cluster) DGraphView.view option
  method tree_view :
    ('tree_vertex, 'tree_edge, 'tree_cluster) DGraphView.view option
  method set_tree_view : 'vertex -> unit
  method depth_backward : int
  method depth_forward : int
  method set_depth_backward : int -> unit
  method set_depth_forward : int -> unit
  method status : status
  method switch : status -> unit
  method adapt_zoom: unit -> unit
end

module type S = sig

  type graph
  type vertex
  type edge

  module Tree: Sig.G with type V.label = vertex

  module GView: DGraphView.S with type vertex = vertex
			     and type edge = edge
			     and type cluster = cluster

  module TView: DGraphView.S with type vertex = Tree.V.t
			     and type edge = Tree.E.t
			     and type cluster = cluster

  type global_view = (vertex, edge, cluster) DGraphView.view
  type tree_view = (Tree.V.t, Tree.E.t, cluster) DGraphView.view

  class view_container :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> mk_global_view: (unit -> global_view)
      -> mk_tree_view:
	(depth_backward:int -> depth_forward:int -> Gtk.widget Gtk.obj -> vertex
	 -> tree_view)
	-> vertex option
	  -> [ vertex, edge, cluster, Tree.V.t, Tree.E.t, cluster]
	    view_container_type

end

(* CONTAINER *)

let with_commands ?packing mk_view model =
  let main_table = GPack.table
    ~columns:2
    ~rows:2
    ?packing () in

  (* Viewer *)
  let view =
    mk_view
      ~packing:(fun w ->
	main_table#attach
	  ~left:0
	  ~right:2
	  ~top:1
	  ~expand:`BOTH
	  w)
      model
  in

  (* View controls *)
  let button_top_box =
    GPack.button_box
      `HORIZONTAL
      ~border_width:3
      ~child_height:10
      ~child_width:85
      ~spacing:10
      ~layout:`START
      ~packing:(main_table#attach ~top:0 ~left:0 ~expand:`X) ()
  in
  let view_label = GMisc.label ~markup:"<b>View</b>" () in
  button_top_box#pack ~expand:false view_label#coerce;
  let button_global_view =
    GButton.button ~label:"Global" ~packing:button_top_box#pack ()
  in
  let button_tree_view =
    GButton.button ~label:"Tree" ~packing:button_top_box#pack ()
  in
  let button_paned_view =
    GButton.button ~label:"Both" ~packing:button_top_box#pack ()
  in
  ignore $ button_global_view#connect#clicked
    ~callback:(fun _ -> view#switch Global) ;
  ignore $ button_tree_view#connect#clicked
    ~callback:(fun _ -> view#switch Tree) ;
  ignore $ button_paned_view#connect#clicked
    ~callback:(fun _ -> view#switch Both) ;

  (* Depth of exploration controls *)
  let depth_hbox = GPack.hbox
    ~packing:(main_table#attach ~expand:`X ~top:0 ~left:1) ()
  in
  let depth_forward_adj = GData.adjustment ~lower:0. ~page_size:0. () in
  let depth_backward_adj = GData.adjustment ~lower:0. ~page_size:0. () in
  let change_depth_forward adj content () =
    content#set_depth_forward (int_of_float adj#value);
  in
  let change_depth_backward adj content () =
    content#set_depth_backward (int_of_float adj#value)
  in
  ignore $ depth_forward_adj#connect#value_changed
    ~callback:(change_depth_forward depth_forward_adj view);
  ignore $ depth_backward_adj#connect#value_changed
    ~callback:(change_depth_backward depth_backward_adj view);
  let depth_label = GMisc.label ~markup:"<b>Depth</b>" () in
  let depth_forward_label = GMisc.label ~text:" forward: " () in
  let depth_backward_label = GMisc.label ~text:" backward: " () in
  let depth_forward_spin =
    GEdit.spin_button ~value:2. ~adjustment:depth_forward_adj ()
  in
  let depth_backward_spin =
    GEdit.spin_button ~value:2. ~adjustment:depth_backward_adj ()
  in
  depth_hbox#pack ~from:`END depth_backward_spin#coerce;
  depth_hbox#pack ~from:`END depth_backward_label#coerce;
  depth_hbox#pack ~from:`END depth_forward_spin#coerce;
  depth_hbox#pack ~from:`END depth_forward_label#coerce;
  depth_hbox#pack ~from:`END depth_label#coerce;

  main_table, view;;

(* FROM GRAPH *)

module HString = struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end

module Build
  (G: Sig.G)
  (T: DGraphTreeModel.S with type Tree.V.label = G.V.t) =
struct

  type graph = G.t
  type vertex = G.V.t
  type edge = G.E.t
  module TreeModel = T
  module Tree = T.Tree

  module HE(E: sig type t val compare: t -> t -> int end) = struct
    type t = E.t
    let equal x y = E.compare x y = 0
    let hash = Hashtbl.hash
  end
  module GView = DGraphView.Make(G.V)(HE(G.E))(HString)
  module TView = DGraphView.Make(Tree.V)(HE(Tree.E))(HString)

  type global_view = (G.V.t, G.E.t, string) DGraphView.view
  type tree_view = (Tree.V.t, Tree.E.t, string) DGraphView.view

  class view_container
    ?packing
    ?(status=Global)
    ~mk_global_view
    ~mk_tree_view
    default_tree_root
    =
    (* widgets *)
    let paned_window = GPack.paned `VERTICAL ?packing () in
    let global_frame = GBin.frame ~label:"Global View" () in
    let tree_frame = GBin.frame ~label:"Tree View" () in
    let scrolled_global_view =
      GBin.scrolled_window
	~hpolicy:`AUTOMATIC
	~vpolicy:`AUTOMATIC
	~packing:global_frame#add
	()
    in
    let scrolled_tree_view =
      GBin.scrolled_window
	~hpolicy:`AUTOMATIC
	~vpolicy:`AUTOMATIC
	~packing:tree_frame#add
	()
    in
    (* Callback functions *)
    let connect_tree_callback obj node =
      let callback = function
	| `BUTTON_PRESS _ ->
	  obj#set_tree_view (T.Tree.V.label node#item);
	  false
	|_ -> false
      in node#connect_event ~callback
    in
    let connect_global_callback obj node =
      let callback = function
	| `BUTTON_PRESS _ ->
	  (match obj#status with
	  | Global -> ()
	  | Both ->
	    obj#set_tree_view node#item;
	    let tree_view = get_some obj#tree_view in
	    tree_view#adapt_zoom ()
	  | Tree -> assert false);
	  false
	|_ -> false
      in node#connect_event ~callback
    in
    let connect_switch_tree_callback obj node =
      let global_view : global_view = get_some obj#global_view in
      let tree = T.tree () in
      let gnode =
	global_view#get_node
	  (T.TreeManipulation.get_graph_vertex node#item tree)
      in
      let callback = function
	| `MOTION_NOTIFY _ ->
	  global_view#highlight gnode;
	  false
	| `LEAVE_NOTIFY _ | `BUTTON_PRESS _ ->
	  global_view#dehighlight gnode;
	  false
	|_ ->
	  false
      in
      node#connect_event ~callback
    in
    let connect_switch_global_callback obj node =
      let tree_view : tree_view = get_some obj#tree_view in
      let tree = T.tree () in
      let vertices = T.TreeManipulation.get_tree_vertices node#item tree in
      let apply f =
	List.iter (fun v -> f (tree_view#get_node v)) vertices;
	false
      in
      let callback = function
	| `MOTION_NOTIFY _ -> apply tree_view#highlight
	| `LEAVE_NOTIFY _ -> apply tree_view#dehighlight
	|_ -> false
      in
      node#connect_event ~callback
    in
  object (self)

    val mutable global_view: global_view option = None
    val mutable tree_view: tree_view option = None
    val mutable status = status
    val mutable depth_forward = 2
    val mutable depth_backward = 2

    (* Getters *)
    method status = status
    method global_view = global_view
    method tree_view = tree_view
    method content = paned_window
    method depth_forward = depth_forward
    method depth_backward = depth_backward

    (* Setters *)
    method set_depth_forward i = depth_forward <- i
    method set_depth_backward i = depth_backward <- i

    method set_tree_view root =
      if tree_view <> None then
	scrolled_tree_view#remove scrolled_tree_view#child;
      let view =
	mk_tree_view ~depth_backward ~depth_forward paned_window#as_widget root
      in
      scrolled_tree_view#add view#coerce;
      tree_view <- Some view;
      view#connect_highlighting_event();
      view#iter_nodes (connect_tree_callback self);
      if status = Both then begin
	view#iter_nodes (connect_switch_tree_callback self);
	(get_some global_view)#iter_nodes (connect_switch_global_callback self)
      end

    method private init_global_view () =
      let view = mk_global_view () in
      scrolled_global_view#add view#coerce;
      view#connect_highlighting_event ();
      view#iter_nodes (connect_global_callback self);
      global_view <- Some view;

    (* Switch *)
    method private switch_to_global_view () =
      if global_view = None then self#init_global_view ();
      (match status with
      | Global -> ()
      | Both ->
	status <- Global;
	paned_window#remove paned_window#child2
      | Tree ->
	status <- Global;
	paned_window#remove paned_window#child2;
	paned_window#pack1 global_frame#coerce);
      match global_view with None -> assert false | Some v -> v#adapt_zoom ()

    method private switch_to_tree_view () =
      match default_tree_root with
      | None -> ()
      | Some root ->
	if tree_view = None then self#set_tree_view root;
	(match status with
	| Tree -> ()
	| Both ->
	  status <- Tree;
	  paned_window#remove paned_window#child1
	| Global ->
	  status <- Tree;
	  paned_window#remove paned_window#child1;
	  paned_window#pack2 tree_frame#coerce);
	match tree_view with None -> () | Some t -> t#adapt_zoom ()

    method private switch_to_paned_view () =
      (match default_tree_root with
      | None -> self#switch_to_global_view ()
      | Some root ->
	if tree_view = None then self#set_tree_view root;
	if global_view = None then self#init_global_view ();
	(get_some tree_view)#iter_nodes (connect_switch_tree_callback self);
	(get_some global_view)#iter_nodes
	  (connect_switch_global_callback self);
	match status with
	| Both -> ()
	| Global ->
	  status <- Both;
	  paned_window#pack2 tree_frame#coerce
	| Tree ->
	  status <- Both;
	  paned_window#pack1 global_frame#coerce);
      self#adapt_zoom ()

    method switch = function
    | Global -> self#switch_to_global_view ()
    | Tree -> self#switch_to_tree_view ()
    | Both -> self#switch_to_paned_view ()

    method adapt_zoom () =
      let az = function None -> () | Some w -> w#adapt_zoom () in
      az tree_view;
      az global_view

    (* Constructor *)
    initializer
      match status, default_tree_root with
      | Global, _ | _, None ->
	status <- Global;
	self#init_global_view ();
	paned_window#pack1 global_frame#coerce
      | Tree, Some r ->
	status <- Tree;
	self#set_tree_view r;
	paned_window#pack2 tree_frame#coerce
      | Both, Some r ->
	status <- Both;
	self#init_global_view ();
	self#set_tree_view r;
	paned_window#pack1 global_frame#coerce;
	paned_window#pack2 tree_frame#coerce

  end

end

module Make(G: Graphviz.GraphWithDotAttrs) = struct

  module FullTreeModel = DGraphTreeModel.SubTreeMake(G)
  include Build(G)(FullTreeModel)
  module GlobalModel = DGraphModel.Make(G)

  let from_graph
      ?packing
      ?status
      ?(mk_global_view = fun model -> GView.view ~aa:true model)
      ?(mk_tree_view = fun model -> TView.view ~aa:true model)
      ?root
      g =
    let status = match status with
      | None ->
	if G.nb_vertex g < 500 && G.nb_edges g < 2500 then Global else Tree
      | Some s -> s
    in
    new view_container
      ?packing
      ~status
      ~mk_global_view:(fun () -> mk_global_view (GlobalModel.from_graph g))
      ~mk_tree_view:(fun ~depth_backward ~depth_forward w v ->
	let model =
	  FullTreeModel.from_graph ~depth_forward ~depth_backward w g v
	in
	mk_tree_view model)
      root

  let from_graph_with_commands
      ?packing ?status ?mk_global_view ?mk_tree_view ?root g =
    with_commands
      ?packing
      (fun ~packing g ->
	from_graph ~packing ?status ?mk_global_view ?mk_tree_view ?root g) g

end

(* FROM DOT *)

module Dot = struct

  include Build(DGraphModel.DotG)(DGraphTreeModel.SubTreeDotModelMake)

  exception Found of DGraphModel.DotG.V.t

  let from_dot
      ?packing
      ?status
      ?(mk_global_view = fun model -> GView.view ~aa:true model)
      ?(mk_tree_view = fun model -> TView.view ~aa:true model)
      dot_file =
    let gmodel =
      if Filename.check_suffix dot_file "xdot" then
	DGraphModel.read_xdot dot_file
      else
	DGraphModel.read_dot dot_file
    in
    let one_vertex =
      try
	gmodel#iter_vertex (fun v -> raise (Found v));
	None
      with Found v ->
	Some v
    in
    let status = match status with
      | None ->
	let nb f =
	  let cpt = ref 0 in
	  f gmodel (fun _ -> incr cpt);
	  !cpt
	in
	if nb (fun g -> g#iter_vertex) < 500
	  && nb (fun g -> g#iter_edges_e) < 2500
	then Global
	else Tree
      | Some s -> s
    in
    new view_container
      ?packing
      ~status
      ~mk_global_view:(fun () -> mk_global_view gmodel)
      ~mk_tree_view:(fun ~depth_backward ~depth_forward _ v ->
	mk_tree_view
	  (DGraphTreeModel.SubTreeDotModelMake.from_model
	     ~depth_forward
	     ~depth_backward
	     gmodel
	     v))
      one_vertex

  let from_dot_with_commands
      ?packing ?status ?mk_global_view ?mk_tree_view dot_file =
    with_commands
      ?packing
      (fun ~packing d ->
	from_dot ~packing ?status ?mk_global_view ?mk_tree_view d)
      dot_file

end
