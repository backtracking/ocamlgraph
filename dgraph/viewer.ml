(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
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
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

open Graph
open Printf

let ($) f x = f x

let debug = ref false

type vertex = OCamlGraph.Model.vertex
type edge = OCamlGraph.Model.edge

type state = {
  mutable file : string option;
  mutable view : (vertex, edge) View.view option;
  mutable table : GPack.table option;
}

let file =
  if Array.length Sys.argv < 2 then None
  else Some Sys.argv.(1)

(* Creates a scrolled graphView in a table *)
let scrolled_view ~packing model =
  let frame = GBin.frame ~shadow_type:`IN () in
  let aa = true (* anti-aliasing *) in
  let view = View.view
    ~aa ~width:1280 ~height:1024 ~packing:frame#add model () in

  ignore $ view#set_center_scroll_region true;
  let table = GPack.table ~packing
                ~rows:2 ~columns:2 ~row_spacings:4 ~col_spacings:4 () in
  ignore $ table#attach ~left:0 ~right:1 ~top:0 ~bottom:1
           ~expand:`BOTH ~fill:`BOTH ~shrink:`BOTH ~xpadding:0 ~ypadding:0
           frame#coerce;
  let w = GRange.scrollbar `HORIZONTAL ~adjustment:view#hadjustment () in
  ignore $ table#attach ~left:0 ~right:1 ~top:1 ~bottom:2
            ~expand:`X ~fill:`BOTH ~shrink:`X ~xpadding:0 ~ypadding:0
            w#coerce;
  let w = GRange.scrollbar `VERTICAL ~adjustment:view#vadjustment () in
  ignore $ table#attach ~left:1 ~right:2 ~top:0 ~bottom:1
            ~expand:`Y ~fill:`BOTH ~shrink:`Y ~xpadding:0 ~ypadding:0 
            w#coerce;

  view, table

let state = 
  { file = None;
    view = None;
    table = None }

(* Top menu *)

let menu_desc = "<ui>\
  <menubar name='MenuBar'>\
    <menu action='FileMenu'>\
      <menuitem action='Open'/>\
      <separator/>\
      <menuitem action='Quit'/>\
    </menu>\
  </menubar>\
</ui>"

let create_menu () =
  let ui_m = GAction.ui_manager () in
  let actions = GAction.action_group ~name:"Actions" () in
    GAction.add_actions actions [
      GAction.add_action "FileMenu" ~label:"File" ;
      GAction.add_action "Open" ~label:"Open" ~accel:""
                         (* callback connected later *);
      GAction.add_action "Quit" ~label:"Quit" ~accel:""
	~callback:(fun _ -> GMain.Main.quit ());
    ];
    ui_m#insert_action_group actions 0 ;
    let _ = ui_m#add_ui_from_string menu_desc in

    ui_m

(* Open a file *)

let update_state ~packing file =
  begin match state.table with
    | None -> ()
    | Some t -> t#destroy ()
  end;
  if !debug then printf "Building Model...\n";
  let model = OCamlGraph.read_dot ~cmd:"dot" ~dot:file in
  if !debug then printf "Building View...\n";
  let view, table = scrolled_view ~packing model in
  state.file <- Some file;
  state.view <- Some view;
  state.table <- Some table

let open_file ~packing () = 
  match GToolbox.select_file ~title:"Select a dot file" () with
    | None -> ()
    | Some file -> update_state ~packing file

(* Main loop *)

let main () =
  (* GUI *)
  let window = GWindow.window ~title:"Graph Widget"
                 ~allow_shrink:true ~allow_grow:true () in
  let vbox = GPack.vbox ~border_width:4 ~spacing:4
                            ~packing:window#add () in

  (* Menu *)
  let ui_m = create_menu () in
  window#add_accel_group ui_m#get_accel_group ;
  vbox#pack ~expand:false (ui_m#get_widget "/MenuBar") ;

  let packing = vbox#pack ~expand:true ~fill:true in

  begin match file with
    | Some f -> update_state ~packing f
    | None -> ()
  end;

  let actions = List.hd ui_m#get_action_groups in
  let open_action = actions#get_action "Open" in 
  ignore $ open_action#connect#activate ~callback:(open_file ~packing);
    
  ignore $ window#connect#destroy ~callback:GMain.Main.quit;

  if !debug then printf "GUI built, time: %f\n" (Sys.time ());
  
  window#show();
  GMain.Main.main ()
    
let _ = Printexc.print main ()
