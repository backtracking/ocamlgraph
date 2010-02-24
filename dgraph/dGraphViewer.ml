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
open Dgraph
open Printf

let ($) f x = f x

let debug = false

type state = {
  mutable file: string option;
  mutable random: bool;
  mutable window: GWindow.window;
  mutable view: DGraphViewItem.common_view option;
  mutable scroll: GBin.scrolled_window option
}

let scrolled_view ~packing model =
  let scroll =
    GBin.scrolled_window ~packing ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let view = DGraphView.view ~aa:true ~packing:scroll#add model in
  ignore (view#set_center_scroll_region true);
  view#connect_highlighting_event ();
  (view :> DGraphViewItem.common_view), scroll

let init_state () = 
   let window = 
     GWindow.window
       ~width:1280 ~height:1024
       ~title:"Graph Widget"
       ~allow_shrink:true ~allow_grow:true () 
   in
   let status = GMisc.label ~markup:"" () in
   status#set_use_markup true;
   let random = ref false in
   let file = ref None in
   for i=1 to Array.length Sys.argv - 1 do
     if Sys.argv.(i) = "--random" then
       random := true
     else
       file := Some Sys.argv.(i)
   done;
   { file = !file;
     random = !random;
     window = window;
     view = None;
     scroll = None }

(* Top menu *)

let menu_desc = "<ui>\
  <menubar name='MenuBar'>\
    <menu action='FileMenu'>\
      <menuitem action='Open'/>\
      <menuitem action='Zoom fit'/>\
      <menuitem action='Quit'/>\
    </menu>\
  </menubar>
</ui>"

let update_state state ~packing =
  (match state.scroll with None -> () | Some t -> t#destroy ());
  try
    let view, scroll = match state.file with	
      | Some file ->
	  if debug then printf "Building Model...\n";
	  let model =
	    if Filename.check_suffix file "xdot" then
	      DGraphModel.read_xdot file
	    else
	      DGraphModel.read_dot ~cmd:"dot" file
	  in
	  state.file <- Some file;
	  scrolled_view ~packing model
      | None when state.random ->
	  state.file <- None;
	  scrolled_view ~packing (DGraphRandModel.create ())
      | None ->
	  raise Not_found
    in
    if debug then printf "Building View...\n";
    state.view <- Some view;
    state.scroll <- Some scroll;
    state.window#show ();
    view#misc#show ()
  with Not_found ->
    if debug then printf "No model\n"

let all_files () =
  let f = GFile.filter ~name:"All" () in
  f#add_pattern "*" ;
  f

let open_file state ~packing () = 
  let dialog = 
    GWindow.file_chooser_dialog 
      ~action:`OPEN
      ~title:"Open File"
      ~parent:state.window () 
  in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (all_files ()) ;
  match dialog#run () with
  | `OPEN ->
      state.file <- dialog#filename;
      dialog#destroy ();
      update_state state ~packing
  | `DELETE_EVENT | `CANCEL -> dialog#destroy ()

let create_menu state ~packing =
  let ui_m = GAction.ui_manager () in
  let actions = GAction.action_group ~name:"Actions" () in
  GAction.add_actions actions [
    GAction.add_action "FileMenu" ~label:"File" ;
    GAction.add_action "Open" ~label:"Open" ~accel:"<Control>o" ~stock:`OPEN
      ~callback:(fun _ -> open_file state ~packing ());
    GAction.add_action
      "Zoom fit" ~label:"Zoom fit" ~accel:"<Control>t" ~stock:`ZOOM_FIT
      ~callback:
      (fun _ -> match state.view with Some v -> v#adapt_zoom() | None -> ());
    GAction.add_action "Quit" ~label:"Quit" ~accel:"<Control>q" ~stock:`QUIT
      ~callback:(fun _ -> GMain.Main.quit ());
  ];
  ui_m#insert_action_group actions 0 ;
  ignore $ ui_m#add_ui_from_string menu_desc;
  ui_m

(* Main loop *)

let main () =
  (* GUI *)
  let state = init_state () in
  let vbox = 
    GPack.vbox ~border_width:4 ~spacing:4 ~packing:state.window#add () 
  in
  let packing = vbox#pack ~expand:true ~fill:true in
  (* Menu *)
  let ui_m = create_menu state ~packing in
  state.window#add_accel_group ui_m#get_accel_group ;
  vbox#pack ~expand:false (ui_m#get_widget "/MenuBar");
  ignore $ state.window#connect#destroy ~callback:GMain.Main.quit;
  if debug then printf "GUI built, time: %f\n" (Sys.time ());
  update_state state ~packing;
  state.window#show ();
  GMain.Main.main ()
    
(* [JS 2009/09/21] Printexc.print prevents to use ocaml < 3.11 *)
let _ = (*Printexc.print*) main ()
