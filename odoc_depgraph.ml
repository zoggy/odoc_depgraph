(*********************************************************************************)
(*                Odoc_depgraph                                                  *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License version       *)
(*    2.1 as published by the Free Software Foundation.                          *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** @ocamldoc_generator
  OCamldoc generator enhancing HTML generator, to build
  an index page with the dependency graph of the top modules.
   @ocamldoc_compilation [ocamlc -I +ocamldoc -c odoc_gi.ml]
  @author Maxence Guesdon
*)

open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module

let p = Printf.bprintf
let bp = Printf.bprintf
let bs = Buffer.add_string

let default_dot_ppi = 72.0
let p_dbg s = ();;

(*c==v=[String.no_blanks]=1.0====*)
let no_blanks s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      ' ' | '\n' | '\t' | '\r' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf
(*/c==v=[String.no_blanks]=1.0====*)

(*c==v=[String.split_string]=1.0====*)
let split_string s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" -> iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.0====*)

let get_graph_bounding_box stmt_list =
  let rec iter = function
    [] ->
      p_dbg "bing!";
      raise Not_found
  | (Odot.Stmt_attr (Odot.Attr_graph attr_list)) :: q ->
      begin
        match Odot.attr_value (Odot.Simple_id "bb") attr_list with
          Some (Odot.Simple_id v)
        | Some (Odot.Double_quoted_id v) ->
            begin
              match split_string v ['\n'; ','] with
                [x1;y1;x2;y2] ->
                  (
                   let (a,b,c,d) =
                     try (float_of_string x1, float_of_string y1,
                        float_of_string x2, float_of_string y2)
                     with
                     | _ -> raise Not_found
                   in
                   match a,b,c,d with
                     0., _, _, 0. -> (0.,0.,c,b)
                   | _ as x -> x
                  )
              | _ ->
                  prerr_endline "bouh!";
                  raise Not_found
            end
        | _ -> iter q
      end
  | _ :: q -> iter q
  in
  iter stmt_list
;;

let analyse_annot_dot_file f =
  try
    let graph = Odot.parse_file f in
    let (_,_,width,height) = get_graph_bounding_box graph.Odot.stmt_list in
    p_dbg (Printf.sprintf "width=%f,height=%f" width height);
    let rec iter acc = function
      [] -> acc
    |	stmt :: q ->
        match stmt with
          Odot.Stmt_node (node_id,attr_list) ->
            p_dbg "Stmt_node";
            begin
              try
                let w =
                  match Odot.attr_value (Odot.Simple_id "width") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      (try float_of_string v
                       with _ -> raise Not_found)
                  | _ -> raise Not_found
                in
                let h =
                  match Odot.attr_value (Odot.Simple_id "height") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      (try float_of_string v
                       with _ -> raise Not_found)
                  | _ -> raise Not_found
                in
                let (x,y) =
                  match Odot.attr_value (Odot.Simple_id "pos") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      begin
                        match split_string v ['\n'; ','] with
                          [x;y] ->
                            (
                             try (float_of_string x, float_of_string y)
                             with | _ -> raise Not_found
                            )
                        | _ -> raise Not_found
                      end
                  | _ -> raise Not_found
                in
                let w = w *. default_dot_ppi in
                let h = h *. default_dot_ppi in
                let x1 = x -. w /. 2.0 in
                let y1 = y -. h /. 2.0 in
                let x2 = x +. w /. 2.0 in
                let y2 = y +. h /. 2.0 in
                let s_id = Odot.string_of_node_id node_id in
                p_dbg (Printf.sprintf "id %s: x1=%f y1=%f x2=%f y2=%f"
                 s_id x1 y1 x2 y2);
                iter ((x1,y1,x2,y2,s_id)::acc) q
              with
                Not_found ->
                prerr_endline (Printf.sprintf "Not_found: %s" (Odot.string_of_node_id node_id));
                  iter acc q
            end
        | Odot.Stmt_subgraph g ->
            iter acc (g.Odot.sub_stmt_list @ q)
        | Odot.Stmt_equals _
        | Odot.Stmt_edge _
        | Odot.Stmt_attr _ -> iter acc q
    in
    (width, height, iter [] graph.Odot.stmt_list)
  with
    e ->
      p_dbg (Printexc.to_string e);
      (1., 1., [])

class dot =
  object(self)
    inherit Odoc_dot.Generator.dot as dot

     method! print_module_atts fmt m =
      Format.fprintf fmt
        "\"%s\" [style=\"rounded,filled\", shape=rect, color=red, fillcolor=grey, fontcolor=black];\n" m.Module.m_name

  end
;;

let width = ref None;;
let height = ref None;;
let zoom = ref None;;

let () = Odoc_args.add_option
  ("-width", Arg.Float (fun f -> width := Some f),
  "<float> set width for module graph on index page")
;;
let () = Odoc_args.add_option
  ("-height", Arg.Float (fun f -> height := Some f),
  "<float> set height for module graph on index page")
;;
let () = Odoc_args.add_option
  ("-zoom", Arg.Float (fun f -> zoom := Some f),
  "<float> set zoom for module graph on index page")
;;

module Generator (G : Odoc_html.Html_generator) =
struct
 class html =
    object(self)
      inherit G.html as html

    method private gen_dot_file ?width ?height modules =
      Odoc_info.Dep.kernel_deps_of_modules modules;
      let out_file_bak = !Odoc_global.out_file in
      let dot_file = Filename.temp_file "odoc_gi" ".dot" in
      let dot_file2 = Filename.temp_file "odoc_gi" ".dot" in
      Odoc_global.out_file := dot_file;
      let dot_gen = new dot in
      dot_gen#generate modules;
      Odoc_global.out_file := out_file_bak;
      let graph_size_flags =
        match width, height with
          None, _ | _, None -> ""
        | Some w, Some h ->
            Printf.sprintf "-Gsize=%f,%f"
             (w /. default_dot_ppi)
              (h /. default_dot_ppi)
      in
      let com = Printf.sprintf "dot -s72 %s %s > %s"
        (Filename.quote dot_file)
        graph_size_flags
        (Filename.quote dot_file2)
      in
      match Sys.command com with
        n when n <> 0 ->
          failwith (Printf.sprintf "Command failed: %s" com)
      | _ ->
          Sys.remove dot_file;
          dot_file2

    val mutable png_file_counter = 0
    method private gen_png_file dot_file =
      let png_file = Filename.concat
        !Odoc_global.target_dir
        (Printf.sprintf "_map%d.png" png_file_counter)
      in
      png_file_counter <- png_file_counter + 1;
      let com = Printf.sprintf "dot -Grotate=0 -Tpng -o %s %s"
        (Filename.quote png_file) (Filename.quote dot_file)
      in
      match Sys.command com with
        n when n <> 0 ->
          failwith (Printf.sprintf "Command failed: %s" com)
      | _ -> png_file

    method private get_dot_info dot_file =
      analyse_annot_dot_file dot_file

    method private gen_map b ~x_factor ~y_factor map_name (_,y,items) =
      bp b "<map id=\"%s\" name=\"%s\">\n" map_name map_name;
      let y = y *. y_factor in
      List.iter
        (fun (x1,y1,x2,y2,id) ->
           let (x1,y1,x2,y2) =
             (x1 *. x_factor, y1 *. y_factor,
              x2 *. x_factor, y2 *. y_factor)
           in
           let (html_file, _) = Naming.html_files id in
           bp b "<area shape=\"rect\" id=\"%s\" href=\"%s\" title=\"%s\" alt=\"\" coords=\"%d,%d,%d,%d\"/>\n"
             id (Filename.basename html_file) id
             (int_of_float x1) (int_of_float (y-.y1)) (int_of_float x2) (int_of_float (y-.y2))
        )
        items;
      bs b "</map>\n"

    method private gen_image_and_map b ?width ?height ?zoom modules =
      let dot_file = self#gen_dot_file ?width ?height modules in
      let png_file = self#gen_png_file dot_file in
      let info = self#get_dot_info dot_file in
      Sys.remove dot_file;
      let map_name = Printf.sprintf "%s_"
        (Filename.chop_extension (Filename.basename png_file))
      in
      let (w, h, _) = info in
      let x_factor =
        match zoom, width with
          None, None -> 1.
        | None, Some w2 -> w2 /. w
        | Some z, _ -> z
      in
      let y_factor =
        match zoom, height with
          None, None -> 1.
        | None, Some h2 -> h2 /. h
        | Some z, _ -> z
      in
      bp b "<img class=\"depgraph\" src=\"%s\" usemap=\"#%s\" width=\"%d\" height=\"%d\"/><br/>\n"
        (Filename.basename png_file) map_name
        (int_of_float (w*.x_factor)) (int_of_float (h*.y_factor));
      self#gen_map b ~x_factor ~y_factor map_name info

    method private html_of_modgraph b t =
      let get_modules s =
        let names = List.map no_blanks (split_string s ['\n'; '\t' ; ' '; ',']) in
        List.filter
          (fun m -> List.mem m.m_name names)
          list_modules
      in
      let get_atts s =
        List.fold_left
          (fun acc s ->
             match split_string s [' '; '='] with
               s1 :: s2 :: _ -> (s1, s2) :: acc
             | _ -> acc
          )
          []
          (split_string s ['\n' ; ','])
      in
      let get_float_att name atts =
        try
          Some (float_of_string (List.assoc name atts))
        with Not_found -> None
      in
      match t with
        [] -> ()
      | [Raw s] -> self#gen_image_and_map b (get_modules s)
      | [Code code ; Raw s] ->
          let atts = get_atts code in
          let width = get_float_att "width" atts
          and height = get_float_att "height" atts
          and zoom = get_float_att "zoom" atts in
          self#gen_image_and_map b
           ?width ?height ?zoom (get_modules s)
      | _ -> failwith "Bad syntax in {modgraph"

    method html_of_custom_text b s t =
      match s with
        "modgraph" -> self#html_of_modgraph b t
      | _ -> html#html_of_custom_text b s t

    method generate_index modules =
      let b = Buffer.create 1024 in
      let title = match !Odoc_global.title with None -> "" | Some t -> self#escape t in
      bs b doctype ;
      bs b "<html>\n";
      self#print_header b self#title;
      bs b "<body>\n";
      bs b "<center><h1>";
      bs b title;
      bs b "</h1></center>\n";
      let info = Odoc_info.apply_opt
            (Odoc_info.info_of_comment_file modules)
            !Odoc_global.intro_file
      in
      (
       match info with
         None ->
           self#html_of_Index_list b;
           bs b "<br/><center>";
           self#gen_image_and_map
             ?width: !width ?height: !height ?zoom: !zoom
             b modules;
           bs b "</center>"
       | Some i -> self#html_of_info ~indent: false b info
      );
      bs b "</body></html>";
      let chanout = open_out (Filename.concat !Odoc_global.target_dir self#index) in
      Buffer.output_buffer chanout b;
      close_out chanout

    initializer
      default_style_options <-
        "img.depgraph { border-width: 0px; }" ::
        default_style_options
  end
end

let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor);;
