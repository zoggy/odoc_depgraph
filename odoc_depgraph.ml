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
              match split_string v [','] with
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
                        match split_string v [','] with
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
    inherit Odoc_dot.dot as dot

     method! print_module_atts fmt m =
      Format.fprintf fmt
        "\"%s\" [style=\"rounded,filled\", shape=rect, color=red, fillcolor=grey, fontcolor=black];\n" m.Module.m_name

  end
;;

class gen () =
  object (self)
    inherit Odoc_html.html as html

    method gen_dot_file modules =
      Odoc_info.Dep.kernel_deps_of_modules modules;
      let out_file_bak = !Odoc_info.Args.out_file in
      let dot_file = Filename.temp_file "odoc_gi" ".dot" in
      let dot_file2 = Filename.temp_file "odoc_gi" ".dot" in
      Odoc_info.Args.out_file := dot_file;
      let dot_gen = new dot in
      dot_gen#generate modules;
      Odoc_info.Args.out_file := out_file_bak;
      let com = Printf.sprintf "dot -s72 %s > %s"
        (Filename.quote dot_file)
        (Filename.quote dot_file2)
      in
      match Sys.command com with
        n when n <> 0 ->
          failwith (Printf.sprintf "Command failed: %s" com)
      | _ ->
          Sys.remove dot_file;
          dot_file2

    method gen_png_file dot_file =
      let png_file = Filename.concat
        !Odoc_info.Args.target_dir "index.png"
      in
      let com = Printf.sprintf "dot -Grotate=0 -Tpng -o %s %s"
        (Filename.quote png_file) (Filename.quote dot_file)
      in
      match Sys.command com with
        n when n <> 0 ->
          failwith (Printf.sprintf "Command failed: %s" com)
      | _ -> png_file

    method get_dot_info dot_file =
      analyse_annot_dot_file dot_file

    method gen_map b map_name (_,y,items) =
      bp b "<map id=\"%s\" name=\"%s\">\n" map_name map_name;
      List.iter
        (fun (x1,y1,x2,y2,id) ->
           let (html_file, _) = Naming.html_files id in
           bp b "<area shape=\"rect\" id=\"%s\" href=\"%s\" title=\"%s\" alt=\"\" coords=\"%d,%d,%d,%d\"/>\n"
             id (Filename.basename html_file) id
             (int_of_float x1) (int_of_float (y-.y1)) (int_of_float x2) (int_of_float (y-.y2))
        )
        items;
      bs b "</map>\n"

    method gen_index_file info png_file =
      let b = Buffer.create 1024 in
      bs b doctype ;
      bs b "<html>\n";
      self#print_header b self#title;
      bs b "<body>\n";
      bs b "<center><h1>";
      bs b self#title;
      bs b "</h1></center>\n<br>\n";
      let map_name = Printf.sprintf "_map%s_" (Filename.basename png_file) in
      let (w, h, _) = info in
      bp b "<center><img class=\"depgraph\" src=\"%s\" usemap=\"#%s\" width=\"%d\" height=\"%d\"/><br/></center>"
        (Filename.basename png_file) map_name (int_of_float w) (int_of_float h);
      self#gen_map b map_name info;
      bs b "</body></html>";
      let chanout = open_out (Filename.concat !Args.target_dir "index.html") in
      Buffer.output_buffer chanout b;
      close_out chanout

    method generate modules =
      html#generate modules;
      let dot_file = self#gen_dot_file modules in
      let png_file = self#gen_png_file dot_file in
      let info = self#get_dot_info dot_file in
      self#gen_index_file info png_file

    initializer
      default_style_options <-
        "img.depgraph { border-width: 0px; }" ::
        default_style_options
  end

let generator = ((new gen ()) :> Odoc_args.doc_generator)

let _ = Odoc_args.set_doc_generator (Some generator)
