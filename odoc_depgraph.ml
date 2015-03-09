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

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_subbytes buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

let dot_to_svg ?(options="") ?size dot =
  let temp_file = Filename.temp_file "odoc_gi" "svg" in
  let com = Printf.sprintf "echo %s |dot %s -Tsvg | tail --lines=+%d > %s"
    (Filename.quote dot) options
    (match size with None -> 7 | Some _ -> 9)
    (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let svg = string_of_file temp_file in
      Sys.remove temp_file;
      let svg =
        match size with
          None -> svg
        | Some (w,h) ->
          Printf.sprintf "<svg width=\"%d\" height=\"%d\" viewBox=\"0.0 0.0 %d.00 %d.00\"\n
          xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n%s"
            w h w h svg
      in
      svg
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;

class dot =
  object(self)
    inherit Odoc_dot.Generator.dot as dot

     method! print_module_atts fmt m =
      let (html_file, _) = Naming.html_files m.m_name in
      Format.fprintf fmt
        "\"%s\" [href=%S, tooltip=%S, style=\"rounded,filled\", shape=rect, color=red, fillcolor=lightgrey, fontcolor=black];\n"
        m.Module.m_name
       html_file
        (match m.Module.m_info with
           None -> m.Module.m_name
         | Some i ->
             match i.Odoc_info.i_desc with
             | None -> m.Module.m_name
             | Some t ->
                 let s = Odoc_info.first_sentence_of_text t in
                 Odoc_text.Texter.string_of_text s
        )
  end
;;

let width = ref 600;;
let height = ref 300;;
let dot_options = ref ""

let () = Odoc_args.add_option
  ("-width", Arg.Int (fun n -> width := n),
  "<n> set width for module graph on index page")
;;
let () = Odoc_args.add_option
  ("-height", Arg.Int (fun n -> height := n),
  "<n> set height for module graph on index page")
;;

let () = Odoc_args.add_option
  ("-dot-options", Arg.Set_string dot_options,
  "<s> set additional dot command-line options (see graphviz documentation)")
;;

module Generator (G : Odoc_html.Html_generator) =
struct
 class html =
    object(self)
      inherit G.html as html

    val mutable top_modules = []
    method private gen_dot ?width ?height modules =
      if !Odoc_dot.dot_reduce then
        Odoc_info.Dep.kernel_deps_of_modules modules;
      let out_file_bak = !Odoc_global.out_file in
      let dot_file = Filename.temp_file "odoc_gi" ".dot" in
      Odoc_global.out_file := dot_file;
      let dot_gen = new dot in
      dot_gen#generate modules;
      Odoc_global.out_file := out_file_bak;
(*      let graph_size_flags =
        match width, height with
          None, _ | _, None -> ""
        | Some w, Some h ->
            Printf.sprintf "-Gsize=%f,%f"
             (w /. default_dot_ppi)
              (h /. default_dot_ppi)
      in
             *)
       let dot = string_of_file dot_file in
       Sys.remove dot_file;
       dot

    method private gen_image b ?(width= !width) ?(height= !height) modules =
      let dot = self#gen_dot ~width ~height modules in
      let size = (width, height) in
      let options =
          Printf.sprintf "-Gsize=%f,%f -Grotate=0 %s"
            (float width /. default_dot_ppi)
            (float height /. default_dot_ppi)
            !dot_options
      in
      let svg = dot_to_svg ~options ~size dot in
      (*Sys.remove dot_file;*)
      bp b "<div class=\"depgraph\">%s</div>" svg

    method private html_of_modgraph b t =
      let get_modules s =
        let names = List.map no_blanks (split_string s ['\n'; '\t' ; ' '; ',']) in
        match names with
          [] -> top_modules
        | _ ->
          List.filter (fun m -> List.mem m.m_name names) list_modules
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
      let get_int_att name atts =
        try
          Some (int_of_string (List.assoc name atts))
        with Not_found -> None
      in
      match t with
        [] -> ()
      | [Raw s] -> self#gen_image b (get_modules s)
      | [Code code ; Raw s] ->
          let atts = get_atts code in
          let width = get_int_att "width" atts
          and height = get_int_att "height" atts in
          self#gen_image b ?width ?height (get_modules s)
      | _ -> failwith "Bad syntax in {modgraph"

    method html_of_custom_text b s t =
      match s with
        "modgraph" -> self#html_of_modgraph b t
      | _ -> html#html_of_custom_text b s t

    method generate_index modules =
      top_modules <- modules ;
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
           self#gen_image ~width: !height ~height: !height b modules;
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
