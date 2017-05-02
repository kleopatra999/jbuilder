open Import
open Jbuild_types

module Jbuilds = struct
  type script =
    { dir              : Path.t
    ; visible_packages : Package.t String_map.t
    ; closest_packages : Package.t list
    }

  type one =
    | Literal of Path.t * Stanza.t list
    | Use_meta_lang of script * Sexp.Ast.t list
    | Script of script

  type t = one list

  let generated_jbuilds_dir = Path.(relative root) "_build/.jbuilds"

  let ensure_parent_dir_exists path =
    match Path.kind path with
    | Local path -> Path.Local.ensure_parent_directory_exists path
    | External _ -> ()

  let extract_requires str =
    List.fold_left (String.split str ~on:'\n') ~init:String_set.empty ~f:(fun acc line ->
      match Scanf.sscanf line "#require %S" (fun x -> x) with
      | exception _ -> acc
      | s ->
        String_set.union acc
          (String_set.of_list (String.split s ~on:',')))
    |> String_set.elements

  let create_plugin_wrapper (context : Context.t) ~exec_dir ~plugin ~wrapper ~target =
    let plugin = Path.to_string plugin in
    let plugin_contents = read_file plugin in
    with_file_out (Path.to_string wrapper) ~f:(fun oc ->
      Printf.fprintf oc {|
let () = Hashtbl.add Toploop.directive_table "require" (Toploop.Directive_string ignore)
module Jbuild_plugin = struct
  module V1 = struct
    let context       = %S
    let ocaml_version = %S

    let ocamlc_config =
      [ %s
      ]

    let send s =
      let oc = open_out_bin %S in
      output_string oc s;
      close_out oc
  end
end
# 1 %S
%s|}
        context.name
        context.version
        (String.concat ~sep:"\n      ; "
           (let longest = List.longest_map context.ocamlc_config ~f:fst in
            List.map context.ocamlc_config ~f:(fun (k, v) ->
                Printf.sprintf "%-*S , %S" (longest + 2) k v)))
        (Path.reach ~from:exec_dir target)
        plugin plugin_contents);
    extract_requires plugin_contents

  let eval jbuilds ~(context : Context.t) =
    let open Future in
    let env = lazy (Jbuild_meta_lang.Env.make context) in
    List.map jbuilds ~f:(function
      | Literal (path, stanzas) ->
        return (path, stanzas)
      | Use_meta_lang ({ dir; visible_packages; closest_packages }, sexps) ->
        let sexps = Jbuild_meta_lang.expand (Lazy.force env) sexps in
        return (dir, Stanzas.parse sexps ~dir ~visible_packages ~closest_packages)
      | Script { dir
               ; visible_packages
               ; closest_packages
               } ->
        let file = Path.relative dir "jbuild" in
        let generated_jbuild =
          Path.append (Path.relative generated_jbuilds_dir context.name) file
        in
        let wrapper = Path.extend_basename generated_jbuild ~suffix:".ml" in
        ensure_parent_dir_exists generated_jbuild;
        let requires =
          create_plugin_wrapper context ~exec_dir:dir ~plugin:file ~wrapper
            ~target:generated_jbuild
        in
        let pkgs =
          List.map requires ~f:(Findlib.find_exn context.findlib
                                  ~required_by:[Utils.jbuild_name_in ~dir:dir])
          |> Findlib.closure ~required_by:dir ~local_public_libs:String_map.empty
        in
        let includes =
          List.fold_left pkgs ~init:Path.Set.empty ~f:(fun acc pkg ->
            Path.Set.add pkg.Findlib.dir acc)
          |> Path.Set.elements
          |> List.concat_map ~f:(fun path ->
              [ "-I"; Path.to_string path ])
        in
        let cmas =
          List.concat_map pkgs ~f:(fun pkg -> pkg.archives.byte)
        in
        let args =
          List.concat
            [ [ "-I"; "+compiler-libs" ]
            ; includes
            ; cmas
            ; [ Path.reach ~from:dir wrapper ]
            ]
        in
        (* CR-someday jdimino: if we want to allow plugins to use findlib:
           {[
             let args =
               match context.toplevel_path with
               | None -> args
               | Some path -> "-I" :: Path.reach ~from:dir path :: args
             in
           ]}
        *)
        Future.run Strict ~dir:(Path.to_string dir) ~env:context.env
          (Path.to_string context.ocaml)
          args
        >>= fun () ->
        if not (Path.exists generated_jbuild) then
          die "@{<error>Error:@} %s failed to produce a valid jbuild file.\n\
               Did you forgot to call [Jbuild_plugin.V*.send]?"
            (Path.to_string file);
        let sexps = Sexp_load.many (Path.to_string generated_jbuild) in
        return (dir, Stanzas.parse sexps ~dir ~visible_packages ~closest_packages))
    |> Future.all
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuilds.t
  ; packages  : Package.t String_map.t
  }

let load ~dir ~visible_packages ~closest_packages =
  let file = Path.relative dir "jbuild" in
  match Sexp_load.many_or_ocaml_script (Path.to_string file) with
  | Sexps sexps ->
    let is_meta_lang : Sexp.Ast.t -> bool = function
      | List (_, [Atom (_, "use_meta_lang")]) -> true
      | _ -> false
    in
    if List.exists sexps ~f:is_meta_lang then
      Jbuilds.Use_meta_lang
        ({ dir
         ; visible_packages
         ; closest_packages
         },
         sexps)
    else
      Jbuilds.Literal (dir, Stanzas.parse sexps ~dir ~visible_packages ~closest_packages)
  | Ocaml_script ->
    Script
      { dir
      ; visible_packages
      ; closest_packages
      }

let load ?(extra_ignored_subtrees=Path.Set.empty) () =
  let ftree = File_tree.load Path.root in
  let packages, ignored_subtrees =
    File_tree.fold ftree ~init:([], extra_ignored_subtrees) ~f:(fun dir (pkgs, ignored) ->
      let path = File_tree.Dir.path dir in
      let files = File_tree.Dir.files dir in
      let pkgs =
        String_set.fold files ~init:pkgs ~f:(fun fn acc ->
          match Filename.split_extension fn with
          | (pkg, ".opam") when pkg <> "" ->
            let version_from_opam_file =
              let lines = lines_of_file (Path.relative path fn |> Path.to_string) in
              List.find_map lines ~f:(fun s ->
                try
                Scanf.sscanf s "version: %S" (fun x -> Some x)
              with _ ->
                None)
          in
          (pkg,
           { Package. name = pkg
           ; path
           ; version_from_opam_file
           }) :: acc
          | _ -> acc)
      in
      if String_set.mem "jbuild-ignore" files then
        let ignore_set =
          String_set.of_list
            (lines_of_file (Path.to_string (Path.relative path "jbuild-ignore")))
        in
        Dont_recurse_in
          (ignore_set,
           (pkgs,
            String_set.fold ignore_set ~init:ignored ~f:(fun fn acc ->
              Path.Set.add (Path.relative path fn) acc)))
      else
        Cont (pkgs, ignored))
  in
  let packages =
    String_map.of_alist_multi packages
    |> String_map.mapi ~f:(fun name pkgs ->
      match pkgs with
      | [pkg] -> pkg
      | _ ->
        die "Too many opam files for package %S:\n%s"
          name
          (String.concat ~sep:"\n"
             (List.map pkgs ~f:(fun pkg ->
                sprintf "- %s.opam" (Path.to_string pkg.Package.path)))))
  in
  let packages_per_dir =
    String_map.values packages
    |> List.map ~f:(fun pkg -> (pkg.Package.path, pkg))
    |> Path.Map.of_alist_multi
  in
  let rec walk dir jbuilds visible_packages closest_packages =
    let path = File_tree.Dir.path dir in
    let files = File_tree.Dir.files dir in
    let sub_dirs = File_tree.Dir.sub_dirs dir in
    let visible_packages, closest_packages =
      match Path.Map.find path packages_per_dir with
      | None -> (visible_packages, closest_packages)
      | Some pkgs ->
        (List.fold_left pkgs ~init:visible_packages ~f:(fun acc pkg ->
           String_map.add acc ~key:pkg.Package.name ~data:pkg),
         pkgs)
    in
    let jbuilds =
      if String_set.mem "jbuild" files then
        let jbuild = load ~dir:path ~visible_packages ~closest_packages in
        jbuild :: jbuilds
      else
        jbuilds
    in
    let children, jbuilds =
      String_map.fold sub_dirs ~init:([], jbuilds)
        ~f:(fun ~key:_ ~data:dir (children, jbuilds) ->
          if Path.Set.mem (File_tree.Dir.path dir) ignored_subtrees then
            (children, jbuilds)
          else
            let child, jbuilds = walk dir jbuilds visible_packages closest_packages in
            (child :: children, jbuilds))
    in
    (Alias.Node (path, children), jbuilds)
  in
  let root = File_tree.root ftree in
  let tree, jbuilds = walk root [] String_map.empty [] in
  { file_tree = ftree
  ; tree
  ; jbuilds
  ; packages
  }
