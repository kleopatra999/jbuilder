open Import
open Sexp.Ast

module Env = struct
  type t = Sexp.t String_map.t

  let make (ctx : Context.t) : t =
    String_map.of_alist_exn
      (List.map ctx.ocamlc_config
         ~f:(fun (name, value) ->
           ("ocaml." ^ name, Sexp.Atom value)))
end

module Atom_patt = String_with_vars.Make(struct let escape = '%' end)

let rec pattern_match (env : Env.t) (patt : Sexp.Ast.t) (value : Sexp.t) =
  match patt with
  | Atom (loc, p) -> begin
      match Atom_patt.just_a_var (Atom_patt.of_string p) with
      | Some var ->
        Ok (String_map.add env ~key:var ~data:value)
      | None ->
        if value = Atom p then
          Ok env
        else
          Error (loc, value)
    end
  | List (loc, p) -> begin
      match value with
      | Atom _ -> Error (loc, value)
      | List l ->
        if List.length l <> List.length p then
          Error (loc, value)
        else
          List.fold_left2 p l ~init:(Ok env) ~f:(fun acc p v ->
            match acc with
            | Ok env -> pattern_match env p v
            | Error _ -> acc)
    end

let pattern_match_exn env patt value =
  match pattern_match env patt value with
  | Ok env -> env
  | Error (loc, value) ->
    Loc.fail loc "pattern matching failure: got %s" (Sexp.to_string value)

let rec expand env sexps : Sexp.Ast.t list =
  match sexps with
  | List (loc, (Atom (_, ":let") :: args)) :: rest -> begin
      match args with
      | [] | [_] ->
        Loc.fail loc "(:let ...) forms expect at least 2 arguments"
      | patt :: value :: body ->
        let value = eval env value in
        let new_env = pattern_match_exn env patt value in
        match body with
        | [] ->
          expand new_env rest
        | _ ->
          expand new_env body @ expand env rest
    end
  | List (loc, (Atom (_, ":foreach") :: args)) :: rest -> begin
      match args with
      | [] | [_] ->
        Loc.fail loc "(:foreach ...) forms expect at least 2 arguments"
      | patt :: value :: body ->
        let values =
          match eval env value with
          | Atom _ as s ->
            Loc.fail (Sexp.Ast.loc value) "list expected, got: %s" (Sexp.to_string s)
          | List l -> l
        in
        List.concat_map values ~f:(fun value ->
          let env = pattern_match_exn env patt value in
          expand env body)
        @ expand env rest
    end
  | Atom (loc, s) as x :: rest ->
    let x =
      let p = Atom_patt.of_string s in
      match Atom_patt.just_a_var p with
      | Some v -> begin
        match String_map.find v env with
          | None -> x
          | Some sexp -> Sexp.add_loc sexp ~loc
      end
      | None ->
        Atom (loc, Atom_patt.expand p ~f:(fun var ->
          match String_map.find var env with
          | None -> None
          | Some (Atom s) -> Some s
          | Some (List _ as sexp) ->
            Loc.fail loc "%S must be a list, got: %s"
              var (Sexp.to_string sexp)))
    in
    x :: expand env rest
  | List (loc, l) :: rest ->
    List (loc, expand env l) :: expand env rest
  | [] -> []

and eval env sexp =
    match expand env [sexp] |> List.map ~f:Sexp.Ast.remove_locs with
    | [x] -> x
    | [] ->
      Loc.fail (Sexp.Ast.loc sexp) "evaluated to no nothing"
    | _ ->
      Loc.fail (Sexp.Ast.loc sexp) "too many values"
