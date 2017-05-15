open Import

module Syntax = struct
  type t = OCaml | Reason
end

module File = struct
  type t =
    { name : string
    ; syntax : Syntax.t
    }

  let to_ocaml t =
    match t.syntax with
    | OCaml -> code_errorf "to_ocaml: can only convert reason Files" ()
    | Reason ->
      { syntax = OCaml
      ; name =
          t.name ^
          (match Filename.extension t.name with
           | ".re" -> ".ml"
           | ".rei" -> ".mli"
           | e -> code_errorf "to_ocaml: unrecognized extension %s" e ())
      }
end

type t =
  { name     : string
  ; impl     : File.t
  ; intf     : File.t option
  ; obj_name : string
  }

let real_unit_name t = String.capitalize_ascii (Filename.basename t.obj_name)

let file t ~dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Some (Path.relative dir t.impl.name)
  | Intf -> Option.map t.intf ~f:(fun f -> Path.relative dir f.name)

let cm_source t ~dir kind = file t ~dir (Cm_kind.source kind)

let cm_file t ~dir kind = Path.relative dir (t.obj_name ^ Cm_kind.ext kind)

let cmt_file t ~dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Some (Path.relative dir (t.obj_name ^ ".cmt"))
  | Intf -> Option.map t.intf ~f:(fun _ -> Path.relative dir (t.obj_name ^ ".cmti"))

let odoc_file t ~dir = Path.relative dir (t.obj_name ^ ".odoc")

let cmti_file t ~dir =
  match t.intf with
  | None   -> Path.relative dir (t.obj_name ^ ".cmt")
  | Some _ -> Path.relative dir (t.obj_name ^ ".cmti")
