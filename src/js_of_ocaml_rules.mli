(** Generate rules for js_of_ocaml *)

open Jbuild_types

val build_cm
  :  Super_context.t
  -> dir:Path.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.t
  -> (unit, Action.t) Build.t list

val build_exe
  :  Super_context.t
  -> dir:Path.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.t
  -> (Lib.t list * Path.t list, Action.t) Build.t list

val setup_separate_compilation_rules
  :  Super_context.t
  -> (unit, Action.t) Build.t list


