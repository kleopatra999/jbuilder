(** Odoc rules *)

open Import
open Jbuild_types

val setup_library_rules
  :  Super_context.t
  -> Library.t
  -> dir:Path.t
  -> modules:Module.t String_map.t
  -> requires:(unit, Lib.t list) Build.t
  -> dep_graph:Ocamldep.dep_graph
  -> unit

val setup_css_rule : Super_context.t -> unit
