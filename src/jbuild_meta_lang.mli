(** Jbuild meta language *)

module Env : sig
  type t

  val make : Context.t -> t
end

val expand : Env.t -> Sexp.Ast.t list -> Sexp.Ast.t list
