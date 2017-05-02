open! Import

module type S = sig
  type t
  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t -> Sexp.t

  val of_string : string -> t
  val raw : string -> t

  val just_a_var : t -> string option

  val vars : t -> String_set.t

  val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

  val expand : t -> f:(string -> string option) -> string
end

module type Syntax = sig
  val escape : char
end

module Make(Syntax : Syntax) = struct
  type var_syntax = Parens | Braces

  type item =
    | Text of string
    | Var of var_syntax * string

  type t = item list

  module Token = struct
    type t =
      | String of string
      | Open   of var_syntax
      | Close  of var_syntax

    let tokenise s =
      let len = String.length s in
      let sub i j = String.sub s ~pos:i ~len:(j - i) in
      let cons_str i j acc = if i = j then acc else String (sub i j) :: acc in
      let rec loop i j =
        if j = len
        then cons_str i j []
        else
          match s.[j] with
          | '}' -> cons_str i j (Close Braces :: loop (j + 1) (j + 1))
          | ')' -> cons_str i j (Close Parens :: loop (j + 1) (j + 1))
          | c when c = Syntax.escape -> begin
              match s.[j + 1] with
              | '{' -> cons_str i j (Open Braces :: loop (j + 2) (j + 2))
              | '(' -> cons_str i j (Open Parens :: loop (j + 2) (j + 2))
              | _   -> loop i (j + 1)
            end
          | _ -> loop i (j + 1)
      in
      loop 0 0

    let open_braces = sprintf "%c{" Syntax.escape
    let open_parens = sprintf "%c(" Syntax.escape

    let to_string = function
      | String s     -> s
      | Open  Braces -> open_braces
      | Open  Parens -> open_parens
      | Close Braces -> "}"
      | Close Parens -> ")"
  end

  let rec of_tokens : Token.t list -> t = function
    | [] -> []
    | Open a :: String s :: Close b :: rest when a = b ->
      Var (a, s) :: of_tokens rest
    | token :: rest ->
      let s = Token.to_string token in
      match of_tokens rest with
      | Text s' :: l -> Text (s ^ s') :: l
      | l -> Text s :: l

  let of_string s = of_tokens (Token.tokenise s)

  let t sexp = of_string (Sexp.Of_sexp.string sexp)

  let raw s = [Text s]

  let just_a_var = function
    | [Var (_, s)] -> Some s
    | _ -> None

  let sexp_of_var_syntax = function
    | Parens -> Sexp.Atom "parens"
    | Braces -> Sexp.Atom "braces"

  let sexp_of_item =
    let open Sexp in function
      | Text s -> List [Atom "text" ; Atom s]
      | Var (vs, s) -> List [sexp_of_var_syntax vs ; Atom s]

  let sexp_of_t = Sexp.To_sexp.list sexp_of_item


  let fold t ~init ~f =
    List.fold_left t ~init ~f:(fun acc item ->
      match item with
      | Text _ -> acc
      | Var (_, v) -> f acc v)

  let vars t = fold t ~init:String_set.empty ~f:(fun acc x -> String_set.add x acc)

  let expand t ~f =
    List.map t ~f:(function
      | Text s -> s
      | Var (syntax, v) ->
        match f v with
        | Some x -> x
        | None ->
          match syntax with
          | Parens -> sprintf "%c(%s)" Syntax.escape v
          | Braces -> sprintf "%c{%s}" Syntax.escape v)
    |> String.concat ~sep:""
end

include Make(struct let escape = '$' end)
