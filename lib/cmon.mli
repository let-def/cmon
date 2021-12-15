(**
   "Caml Object Notation", a library for pretty-printing ocaml values
    with sharing.
*)

type id = private int
(** Unique identifiers to make sharing explicit.
    Variable names are automatically generated.
*)

(** The output is the syntax of OCaml values (with unique identifiers
    for structured values) extended with variables and let-bindings
    to represent sharing.
*)
type syntax =
  | Unit               (* () *)
  | Nil                (* [] *)
  | Bool of bool       (* true, false *)
  | Char of char       (* 'x' *)
  | Int of int         (* 0, 1, ... *)
  | Float of float     (* 0.0, 1.0, ... *)
  | Constant of string (* constant constructor, e.g None *)
  | Cons of {id: id; car: syntax; cdr: syntax} (* x :: xs *)
  | String of {id: id; data: string} (* "Foo" *)
  | Tuple of {id: id; data: syntax list} (* (a, b, c) ... *)
  | Record of {id: id; data: (string * syntax) list} (* {a: va; b: vb} *)
  | Constructor of {id: id; tag: string; data: syntax} (* Some foo *)
  | Var of id          (* x *)
  | Let of {id: id; bindings: (id * syntax) list; body: syntax}

type t = private syntax
(** A sub-type of [syntax] that is guaranteed to represent well-scoped
    values.  ([Var] nodes always refer to variables bound by an
    enclosing [Let]). *)

(** Primitive values *)

val unit: t
(** print `()` *)

val bool: bool -> t
(** print `true` or `false` *)

val char: char -> t
(** print a single quoted character, escaped if necessary *)

val int: int -> t
(** print an integer *)

val float: float -> t
(** print a floating point value *)

val string: string -> t
(** print a double quoted string, with necessary escapes *)

val constant: string -> t
(** print a literal string (without quoting), useful for non-parameterized
    data constructors. [constant "None"] prints `None`. *)

val constructor: string -> t -> t
(** print a parameterized dataconstructor.
    [constructor "Some" unit] prints `Some ()`. *)

val tuple: t list -> t
(** print an OCaml tuple.
    [tuple [int 1; char 'c']] prints `(1, 'c')`. *)

val record: (string * t) list -> t
(** print an OCaml record.
    [record ["a", int 1; "b", bool false]] prints `{a: 1, b: false}`. *)

val cons: t -> t -> t
(** construct a cons cell.
    [cons (int 1) nil] prints `[1]`,
    [cons (int 1) (constant "xs")] prints `1 :: xs`, *)

val construct: string -> t list -> t
(** Shortcut for a data constructor with multiple arguments.
    [construct "None" []] = [constant "None"] prints `None`,
    [construct "Some" [int 1]] = [constructor "Some" (int 1)] prints `Some 1`,
    [construct "A" [int 1; int 2] = [constructor "A" (tuple [int 1; int 2])]
    prints `A (1, 2)`.
*)

val crecord: string -> (string * t) list -> t
(** Shortcut for constructor with inline record.
    [crecord "A" ["a", int 1; "b", bool false]] prints `A {a: 1, b: false}`. *)

val nil: t
(** nil prints `[]` *)

val list: t list -> t
(** [list xs] = [List.fold_right cons xs nil].
    [list [int 1; int 2; int 3]] prints `[1; 2; 3]`. *)

(** Variants that prevent sharing these values *)

val unshared_string: string -> t
val unshared_constructor: string -> t -> t
val unshared_tuple: t list -> t
val unshared_record: (string * t) list -> t
val unshared_construct: string -> t list -> t
val unshared_crecord: string -> (string * t) list -> t
val unshared_list: t list -> t

val explicit_sharing: t -> t
(** Rewrite a value, introducing let-binders to make sharing explicit. *)

val print_as_is: t -> PPrint.document
(** Print the value as it is (without changing sharing) to a
    [PPrint.document]. *)


val print: t -> PPrint.document
(** Print the value with explicit sharing to a [PPrint.document].
    [print t == print_as_is (explicit_sharing t)]. *)

val format_document : Format.formatter -> PPrint.document -> unit
(** Print a PPrint document on a formatter while trying to follow respect the
    margin specification of the formatter. *)

val format_as_is: Format.formatter -> t -> unit
(** Format the value as it is (without changing sharing) to a
    [Format.formatter] *)

val format: Format.formatter -> t -> unit
(** Format the value with explicit sharing to a [Format.formatter].
    [format t == format_as_is (explicit_sharing t)].

    To display cmon values in a top-level, you can use #install_printer format.
    For instance:

    utop # Cmon.unit;;
    - : Cmon.t = Cmon.Unit
    utop # #install_printer Cmon.format;;
    utop # Cmon.unit;;
    - : Cmon.t = ()
*)
