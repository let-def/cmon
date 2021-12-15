# CMON : CaMl Object Notation

`Cmon` provides a few tools for printing values. This "object notation" mimics the syntax of OCaml literal values: tuples, named constructors and records.

No parser is provided (yet?). The library is intended to print values, for logging and debugging, that can be copy-pasted to an OCaml toplevel.

The unusual feature is that the printer makes sharing explicit: let-binding are introduced at the optimal place by computing dominating nodes. This is particularly convenient for printing the internal structures of symbolic manipulation tools (e.g. typecheckers) that can internally make use of sharing a lot.

## Example

A few samples, from the toplevel:

```ocaml
# #require "cmon";;
# #install_printer Cmon.format;;
# Cmon.unit
- : Cmon.t = ()
# Cmon.record ["int", Cmon.int 42; "bool", Cmon.bool false];;
- : Cmon.t = { int = 42; bool = false; }
# let ty1 = Cmon.construct "Ty_arrow" [
    Cmon.construct "Ty_int" [];
    Cmon.construct "Ty_int" []
  ];;
val ty1 : Cmon.t = Ty_arrow (Ty_int, Ty_int)
# (* Printing the type derivation for a fictitious, simple,
     programming language to illustrate sharing of sub values *)
  Cmon.crecord "Let_binding" [
    "name", Cmon.string "id_int";
    "typ", ty1; 
    "body", Cmon.crecord "Exp_fun" [
      "var", Cmon.string "x";
      "typ", ty1;
      "body", Cmon.construct "Exp_ident" [Cmon.string "x"]
    ]
  ];;
- : Cmon.t =
let v0 = Ty_arrow (Ty_int, Ty_int) in
Let_binding {
  name = "id_int";
  typ = v0;
  body = Exp_fun { var = "x"; typ = v0; body = Exp_ident "x"; };
}
```

## Installation and usage

The library is distributed on opam:

```shell
$ opam install cmon
```

Add `cmon` in the `libraries` of a `dune` file to use it.

## Documentation

Simple values are built using the functions `unit`, `bool`, `char`, `int`, `float`, `string`, `tuple`, `record`, `nil`, `cons`, `list`, `constant`, `construct` and `crecord`.

```ocaml
# #require "cmon";;
# #install_printer Cmon.format;;
# open Cmon;;
# unit;;
- : t = ()
# bool false;;
- : t = false
# int 10;;
- : t = 10
# char 'a';;
- : t = 'a'
# float 10.0;;
- : t = 10.
# string "foo";;
- : t = "foo"
# tuple [int 42; bool true];;
- : t = (42, true)
# record ["field1", int 1; "field2", char 'b'];;
- : t = { field1 = 1; field2 = 'b'; }
# nil;;
- : t = []
# cons (int 1) nil;;
- : t = [ 1 ]
# cons (int 1) (constant "x");;
- : t = 1 :: x
# cons (int 1) (constant "xs");;
- : t = 1 :: xs
# list [int 1; int 2; int 3];;
- : t = [ 1; 2; 3 ]
# crecord "Inline_record" ["field1", int 1; "field2", char 'b'];;
- : t = Inline_record { field1 = 1; field2 = 'b'; }
```

Sharing is enabled by default for all compound values and strings. 

```ocaml
# let twice x = Cmon.tuple [x;x];;
# twice (string "foo-bar-baz");;
- : t = let v0 = "foo-bar-baz" in
        (v0, v0)
# twice (construct "None" []);;
- : t = (None, None)
# twice (construct "Some" [bool true]);;
- : t = let v0 = Some true in
        (v0, v0)
```

To opt out, use the `unshared_*` variants of the main functions to opt out: `unshared_construct`, `unshared_crecord`, `unshared_list`, `unshared_record`, `unshared_string`, `unshared_tuple`.

```ocaml
# twice (unshared_string "foo-bar-baz");;
- : t = ("foo-bar-baz", "foo-bar-baz")
# twice (unshared_construct "Some" [bool true]);;
- : t = (Some true, Some true)
```

Sharing is based on the physical identities of `Cmon.t` values, not their structure:

```ocaml
# let s = string "foo-bar-baz" in
  Cmon.tuple [s; s];;
- : t = let v0 = "foo-bar-baz" in
        (v0, v0)
# Cmon.tuple [string "foo-bar-baz"; string "foo-bar-baz"];;
- : t = ("foo-bar-baz", "foo-bar-baz")
```

Once composed, a `Cmon.t` value can be converted to a [`PPrint.document`](https://github.com/fpottier/pprint) or directly sent to a `Format.formatter`:

```ocaml
val Cmon.print : t -> PPrint.document
val Cmon.format : Format.formatter -> t -> unit
```

## Inspecting `Cmon.t` values

`Cmon.t` is defined as a private alias of an algebraic type: 

```ocaml
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

```

It is possible to match on `Cmon.t` values and inspect their structure, but it is not possible to create new ones directly; one has to use the public functions.

The `Cmon.explicit_sharing` function can reveal the sharing that would be displayed by the `print` functions:

```ocaml
# let c = Cmon.tuple [s; s];;
val c : t =
  Tuple
   {Cmon.id = 6;
    data =
     [String {Cmon.id = 4; data = "foo-bar-baz"};
      String {Cmon.id = 4; data = "foo-bar-baz"}]}
# Cmon.explicit_sharing c;;
- : t =
Let
 {Cmon.id = 5; bindings = [(0, String {Cmon.id = 1; data = "foo-bar-baz"})];
  body = Tuple {Cmon.id = 0; data = [Var 0; Var 0]}}
```

To print without introducing more sharing, the `_as_is` variants are provided:

```ocaml
val Cmon.print : t -> PPrint.document
val Cmon.format : Format.formatter -> t -> unit
```

Mixing `explicit_sharing` and `print_as_is` can be useful to print composite documents while "controlling" sharing boundaries.

## 