(*
 * Copyright (c) 2021 Frédéric Bour <frederic.bour@lakaban.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


type id = int let id_k = ref 0
let id = fun () -> incr id_k; !id_k
let unshared = 0

type var = int ref
let var_id : var -> int = (!)

type t =
  | Unit
  | Nil
  | Bool of bool
  | Char of char
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float of float
  | Constant of string
  | Cons of {id: id; car: t; cdr: t}
  | String of {id: id; data: string}
  | Tuple of {id: id; data: t list}
  | Record of {id: id; data: (string * t) list}
  | Constructor of {id: id; tag: string; data: t}
  | Array of {id: id; data: t array}
  | Lazy of {id: id; data: t lazy_t}
  | Var of var
  | Let of {id: id; recursive: bool; bindings: (var * t) list; body: t}

let nil  = Nil
let unit = Unit
let bool data = Bool data
let char data = Char data
let int data = Int data
let int32 data = Int32 data
let int64 data = Int64 data
let nativeint data = Nativeint data
let float data = Float data
let string data = String {id=id(); data}
let constant tag = Constant tag
let constructor tag data = Constructor {id=id(); tag; data}
let tuple data = Tuple {id=id(); data}
let record data = Record {id=id(); data}
let cons car cdr = Cons {id=id(); car; cdr}

let unshared_string data = String{id=unshared; data}
let unshared_constructor tag data = Constructor {id=unshared; tag; data}
let unshared_tuple data = Tuple {id=unshared; data}
let unshared_record data = Record {id=unshared; data}
let unshared_cons car cdr = Cons {id=unshared; car; cdr}

let list xs = List.fold_right cons xs nil
let list_map f xs = list (List.map f xs)
let unshared_list xs = List.fold_right unshared_cons xs nil

let array data = Array{id=id(); data}
let array_map f xs = array (Array.map f xs)
let unshared_array data = Array {id=unshared; data}

let construct tag = function
  | []  -> constant tag
  | [x] -> constructor tag x
  | xs  -> constructor tag (unshared_tuple xs)

let unshared_construct tag = function
  | []  -> constant tag
  | [x] -> unshared_constructor tag x
  | xs  -> unshared_constructor tag (unshared_tuple xs)

let crecord tag data = constructor tag (unshared_record data)
let unshared_crecord tag data = unshared_constructor tag (unshared_record data)

let id_of = function
  | Bool _ | Char _ | Int _ | Int32 _ | Int64 _ | Nativeint _
  | Float _ | Nil | Unit | Constant _ | Var _ -> unshared
  | Tuple {id; _} | Record {id; _} | Constructor {id; _} | Cons {id; _}
  | String {id; _} | Let {id; _} | Array {id; _} | Lazy {id; _} -> id

let graph : t Fastdom.graph = {
  successors = begin fun f acc ->
    let rec aux acc = function
      | Bool _ | Char _ | Int _ | Int32 _ | Int64 _ | Nativeint _
      | Float _ | Nil | Unit | Constant _
      | String _ | Var _ | Let _ -> acc
      | Lazy {data = lazy t; _} -> f_ acc t
      | Tuple {data; _} -> List.fold_left f_ acc data
      | Record {data; _} -> List.fold_left f_field acc data
      | Array {data; _} -> Array.fold_left f_ acc data
      | Constructor {data; _} -> f_ acc data
      | Cons {car; cdr; _} -> f_ (f_ acc car) cdr
    and f_field acc (_, v) =
      f_ acc v
    and f_ acc self =
      if id_of self <> unshared
      then f acc self
      else aux acc self
    in
    aux acc
  end;
  memoize = begin fun (type b) (f : _ -> b) ->
    let table : (id, b) Hashtbl.t = Hashtbl.create 7 in
    fun x ->
      let id = id_of x in
      if id = unshared then f x else
        try Hashtbl.find table id
        with Not_found ->
          let y = f x in
          Hashtbl.add table id y;
          y
  end;
}

type occurrence = {
  mutable min_scope: int;
  cursor: int ref;
}

let explicit_sharing t =
  let postorder, dominance = Fastdom.dominance graph t in
  let count = Array.length postorder in
  let bindings = Array.make count [] in
  let var_name = Array.make count (ref min_int) in
  let share tag = match Fastdom.predecessors tag with
    | [] -> false
    | [_] -> Fastdom.node tag == t
    | _ :: _ :: _ -> true
  in
  for i = count - 1 downto 0 do
    let tag = postorder.(i) in
    if share tag then begin
      let name = ref min_int in
      var_name.(i) <- name;
      let dominator = Fastdom.dominator tag in
      let index = Fastdom.postorder_index dominator in
      bindings.(index) <- (name, tag) :: bindings.(index)
    end
  done;
  let null_occurrence = {min_scope = 0; cursor = ref 0} in
  let rec_occurrences = Array.make count null_occurrence in
  let fresh_name = ref 0 in
  let rec traverse ~is_binding t =
    let cursor = ref max_int in
    let bindings, t =
      if id_of t = unshared then
        ([], t)
      else
        let tag = dominance t in
        let id = Fastdom.postorder_index tag in
        if share tag && not is_binding then (
          let occ = rec_occurrences.(id) in
          if !(occ.cursor) < occ.min_scope then
            occ.min_scope <- !(occ.cursor);
          ([], Var var_name.(id))
        ) else (
          match bindings.(id) with
          | [] -> ([], t)
          | bindings' ->
            bindings.(id) <- [];
            let init_occurrence (_, tag) =
              rec_occurrences.(Fastdom.postorder_index tag) <- {
                min_scope = max_int;
                cursor;
              }
            in
            List.iter init_occurrence bindings';
            (bindings', t)
        )
    in
    let t = match t with
      | Bool _ | Char _ | Int _ | Int32 _ | Int64 _ | Nativeint _
      | Float _ | Nil | Unit | Constant _
      | String _ | Var _ | Let _ -> t
      | Lazy {data = lazy t; _} -> traverse_child t
      | Tuple t ->
        unshared_tuple (List.map traverse_child t.data)
      | Record t ->
        unshared_record (List.map (fun (k,v) -> k, traverse_child v) t.data)
      | Constructor t ->
        unshared_constructor t.tag (traverse_child t.data)
      | Cons t ->
        unshared_cons (traverse_child t.car) (traverse_child t.cdr)
      | Array t ->
        unshared_array (Array.map traverse_child t.data)
    in
    match List.mapi traverse_binding bindings with
    | [] -> t
    | bindings ->
      let normalize_scope (_, occ, _) min_scope =
        if min_scope < occ.min_scope then (
          occ.min_scope <- min_scope;
          min_scope
        ) else
          occ.min_scope
      in
      ignore (List.fold_right normalize_scope bindings max_int : int);
      let prepare ~recursive group =
        let group = if recursive then group else List.rev group in
        List.iter (fun (var, _) ->
            var := !fresh_name;
            incr fresh_name
          ) group;
        group
      in
      let let_ ~recursive group body =
        match group with
        | [] -> body
        | bindings -> Let {id=id(); recursive; bindings; body}
      in
      let rec nonrec_bindings group scope_limit index = function
        | [] ->
          let group = prepare ~recursive:false group in
          let_ ~recursive:false group t
        | (var, occ, t') :: bindings when occ.min_scope > index ->
          if index >= scope_limit then (
            let group = prepare ~recursive:false group in
            let_ ~recursive:false group
              (nonrec_bindings [var, t'] (index + 1) occ.min_scope bindings)
          ) else
            nonrec_bindings
              ((var, t') :: group)
              (min occ.min_scope scope_limit) (index + 1) bindings
        | bindings ->
          let group = prepare ~recursive:false group in
          let_ ~recursive:false group (rec_bindings [] index bindings)
      and rec_bindings group index = function
        | (var, occ, t') :: bindings when occ.min_scope <= index ->
          rec_bindings ((var, t') :: group) (index + 1) bindings
        | bindings ->
          let group = prepare ~recursive:true group in
          let_ ~recursive:true group
            (nonrec_bindings [] max_int index bindings)
      in
      nonrec_bindings [] max_int 0 bindings
  and traverse_child t =
    traverse ~is_binding:false t
  and traverse_binding index (var, tag) =
    let occ = rec_occurrences.(Fastdom.postorder_index tag) in
    occ.cursor := index;
    (var, occ, traverse ~is_binding:true (Fastdom.node tag))
  in
  traverse ~is_binding:true t

(*let explicit_sharing t =
  explicit_sharing (Lazy {id=id(); data=lazy t})*)

let rec list_of_cons acc = function
  | Cons {id = _; car; cdr} -> list_of_cons (car :: acc) cdr
  | Nil -> List.rev acc, None
  | other -> List.rev acc, Some other

let print_record f fields =
  let add_field acc x =
    let k, v = f x in
    PPrint.(acc ^/^ group (group(string k ^/^ char '=') ^^
                           nest 2 (break 1 ^^ v) ^^ char ';'))
  in
  let fields = List.fold_left add_field PPrint.empty fields in
  PPrint.(group (string "{" ^^ nest 2 fields ^/^ string "}"))

let rec sub_print_as_is =
  let open PPrint in
  function
  | Unit    -> true, string "()"
  | Nil     -> true, string "[]"
  | Constant tag -> true, string tag
  | Bool b  -> true, OCaml.bool b
  | Char c  -> true, OCaml.char c
  | Int  i  -> true, OCaml.int i
  | Int32 i -> true, OCaml.int32 i
  | Int64 i -> true, OCaml.int64 i
  | Nativeint i -> true, OCaml.nativeint i
  | Float f -> true, OCaml.float f
  | Lazy {id=_; data=lazy t} as t' ->
    if t == t'
    then (true, string "<cycle>")
    else sub_print_as_is t
  | Cons _ as self ->
    begin match list_of_cons [] self with
      | items, None ->
        true, OCaml.list print_as_is items
      | items, Some cdr ->
        false,
        group (
          let print_one item =
            group (string "::" ^/^ item)
          in
          let rec print = function
            | [] -> print_one (print_as_is cdr)
            | x :: xs -> print_one (print_as_is x) ^^ break 1 ^^ print xs
          in
          match items with
          | x :: xs -> print_as_is x ^^ break 1 ^^ print xs
          | [] -> assert false
        )
    end
  | Array {id=_; data} -> true, OCaml.array print_as_is data
  | String {id=_; data} -> true, OCaml.string data
  | Tuple {id=_; data} ->
    true, OCaml.tuple (List.map print_as_is data)
  | Record {id=_; data} ->
    true,
    (*OCaml.record "" (List.map (fun (k,v) -> k, print_as_is v) data)*)
    print_record (fun (k,v) -> k, print_as_is v) data
  | Constructor {id=_; tag; data} ->
    let delimited, sub_doc = sub_print_as_is data in
    let doc =
      if delimited
      then sub_doc
      else OCaml.tuple [sub_doc]
    in
    false, group (string tag ^^ blank 1 ^^ doc)
  | Var id -> true, string ("v" ^ string_of_int !id)
  | Let {id=_; recursive; bindings; body} ->
    let rec print_bindings prefix = function
      | [] -> string "in"
      | (id, value) :: values ->
        let id = string ("v" ^ string_of_int !id) in
        let doc = print_as_is value in
        let need_break = match value with Let _ -> true | _ -> false in
        let doc =
          group @@
          if need_break
          then group (string prefix ^/^ id ^/^ string "=") ^^
               nest 2 (break 1 ^^ doc)
          else group (string prefix ^/^ id ^/^ string "= ") ^^
               nest 2 doc
        in
        doc ^/^ print_bindings "and" values
    in
    let prefix = if recursive then "let rec" else "let" in
    let bindings = group (print_bindings prefix bindings) in
    false,
    bindings ^/^
    print_as_is body

and print_as_is doc =
  let _delim, doc = sub_print_as_is doc in
  doc

let format_document ppf doc : unit =
  let margin = Format.pp_get_margin ppf () in
  Format.fprintf ppf "@[%a@]" (PPrint.ToFormatter.pretty 0.9 margin) doc

let format_as_is ppf t : unit =
  format_document ppf (print_as_is t)

let print t : PPrint.document = print_as_is (explicit_sharing t)
let format ppf t : unit = format_as_is ppf (explicit_sharing t)

let of_lazy data = Lazy {id=id(); data}
