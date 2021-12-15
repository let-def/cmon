let print name value =
  print_string name;
  print_endline ":";
  PPrint.ToChannel.pretty 0.9 80 stdout (Cmon.print value);
  print_newline ();
  print_newline ()

let short_args = Cmon.[int 1; bool true]
let mid_args = short_args @ short_args @ short_args
let long_args = mid_args @ mid_args @ mid_args

let short_fields = Cmon.["a", int 1; "b", bool true]
let mid_fields = short_fields @ short_fields @ short_fields
let long_fields = mid_fields @ mid_fields @ mid_fields

let () =
  (* Base types *)
  print "unit"  Cmon.unit;
  print "false" (Cmon.bool false);
  print "true"  (Cmon.bool true);

  (* Integers *)
  print "0"       (Cmon.int 0);
  print "1"       (Cmon.int 1);
  print "2"       (Cmon.int 2);
  print "-1"      (Cmon.int (-1));
  print "-2"      (Cmon.int (-2));
  print "max_int" (Cmon.int max_int);
  print "min_int" (Cmon.int min_int);

  (* Floats *)
  print "0."        (Cmon.float 0.0);
  print "1."        (Cmon.float 1.0);
  print "-0."       (Cmon.float (-0.0));
  print "-1."       (Cmon.float (-1.0));
  print "+inf"      (Cmon.float ( 1.0 /. 0.0));
  print "-inf"      (Cmon.float (-1.0 /. 0.0));
  print "nan"       (Cmon.float ( 0.0 /. 0.0));
  print "max_float" (Cmon.float max_float);
  print "min_float" (Cmon.float min_float);

  (* Chars *)
  print "'a'"    (Cmon.char 'a');
  print "'\\x00'" (Cmon.char '\x00');
  print "'\\xFF'" (Cmon.char '\xFF');
  print "'\\t'"    (Cmon.char '\t');
  print "'\\n'"    (Cmon.char '\n');

  (* Strings *)
  print "\"foo\"" (Cmon.string "foo");
  print "\"\\\"foo\\\"\"" (Cmon.string "\"foo\"");
  print "\"foo\\nbar\"" (Cmon.string "foo\nbar");

  (* Tuple *)
  print "(1, true)" (Cmon.tuple short_args);
  print "(1, true, ...)" (Cmon.tuple mid_args);
  print "(1, true, ..., ...)" (Cmon.tuple long_args);

  (* Terminated lists *)
  print "[1; true]" (Cmon.list short_args);
  print "[1; true; ...]" (Cmon.list mid_args);
  print "[1; true; ...; ...]" (Cmon.list long_args);

  (* Open lists *)
  let open_list xs = List.fold_right Cmon.cons xs (Cmon.constant "xs") in
  print "1 :: true :: xs" (open_list short_args);
  print "1 :: true :: ... :: xs" (open_list mid_args);
  print "1 :: true :: ... ... :: xs" (open_list long_args);

  (* Nested lists and tuples *)
  let step = ref 0 in
  let rec deep n =
    incr step;
    let is_tuple = !step land 1 = 0 in
    if n = 0 then (
      if is_tuple
      then Cmon.unit
      else Cmon.nil
    ) else (
      let left = deep (n - 1) in
      let right = deep (n - 1) in
      if is_tuple
      then Cmon.tuple [left; right]
      else Cmon.list [left; right]
    )
  in
  print "nested-1" (deep 3);
  print "nested-2" (deep 6);

  (* Constructors *)
  print "data-constructors-1"
    (Cmon.tuple [
        Cmon.constant "None";
        Cmon.constructor "Some" Cmon.unit;
        Cmon.constructor "Pair" (Cmon.tuple [Cmon.int 1; Cmon.int 2]);
      ]);

  print "data-constructors-2"
    (Cmon.tuple [
        Cmon.construct "None" [];
        Cmon.construct "Some" [Cmon.unit];
        Cmon.construct "Pair" [Cmon.int 1; Cmon.int 2];
      ]);

  (* Records *)
  print "record-1" (Cmon.record ["contents", Cmon.int 1]);
  print "record-2" (Cmon.record short_fields);
  print "record-3" (Cmon.record mid_fields);
  print "record-4" (Cmon.record long_fields);

  (* Inline records *)
  print "inline-record-1" (Cmon.crecord "R1" ["contents", Cmon.int 1]);
  print "inline-record-2" (Cmon.crecord "R2" short_fields);
  print "inline-record-3" (Cmon.crecord "R3" mid_fields);
  print "inline-record-4" (Cmon.crecord "R4" long_fields);

  (* Sharing test *)
  let shared_term1 =
    Cmon.tuple [Cmon.string "test"; Cmon.int 1] in
  let shared_term2 =
    Cmon.tuple [shared_term1; shared_term1] in
  let shared_term1' =
    Cmon.tuple [Cmon.string "test"; Cmon.int 1] in
  let shared_term2' =
    Cmon.tuple [shared_term1'; shared_term1'] in
  let shared_term3 =
    Cmon.tuple [shared_term2; shared_term2; shared_term2'] in
  let shared_term4 =
    Cmon.tuple [shared_term3; shared_term2'] in
  print "shared-terms" shared_term4
