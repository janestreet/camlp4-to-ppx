open Printf
open StdLabels
open Camlp4.PreCast
open Syntax

let module M =
  Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Camlp4.PreCast.Syntax))
in ()

let program_name = Filename.basename Sys.executable_name

let input_file =
  match Sys.argv with
  | [| _; fname |] -> fname
  | _ ->
    Printf.eprintf "Usage: %s FILE\n" program_name;
    exit 2

type kind = Signature | Structure

let kind =
  if Filename.check_suffix input_file ".mli" then Signature else
  if Filename.check_suffix input_file ".ml"  then Structure else begin
    Printf.eprintf "%s: unknown suffix in filename: %s\n"
      program_name input_file;
    exit 2
  end

let file_contents =
  let ic = open_in input_file in
  let len = in_channel_length ic in
  let str = Bytes.create len in
  really_input ic str 0 len;
  str

type subst =
  { start : int
  ; stop  : int
  ; repl  : string
  }

let substs = ref []

let do_output () =
  let rec loop pos substs =
    match substs with
    | [] ->
      output_substring stdout file_contents pos (String.length file_contents - pos)
    | { start; stop; repl } :: rest ->
      assert (pos <= start && start <= stop);
      output_substring stdout file_contents pos (start - pos);
      output_string stdout repl;
      loop stop rest
  in
  let substs = List.sort !substs ~cmp:(fun a b -> compare a.start b.start) in
  loop 0 substs
;;

let add_subst ~start ~stop ~repl =
  substs := { start; stop; repl } :: !substs

let replace loc s =
  add_subst ~start:(Loc.start_off loc) ~stop:(Loc.stop_off loc) ~repl:s

let print_at ~pos s =
  add_subst ~start:pos ~stop:pos ~repl:s

let print_before loc s =
  print_at ~pos:(Loc.start_off loc) s

let print_after loc s =
  print_at ~pos:(Loc.stop_off loc) s

let erase_keyword loc =
  let start = Loc.start_off loc in
  let stop  = Loc.stop_off  loc in
  if start > 0                          && file_contents.[start - 1] = ' ' &&
    stop  < String.length file_contents && file_contents.[stop     ] = ' ' then
    add_subst ~start ~stop:(stop + 1) ~repl:""
  else
    add_subst ~start ~stop ~repl:""

let skip_trailing_semi loc =
  let stop = Loc.stop_off loc in
  if stop > 0 && file_contents.[stop - 1] = ';' then
    add_subst ~start:(stop-1) ~stop ~repl:""

DELETE_RULE Gram let_binding: ipatt; fun_binding END

(* [let _ a = a] is a syntax error in ocaml because a single underscore is a pattern,
   but not an identifier as required by the ocaml parser in a function-style binding.

   However, if camlp4 is used to preprocess, the illegal syntax is masked by camlp4's
   more permissive grammar. In effect the example gets rewritten as [let _ = fun a -> a]

   We do the same translation as camlp4. *)
EXTEND Gram
  GLOBAL: let_binding;

  equal: [ [ "=" -> _loc ] ];

   unquoted_typevars:
     [ LEFTA
        [ SELF; SELF -> ()
        | a_ident -> ()
      ] ] ;

  cvalue_binding:
    [ [ l = equal; expr -> l
      | ":"; "type"; unquoted_typevars; "." ; ctyp ; l = equal; expr -> l
      | ":"; poly_type; l = equal; expr -> l
      | ":"; poly_type; ":>"; ctyp; l = equal; expr -> l
      | ":>"; ctyp; l = equal; expr -> l ] ];

  fun_binding:
    [ RIGHTA
        [ TRY ["("; "type"]; a_LIDENT; ")"; (n, x) = SELF -> (n + 1, x)
        | TRY labeled_ipatt; (n, x) = SELF -> (n + 1, x)
        | x = cvalue_binding -> (0, x)
        ] ];

  let_binding:
    [ [ p = ipatt; (n, loc_eq) = fun_binding ->
      begin match p with
      | <:patt@ploc< _ >> when n > 0 ->
        print_after ploc " = fun";
        replace loc_eq "->"
      | _ -> ()
      end;
      <:binding<  >>
    ] ] ;
END

DELETE_RULE Gram expr: `LABEL _; SELF END;

EXTEND Gram
  GLOBAL: expr;

  located_expr: [[expr -> _loc]];

  expr: LEVEL "label"
    [[ `LABEL _; expr LEVEL "." -> <:expr< >>
     | `LABEL _; e_loc = located_expr ->
     begin
       if not (file_contents.[Loc.start_off e_loc] = '(') then (
         print_before e_loc "(";
         print_after  e_loc ")";
       );
       <:expr< >>
     end
     ]];
END

type payload_kind = Str | Typ | Pat

let payload_kinds = Hashtbl.create 128

let set_payload_kind ~quotation_name kind =
  Hashtbl.add payload_kinds quotation_name kind

(* Update quotations *)
let () = DELETE_RULE Gram expr: `QUOTATION  _ END
EXTEND Gram
  expr: LEVEL "simple"
    [[ `QUOTATION q ->
       let { Camlp4.Sig. q_name; q_contents; q_loc=_; q_shift=_ } = q in
       let kind_marker =
         match Hashtbl.find payload_kinds q_name with
         | exception Not_found -> ":"
         | Typ -> ":"
         | Pat -> "?"
         | Str -> ""
       in
       let start = Loc.start_off _loc in
       let stop  = Loc.stop_off  _loc in
       add_subst ~start ~stop:(start + 2) ~repl:"[%";
       (let start = start + 2 + String.length q_name in
        add_subst ~start ~stop:(start + 1) ~repl:kind_marker);
       add_subst ~start:(stop - 2) ~stop ~repl:"]";
       <:expr< >>
    ]];
END

external not_filtered : 'a -> 'a Gram.not_filtered = "%identity"
external filtered     : 'a Gram.not_filtered -> 'a = "%identity"

let fix_lexer_stream stream =
  let maybe_last = ref None in
  (* Fix the Camlp4 lexer. Start locations are often wrong but end locations are always
     correct. *)
  let next _i =
    (* [loc] is this location, [loc'] is the last location *)
    let tok, loc = Stream.next stream in
    match !maybe_last with
    | None ->
      maybe_last := Some loc;
      Some (tok, loc)
    | Some loc' ->
      maybe_last := Some loc;
      if Loc.file_name loc' = Loc.file_name loc then
        let _, _, _, _, a, b, c, _ = Loc.to_tuple loc'
        and n, _, _, _, d, e, f, g = Loc.to_tuple loc in
        Some (tok, Loc.of_tuple (n, a, b, c, d, e, f, g))
      else
        Some (tok, loc)
  in
  Stream.from next

let rec parse entry token_stream =
  let _, stopped_at_directive = Gram.parse_tokens_before_filter entry token_stream in
  match stopped_at_directive with
  | Some (_ : Loc.t) ->
    parse entry token_stream
  | None ->
    ()

let main_internal () =
  let token_stream = Gram.lex_string (Loc.mk input_file) file_contents in
  let token_stream =
    token_stream
    |> filtered
    |> fix_lexer_stream
    |> not_filtered
  in
  (match kind with
   | Structure -> parse Syntax.implem token_stream
   | Signature -> parse Syntax.interf token_stream);
  do_output()

let main () =
  try
    main_internal ()
  with exn ->
    Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exn;
    exit 2
