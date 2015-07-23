open StdLabels
open Camlp4.PreCast
open Syntax
open Camlp4_to_ppx

let get_body_text xs =
  let _ : (camlp4_token * Gram.token_info) list = xs in
  match xs with
  | [] -> ""
  | (_,first_info)::_ ->
    let (_,last_info) = (match List.rev xs with [] -> assert false | x::_ -> x) in
    let start = Loc.start_off (Gram.token_location first_info) in
    let stop = Loc.start_off (Gram.token_location last_info) in
    String.sub file_contents start (stop - start)

let mk_type_dec_with gens =
  Printf.sprintf "[@@deriving %s]"
    (String.concat ~sep:", "
       (List.map gens ~f:(fun (name,body_opt) ->
          match body_opt with
          | None -> name
          | Some body ->
            let _ : (camlp4_token * Gram.token_info) list = body in
            Printf.sprintf "%s ~%s" name (get_body_text body)
        )))

let mk_label_dec_with gens = (* i.e. [with default(..)] *)
  String.concat ~sep:" " (List.map gens ~f:(fun (name,body_opt) ->
    match body_opt with
    | None -> Printf.sprintf "[@%s]" name
    | Some body ->
      let _ : (camlp4_token * Gram.token_info) list = body in
      Printf.sprintf "[@%s %s]" name (get_body_text body)
  ))

let nonrec_opt =
  Gram.Entry.of_parser "nonrec" (fun strm ->
    match Stream.peek strm with
    | Some (LIDENT "nonrec", _info) ->
      Stream.junk strm;
      false
    | _ ->
      true)

let rec fetch_generator_arg paren_count acc strm =
  let token, token_info as elt = Stream.next strm in
  match token with
  | KEYWORD "(" ->
    fetch_generator_arg (paren_count + 1) (elt :: acc) strm
  | KEYWORD ")" when paren_count = 1 ->
    (EOI, token_info) :: acc
  | KEYWORD ")" ->
    fetch_generator_arg (paren_count - 1) (elt :: acc) strm
  | EOI ->
    Loc.raise (Gram.token_location token_info) (Stream.Error "')' missing")
  | _ ->
    fetch_generator_arg paren_count (elt :: acc) strm

let generator_arg =
  Gram.Entry.of_parser "generator_arg" (fun strm ->
    match Stream.peek strm with
    | Some (KEYWORD "(", _) ->
      Stream.junk strm;
      Some (List.rev (fetch_generator_arg 1 [] strm))
    | _ -> None)

EXTEND Gram

  GLOBAL: str_item sig_item label_declaration;

  generator: [[
    (*[ id = LIDENT; l = LIST1 [ "-"; x = LIDENT -> x ] -> (id, None, l)*)
    id = LIDENT; arg = generator_arg -> (id, arg)
  ]];

  with_generators:
    [[
      "with"; drvs = LIST1 generator SEP "," ->
      _loc,drvs
    ]];

  located_type_declaration:
    [[
     type_declaration ->
       _loc
    ]];

  str_item:
    [[
      "type"; nonrec_opt; located_type_declaration; (loc1,drvs) = with_generators ->
      replace loc1 (mk_type_dec_with drvs);
      <:str_item<>>
    | "type"; nonrec_opt; located_type_declaration ->
      <:str_item<>>
    ]];

  str_item:
    [[
      "exception"; constructor_declaration; (loc1,drvs) = with_generators ->
      replace loc1 (mk_type_dec_with drvs);
      <:str_item<>>
    | "exception"; constructor_declaration ->
      <:str_item<>>
    ]];

  sig_item:
    [[
      "type"; nonrec_opt; located_type_declaration; (loc1,drvs) = with_generators ->
      replace loc1 (mk_type_dec_with drvs);
      <:sig_item< >>
    | "type"; nonrec_opt; located_type_declaration ->
      <:sig_item<>>
    ]];

  sig_item:
    [[
      "exception"; constructor_declaration; (loc1,drvs) = with_generators ->
      replace loc1 (mk_type_dec_with drvs);
      <:sig_item<>>
    | "exception"; constructor_declaration ->
      <:sig_item<>>
    ]];

  label_declaration:
    [[
      a_LIDENT; ":"; poly_type; (loc1,drvs) = with_generators ->
      replace loc1 (mk_label_dec_with drvs);
      <:ctyp< >>
    | "mutable"; a_LIDENT; ":"; poly_type; (loc1,drvs) = with_generators ->
      replace loc1 (mk_label_dec_with drvs);
      <:ctyp< >>
    ]];
END

let linkme = ()
