open Camlp4.PreCast
open Syntax
open Camlp4_to_ppx

(* js_of_ocaml : pa_js *)
let loc_end_to_end x y =
  let _, _, _, _, a, b, c, _ = Loc.to_tuple x in
  let n, _, _, _, d, e, f, g = Loc.to_tuple y in
  Loc.of_tuple (n, a, b, c, d, e, f, g)

EXTEND Gram
  GLOBAL: expr;
  jsmeth: [[loc_op = [ "##" -> _loc ]; loc_label = [ label -> _loc] -> (loc_op,loc_label) ]];
  opt_class_self_patt_jsoo:
    [[ "("; p = patt; ")" -> p
     | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
     | -> <:patt<_>> ]];

  expr_within_comma_separated_list:
    [[ e = expr LEVEL ":="  ->
      begin match e with
      | <:expr< $lid:_$ >> -> _loc
      | <:expr< $uid:_$ >> -> _loc
      | _ ->
        print_before _loc "(";
        print_after _loc ")";
        _loc
      end
     ]];

  comma_exprs:
    [[ expr_within_comma_separated_list ; comma_loc = [ "," -> _loc ]; SELF ->
    replace comma_loc "";
       Some _loc
     | expr_within_comma_separated_list ->
       Some _loc
     | -> None ]];

  js_object: [[ "jsobject" -> replace _loc "object%js" ]];

  expr: BEFORE "."
    ["##" RIGHTA
        [ SELF; (loc_op,_) = jsmeth ->
        replace loc_op "##.";
          <:expr<dummy_ident_because_it_doesn't_need_parent_like_ident>>
        | SELF; (loc_op1,_) = jsmeth; loc_op2 = [ "<-" -> _loc ] ; expr LEVEL "top" ->
        replace loc_op1 "##.";
          replace loc_op2 ":=";
          <:expr<>>
        | e = SELF; (_loc_op,label_loc) = jsmeth;
          lpar_loc = ["(" -> _loc];
          args_loc = comma_exprs;
          rpar_loc = [")" -> _loc] ->
          match args_loc with
          | None ->
         (* No arguments *)
            replace (loc_end_to_end label_loc rpar_loc) "";
            begin match e with
            | <:expr< $lid:_$ >> -> ()
            | <:expr< $e$ >> ->
              let _loc = Ast.loc_of_expr e in
              print_before _loc "(";
              print_after _loc ")";
            end;
            <:expr<>>
          | Some args_loc ->
            print_before label_loc "(";
            replace lpar_loc "";
            replace rpar_loc "";
            if Loc.start_off args_loc = Loc.stop_off label_loc + 1
            then print_after label_loc " "
            else print_after label_loc "";
            print_after rpar_loc ")";
            <:expr<>>
        ]];

  expr: LEVEL "simple"
    [[ ["jsnew" -> replace _loc "new%js" ]; e_loc = [ expr LEVEL "label" -> _loc] ;
       lpar_loc = ["(" -> _loc];
       comma_exprs_loc = comma_exprs;
       rpar_loc = [")" -> _loc] ->
       begin
         match comma_exprs_loc with
         | Some _ ->
           replace lpar_loc "";
           replace rpar_loc "";
           <:expr< >>
         | None ->
           replace (loc_end_to_end e_loc _loc) "";
           <:expr<>>
       end
     | js_object; "end" ->
     <:expr<>>
     | js_object; opt_class_self_patt_jsoo; class_structure ; "end" ->
     <:expr<>>
     ]];
END

let rec filter stream =
  match stream with parser
    [< '(KEYWORD "#", loc); rest >] ->
      begin match rest with parser
        [< '(KEYWORD "#", loc') >] ->
          [< '(KEYWORD "##", Loc.merge loc loc'); filter rest >]
      | [< >] ->
        [< '(KEYWORD "#", loc); filter rest >]
      end
  | [< 'other; rest >] -> [< 'other; filter rest >]


let _ =
  Token.Filter.define_filter (Gram.get_filter ())
    (fun old_filter stream -> old_filter (filter stream))

let linkme = ()
