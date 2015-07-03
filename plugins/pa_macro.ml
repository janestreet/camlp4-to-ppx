open Camlp4.PreCast
open Syntax
open Camlp4_to_ppx

let all_spaces_between start stop =
  let rec loop ofs =
    if ofs >= stop then
      true
    else
      match file_contents.[ofs] with
      | ' ' | '\t' -> loop (ofs + 1)
      | _ -> false
  in
  loop start

let replace_macro ?(addnl=false) loc dir =
  let start =
    if all_spaces_between (Loc.start_bol loc) (Loc.start_off loc) then
      Loc.start_bol loc
    else
      Loc.start_off loc
  in
  add_subst ~start ~stop:(Loc.stop_off loc) ~repl:dir;
  if Loc.stop_off loc < String.length file_contents &&
     file_contents.[Loc.stop_off loc] <> '\n' && addnl then
    print_after loc "\n"
;;

(* pa_macro *)
EXTEND Gram
  GLOBAL: str_item sig_item expr patt;

  define:   [ [ "DEFINE"  -> replace_macro _loc "#define"            ] ];
  ifdef:    [ [ "IFDEF"   -> replace_macro _loc "#ifdef"             ] ];
  ifndef:   [ [ "IFNDEF"  -> replace_macro _loc "#ifndef"            ] ];
  then_:    [ [ "THEN"    -> replace       _loc ""                   ] ];
  else_:    [ [ "ELSE"    -> replace_macro _loc "#else"  ~addnl:true ] ];
  end_:     [ [ "END"     -> replace_macro _loc "#endif" ~addnl:true ] ];
  endif_:   [ [ "ENDIF"   -> replace_macro _loc "#endif" ~addnl:true ] ];
  include_: [ [ "INCLUDE" -> replace_macro _loc "#import"            ] ];

  equal: [ [ "=" -> erase_keyword _loc ] ];

  str_item: FIRST [ [ macro_def     -> <:str_item< >> ] ];
  sig_item: FIRST [ [ macro_def_sig -> <:sig_item< >> ] ];
  macro_def:
    [ [ define; uident; opt_macro_value -> ()
      | ifdef; uident_eval_ifdef;  then_; smlist_then; else_macro_def -> ()
      | ifndef; uident_eval_ifndef; then_; smlist_then; else_macro_def -> ()
      | include_; STRING -> () ] ]
    ;
    macro_def_sig:
      [ [ define; uident -> ()
        | ifdef;  uident_eval_ifdef; then_; sglist_then; else_macro_def_sig -> ()
        | ifndef; uident_eval_ifndef; then_; sglist_then; else_macro_def_sig -> ()
        | include_; STRING -> () ] ]
    ;
    uident_eval_ifdef:
      [ [ uident -> () ]]
    ;
    uident_eval_ifndef:
      [ [ uident -> () ]]
    ;
    else_macro_def:
      [ [ else_; smlist_else; endif -> ()
        | endif -> () ] ]
    ;
    else_macro_def_sig:
      [ [ else_; sglist_else; endif -> ()
        | endif -> () ] ]
    ;
    else_expr:
      [ [ else_; expr; endif -> ()
      | endif -> () ] ]
    ;
    smlist_then:
      [ [ LIST1 [ macro_def; semi -> () | str_item; semi -> () ] -> () ] ];
    smlist_else:
      [ [ LIST1 [ macro_def; semi -> () | str_item; semi -> () ] -> () ] ];
    sglist_then:
      [ [ LIST1 [ macro_def_sig; semi -> () | sig_item; semi -> () ] -> () ] ];
    sglist_else:
      [ [ LIST1 [ macro_def_sig; semi -> () | sig_item; semi -> () ] -> () ] ];
    endif:
      [ [ end_   -> ()
        | endif_ -> () ] ]
    ;
    opt_macro_value:
      [ [ equal; expr -> ()
        | -> () ] ]
    ;
    expr: LEVEL "top"
      [ [ ifdef ; uident; then_; expr; else_expr -> <:expr< >>
        | ifndef; uident; then_; expr; else_expr -> <:expr< >>
        ] ]
    ;
    patt:
      [ [ ifdef ; uident; then_; patt; else_; patt; endif -> <:patt< >>
        | ifndef; uident; then_; patt; else_; patt; endif -> <:patt< >>
        ] ]
    ;
    uident:
      [ [ UIDENT -> print_before _loc "JSC_" ] ]
    ;
    (* dirty hack to allow polymorphic variants using the introduced keywords. *)
    expr: BEFORE "simple"
      [ [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"
               | "DEFINE" ] -> <:expr<  >>
        | "`"; a_ident -> <:expr< >> ] ]
    ;
    (* idem *)
    patt: BEFORE "simple"
      [ [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ] -> <:patt<  >>
        | "`"; a_ident -> <:patt<  >> ] ]
    ;
END

let linkme = ()
