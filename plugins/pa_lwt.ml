open Camlp4.PreCast ;;
open Syntax ;;
open Camlp4_to_ppx ;;

let linkme = () ;;

EXTEND Gram
  GLOBAL: expr str_item;

lwt_binding: [[ SELF; "and"; SELF | patt; "="; expr ]];

located_lwt: [[ "lwt" -> _loc ]];

expr: LEVEL "top" [
  [ loc = ["try_lwt" -> _loc];
    expr LEVEL ";";
    OPT ["with"; match_case];
    fnl = OPT [loc = ["finally" -> ()]; sequence -> loc] -> (
    match fnl with
    | Some _ ->
      failwith "\"try_lwt ... finally ...\" not supported"
    | None ->
      (replace loc "try%lwt"; <:expr<>>))
  | loc = located_lwt; lwt_binding; "in"; expr LEVEL ";" -> (
    replace loc "let%lwt";
    <:expr<>>)
  | loc = ["for_lwt" -> _loc]; patt;
    b = [ "="; sequence; "to"; sequence -> true
        | "="; sequence; "downto"; sequence -> true
        | "in"; sequence -> false];
    "do"; do_sequence -> (
    if b then
      (replace loc "for%lwt"; <:expr<>>)
    else
      failwith "\"for_lwt ... in ...\" not supported")
  | loc = ["raise_lwt" -> _loc]; SELF -> (
    replace loc "[%lwt raise (";
    print_after _loc ")]";
    <:expr<>>)
  | loc = ["assert_lwt" -> _loc]; SELF -> (
    replace loc "assert%lwt (";
    print_after _loc ")";
    <:expr<>>)
  | loc = ["while_lwt" -> _loc]; sequence; "do"; sequence; "done" -> (
    replace loc "while%lwt";
    <:expr<>>)
  | loc = ["match_lwt" -> _loc]; sequence; "with"; match_case -> (
    replace loc "match%lwt";
    <:expr<>>)
  ] ];

str_item: [
  [ loc = located_lwt; lwt_binding ->
    failwith "toplevel \"lwt\" bindings not supported"
  | loc = located_lwt; lwt_binding; "in"; expr ->
    failwith "toplevel \"lwt\" bindings not supported"
  ]
];

END
