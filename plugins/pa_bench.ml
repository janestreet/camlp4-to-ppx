open Camlp4.PreCast
open Syntax
open Camlp4_to_ppx

EXTEND Gram
  GLOBAL: str_item;

  name:   [ [ `STRING (_, _) -> () ] ];

  bench:         [ [ "BENCH"         -> replace _loc "let%bench"         ] ];
  bench_fun:     [ [ "BENCH_FUN"     -> replace _loc "let%bench_fun"     ] ];
  bench_indexed: [ [ "BENCH_INDEXED" -> replace _loc "let%bench_fun"     ] ];
  bench_module:  [ [ "BENCH_MODULE"  -> replace _loc "let%bench_module"  ] ];

  var:  [ [ a_LIDENT ->
            print_before _loc "[@indexed ";
            print_after _loc " ="
          ] ];
  args: [ [ expr LEVEL "^" -> print_after _loc "]" ] ];

  module_expr2: [ [ module_expr ->
                    print_before _loc "(module ";
                    print_after  _loc ")"
                  ] ];

  str_item:
    [[ bench; name; "="; expr ->
       <:str_item< >>
    | bench_fun; name; "="; expr ->
       <:str_item< >>
    | bench_indexed; name; var; args; "="; expr ->
       <:str_item< >>
    | bench_module; name; "="; module_expr2 ->
      <:str_item< >>
    ]];
END

let linkme = ()
