open Camlp4.PreCast
open Syntax
open Camlp4_to_ppx

EXTEND Gram
  GLOBAL: str_item;

  name_equal: [ [ `STRING _; "=" -> ()
                |            "=" -> replace _loc "_ ="
                ] ];

  test:        [ [ "TEST"        -> replace _loc "let%test"        ] ];
  test_unit:   [ [ "TEST_UNIT"   -> replace _loc "let%test_unit"   ] ];
  test_module: [ [ "TEST_MODULE" -> replace _loc "let%test_module" ] ];

  module_expr2: [ [ module_expr ->
                    print_before _loc "(module ";
                    print_after  _loc ")"
                  ] ];

  expr_skip_trailing_semi : [[ expr -> skip_trailing_semi _loc ]];

  str_item:
    [[ test;      name_equal; expr_skip_trailing_semi -> <:str_item< >>
     | test_unit; name_equal; expr_skip_trailing_semi -> <:str_item< >>
     | test_module; name_equal; module_expr2          -> <:str_item< >>
    ]];
END

let linkme = ()
