(* Test syntax analyser *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let tree = Absyntree.flat_nodes (Absyntree.tree_of_lfundecs ast) in
    let box = Tree.box_of_tree tree in
    Format.printf "%s\n\n%!" (Box.string_of_box box);
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test _ =
  (* function declaration and constant expression *)
  check "int f(int x) = 100";
  [%expect{|
                 ╭───────╮
                 │Program│
                 ╰────┬──╯
                   ╭──┴╮
                   │Fun│
                   ╰──┬╯
         ╭───────────┬┴───────────╮
    ╭────┴────╮  ╭───┴───╮  ╭─────┴────╮
    │    f    │  │Formals│  │IntExp 100│
    │Absyn.Int│  ╰───┬───╯  ╰──────────╯
    ╰─────────╯ ╭────┴────╮
                │    x    │
                │Absyn.Int│
                ╰─────────╯ |}];

  check "int f(int x, int y, bool z) = 100";
  [%expect{|
                              ╭───────╮
                              │Program│
                              ╰───┬───╯
                                ╭─┴─╮
                                │Fun│
                                ╰─┬─╯
         ╭────────────────────────┴────────────────────────╮
    ╭────┴────╮              ╭────┴──╮               ╭─────┴────╮
    │    f    │              │Formals│               │IntExp 100│
    │Absyn.Int│              ╰────┬──╯               ╰──────────╯
    ╰─────────╯      ╭───────────┬┴───────────╮
                ╭────┴────╮ ╭────┴────╮ ╭─────┴────╮
                │    x    │ │    y    │ │    z     │
                │Absyn.Int│ │Absyn.Int│ │Absyn.Bool│
                ╰─────────╯ ╰─────────╯ ╰──────────╯ |}];

  check "int f() = 100";
  [%expect{| :1.7 error: syntax |}];

  check "foo f(int x) = 100";
  [%expect{| :1.3 error: syntax |}];

  (* binary operators *)
  check "bool f(int x) = 2 + 3 + 4 < 5 + 6";
  [%expect{|
                                       ╭───────╮
                                       │Program│
                                       ╰───┬───╯
                                         ╭─┴─╮
                                         │Fun│
                                         ╰─┬─╯
          ╭───────────┬────────────────────┴────────────╮
    ╭─────┴────╮  ╭───┴───╮                        ╭────┴──╮
    │    f     │  │Formals│                        │OpExp <│
    │Absyn.Bool│  ╰───┬───╯                        ╰────┬──╯
    ╰──────────╯ ╭────┴────╮                 ╭──────────┴───────────────╮
                 │    x    │            ╭────┴──╮                   ╭───┴───╮
                 │Absyn.Int│            │OpExp +│                   │OpExp +│
                 ╰─────────╯            ╰────┬──╯                   ╰───┬───╯
                                       ╭─────┴──────────╮          ╭────┴─────╮
                                   ╭───┴───╮       ╭────┴───╮ ╭────┴───╮ ╭────┴───╮
                                   │OpExp +│       │IntExp 4│ │IntExp 5│ │IntExp 6│
                                   ╰───┬───╯       ╰────────╯ ╰────────╯ ╰────────╯
                                  ╭────┴─────╮
                             ╭────┴───╮ ╭────┴───╮
                             │IntExp 2│ │IntExp 3│
                             ╰────────╯ ╰────────╯ |}];

  check "bool f(int x) = 2 < 3 < 4";
  [%expect{| :1.23 error: syntax |}];

  check "int g(int x, bool y) = 6 < 7 + 1";
  [%expect{|
                                  ╭───────╮
                                  │Program│
                                  ╰───┬───╯
                                    ╭─┴─╮
                                    │Fun│
                                    ╰─┬─╯
         ╭──────────────────┬─────────┴──────────────────╮
    ╭────┴────╮        ╭────┴──╮                    ╭────┴──╮
    │    g    │        │Formals│                    │OpExp <│
    │Absyn.Int│        ╰────┬──╯                    ╰────┬──╯
    ╰─────────╯      ╭──────┴─────╮           ╭──────────┴────╮
                ╭────┴────╮ ╭─────┴────╮ ╭────┴───╮       ╭───┴───╮
                │    x    │ │    y     │ │IntExp 6│       │OpExp +│
                │Absyn.Int│ │Absyn.Bool│ ╰────────╯       ╰───┬───╯
                ╰─────────╯ ╰──────────╯                 ╭────┴─────╮
                                                    ╭────┴───╮ ╭────┴───╮
                                                    │IntExp 7│ │IntExp 1│
                                                    ╰────────╯ ╰────────╯ |}];

  check "bool f(int x) = x + y";
  [%expect{|
                     ╭───────╮
                     │Program│
                     ╰────┬──╯
                       ╭──┴╮
                       │Fun│
                       ╰──┬╯
          ╭───────────┬───┴───────────╮
    ╭─────┴────╮  ╭───┴───╮       ╭───┴───╮
    │    f     │  │Formals│       │OpExp +│
    │Absyn.Bool│  ╰───┬───╯       ╰───┬───╯
    ╰──────────╯ ╭────┴────╮     ╭────┴────╮
                 │    x    │ ╭───┴───╮ ╭───┴───╮
                 │Absyn.Int│ │IdExp x│ │IdExp y│
                 ╰─────────╯ ╰───────╯ ╰───────╯ |}];

  check "int f(int x, int y) = if x then y else xy";
  [%expect{|
                                ╭───────╮
                                │Program│
                                ╰────┬──╯
                                  ╭──┴╮
                                  │Fun│
                                  ╰──┬╯
         ╭─────────────────┬─────────┴─────────────────╮
    ╭────┴────╮        ╭───┴───╮             ╭─────────┴───────╮
    │    f    │        │Formals│             │ConditionalExp if│
    │Absyn.Int│        ╰───┬───╯             ╰─────────┬───────╯
    ╰─────────╯      ╭─────┴─────╮          ╭─────────┬┴─────────╮
                ╭────┴────╮ ╭────┴────╮ ╭───┴───╮ ╭───┴───╮ ╭────┴───╮
                │    x    │ │    y    │ │IdExp x│ │IdExp y│ │IdExp xy│
                │Absyn.Int│ │Absyn.Int│ ╰───────╯ ╰───────╯ ╰────────╯
                ╰─────────╯ ╰─────────╯ |}];

  check "int f(int x) = y";
  [%expect{|
                ╭───────╮
                │Program│
                ╰───┬───╯
                  ╭─┴─╮
                  │Fun│
                  ╰─┬─╯
         ╭──────────┴┬──────────╮
    ╭────┴────╮  ╭───┴───╮  ╭───┴───╮
    │    f    │  │Formals│  │IdExp y│
    │Absyn.Int│  ╰───┬───╯  ╰───────╯
    ╰─────────╯ ╭────┴────╮
                │    x    │
                │Absyn.Int│
                ╰─────────╯ |}];

  check "int f(int x) = int = y";
  [%expect{| :1.18 error: syntax |}];

  check "int f(int x) = let const = x in const = y";
  [%expect{| :1.39 error: syntax |}];

  check "int f(int x) = let var = x in var + y";
  [%expect{|
                           ╭───────╮
                           │Program│
                           ╰───┬───╯
                             ╭─┴─╮
                             │Fun│
                             ╰─┬─╯
         ╭───────────┬─────────┴───────────╮
    ╭────┴────╮  ╭───┴───╮       ╭─────────┴────────╮
    │    f    │  │Formals│       │DeclarationExp var│
    │Absyn.Int│  ╰───┬───╯       ╰─────────┬────────╯
    ╰─────────╯ ╭────┴────╮     ╭──────────┴────╮
                │    x    │ ╭───┴───╮       ╭───┴───╮
                │Absyn.Int│ │IdExp x│       │OpExp +│
                ╰─────────╯ ╰───────╯       ╰───┬───╯
                                           ╭────┴─────╮
                                      ╭────┴────╮ ╭───┴───╮
                                      │IdExp var│ │IdExp y│
                                      ╰─────────╯ ╰───────╯ |}];

  check "bool f(int x) = x (a + b + c)";
  [%expect{|
                          ╭───────╮
                          │Program│
                          ╰────┬──╯
                            ╭──┴╮
                            │Fun│
                            ╰──┬╯
          ╭───────────┬────────┴───────────╮
    ╭─────┴────╮  ╭───┴───╮         ╭──────┴──────╮
    │    f     │  │Formals│         │FunctionExp x│
    │Absyn.Bool│  ╰───┬───╯         ╰──────┬──────╯
    ╰──────────╯ ╭────┴────╮           ╭───┴───╮
                 │    x    │           │OpExp +│
                 │Absyn.Int│           ╰───┬───╯
                 ╰─────────╯          ╭────┴─────────╮
                                  ╭───┴───╮      ╭───┴───╮
                                  │OpExp +│      │IdExp c│
                                  ╰───┬───╯      ╰───────╯
                                 ╭────┴────╮
                             ╭───┴───╮ ╭───┴───╮
                             │IdExp a│ │IdExp b│
                             ╰───────╯ ╰───────╯ |}];

  check "int f(int x, int y) = x(if x < 2 then x + 2 else x) + y(if y < 3 then y else y + 3)";
  [%expect{|
                                                                     ╭───────╮
                                                                     │Program│
                                                                     ╰───┬───╯
                                                                       ╭─┴─╮
                                                                       │Fun│
                                                                       ╰─┬─╯
         ╭─────────────────┬─────────────────────────────────────────────┴─────────────────╮
    ╭────┴────╮        ╭───┴───╮                                                       ╭───┴───╮
    │    f    │        │Formals│                                                       │OpExp +│
    │Absyn.Int│        ╰───┬───╯                                                       ╰───┬───╯
    ╰─────────╯      ╭─────┴─────╮                               ╭─────────────────────────┴─────────────────────────╮
                ╭────┴────╮ ╭────┴────╮                   ╭──────┴──────╮                                     ╭──────┴──────╮
                │    x    │ │    y    │                   │FunctionExp x│                                     │FunctionExp y│
                │Absyn.Int│ │Absyn.Int│                   ╰──────┬──────╯                                     ╰──────┬──────╯
                ╰─────────╯ ╰─────────╯                 ╭────────┴────────╮                                 ╭────────┴────────╮
                                                        │ConditionalExp if│                                 │ConditionalExp if│
                                                        ╰────────┬────────╯                                 ╰────────┬────────╯
                                                  ╭──────────────┴─────┬──────────────╮               ╭──────────────┴───────────────╮
                                             ╭────┴──╮            ╭────┴──╮       ╭───┴───╮      ╭────┴──╮       ╭───┴───╮      ╭────┴──╮
                                             │OpExp <│            │OpExp +│       │IdExp x│      │OpExp <│       │IdExp y│      │OpExp +│
                                             ╰────┬──╯            ╰────┬──╯       ╰───────╯      ╰────┬──╯       ╰───────╯      ╰────┬──╯
                                            ╭─────┴────╮         ╭─────┴────╮                   ╭─────┴────╮                   ╭─────┴────╮
                                        ╭───┴───╮ ╭────┴───╮ ╭───┴───╮ ╭────┴───╮           ╭───┴───╮ ╭────┴───╮           ╭───┴───╮ ╭────┴───╮
                                        │IdExp x│ │IntExp 2│ │IdExp x│ │IntExp 2│           │IdExp y│ │IntExp 3│           │IdExp y│ │IntExp 3│
                                        ╰───────╯ ╰────────╯ ╰───────╯ ╰────────╯           ╰───────╯ ╰────────╯           ╰───────╯ ╰────────╯ |}];