open Eprol_compiler_lib
open Ast
open Location

let dl = dummy_loc

let parse_string str =
  let lexbuf = Lexing.from_string str in
  Parser.debug_prog Lexer.tokenize_debug lexbuf


let run_test name input expected =
  try
    let actual = parse_string input in
    if actual = expected then begin
      Printf.printf "✓ %s\n" name;
      true
    end else begin
      Printf.printf "✗ %s: AST mismatch\n" name;
      false
    end
  with
  | Lexer.LexError msg ->
      Printf.printf "✗ %s: Lexer error: %s\n" name msg;
      false
  | Parser.Error ->
      Printf.printf "✗ %s: Parser error\n" name;
      false
  | e ->
      Printf.printf "✗ %s: Unexpected error: %s\n" name (Printexc.to_string e);
      false


let tests = [
  ("basic literals",
   ";37; 3.7; 3.;;; .7; \"Hello\";;;",
   [
     Expr (Lit (LitInt    (dl, 37     )));
     Expr (Lit (LitFloat  (dl, 3.7    )));
     Expr (Lit (LitFloat  (dl, 3.0    )));
     Expr (Lit (LitFloat  (dl, 0.7    )));
     Expr (Lit (LitString (dl, "Hello")));
   ]);
  ("idents",
   "foo; foo.bar; foo.bar.baz",
   [
     Expr (Var (dl, { name = "foo"; namespace = [] }));
     Expr (Var (dl, { name = "bar"; namespace = ["foo"] }));
     Expr (Var (dl, { name = "baz"; namespace = ["foo"; "bar"] }));
   ]);
  ("assignment",
   "x := 42",
   [
     Assign (dl, { name = "x"; namespace = [] }, Lit (LitInt (dl, 42)))
   ]);
  ("binops",
   "1 + 2 .* 3 -. 4 ./. 5",
   [
     Expr (Binop (Sub (dl, false, true),
                  Binop (Add (dl, false, false),
                         Lit (LitInt (dl, 1)),
                         Binop (Mul (dl, true, false),
                                Lit (LitInt (dl, 2)),
                                Lit (LitInt (dl, 3)))),
                  Binop (Div (dl, true, true),
                         Lit (LitInt (dl, 4)),
                         Lit (LitInt (dl, 5)))))
   ]);
  ("blocks and semicolons",
   "37; LOOP ;1; 4 END LOOP DO 1; LOOP END; 4 END",
   [
     Expr (Lit (LitInt (dl, 37)));
     Loop (dl, [Expr (Lit (LitInt (dl, 1)));
                Expr (Lit (LitInt (dl, 4)))]);
     Loop (dl, [Expr (Lit (LitInt (dl, 1)));
                Loop (dl, []);
                Expr (Lit (LitInt (dl, 4)))]);
   ]);
  ("while loop",
   "WHILE 1 DO 2; 3 END",
   [
     While (dl, Lit (LitInt (dl, 1)),
           [Expr (Lit (LitInt (dl, 2)));
            Expr (Lit (LitInt (dl, 3)))])
   ]);
  ("for loop",
    "FOR i FROM 0 TO 10 STEP 2 DO 1 END
     FOR i FROM 10 DOWNTO 0 DO 1 END
     FOR i FROM 0 UNTIL i >= 20 STEP 2 DO 1 END",
    [
      (For (dl, { name = "i"; namespace = [] },
            Lit (LitInt (dl, 0)),
            Binop ((Lt (dl, false, false),
                   (Var (dl, { name = "i"; namespace = [] })),
                   (Lit (LitInt (dl, 10))))),
            Lit (LitInt (dl, 2)),
            [Expr (Lit (LitInt (dl, 1)))]));

      (For (dl, { name = "i"; namespace = [] },
            Lit (LitInt (dl, 10)),
            Binop ((Gt (dl, false, false),
                   (Var (dl, { name = "i"; namespace = [] })),
                   (Lit (LitInt (dl, 0))))),
            Lit (LitInt (dl, -1)),
            [Expr (Lit (LitInt (dl, 1)))]));

      (For (dl, { name = "i"; namespace = [] },
            Lit (LitInt (dl, 0)),
            Binop ((Geq (dl, false, false),
                   (Var (dl, { name = "i"; namespace = [] })),
                   (Lit (LitInt (dl, 20))))),
            Lit (LitInt (dl, 2)),
            [Expr (Lit (LitInt (dl, 1)))]));
    ]);
  ("procedure calls",
   "foo.bar(1, 2 + 3, baz())",
   [
      Expr (Call (dl, { name = "bar"; namespace = ["foo"] },
                  [Lit (LitInt (dl, 1));
                   Binop (Add (dl, false, false),
                           Lit (LitInt (dl, 2)),
                           Lit (LitInt (dl, 3)));
                   Call (dl, { name = "baz"; namespace = [] }, [])]))
   ]);
  ("if statements",
   "IF 1 DO 2;3 END 
    IF 1 DO ELIF 2 DO 3 ELIF 4 DO 5 ELSE 6 END
    IF 1 DO 3 ELSE DO 7 END",
   [
     (If (dl, Lit (LitInt (dl, 1)),
          [Expr (Lit (LitInt (dl, 2)));
           Expr (Lit (LitInt (dl, 3)))],
          [],
          []));
     (If (dl, Lit (LitInt (dl, 1)),
          [],
          [(dl, (Lit (LitInt (dl, 2))), [Expr (Lit (LitInt (dl, 3)))]);
           (dl, (Lit (LitInt (dl, 4))), [Expr (Lit (LitInt (dl, 5)))])],
          [Expr (Lit (LitInt (dl, 6)))]));
     (If (dl, Lit (LitInt (dl, 1)),
          [Expr (Lit (LitInt (dl, 3)))],
          [],
          [Expr (Lit (LitInt (dl, 7)))]));
   ]);
  ("import statement",
   "IMPORT \"foo\" \"bar\" AS bar : foo I32",
   [
     ToplevelDbg (Import (dl, ["foo"; "bar"],
                          (dl, { name = "bar"; namespace = ["foo"] }),
                          (I32 dl)))
   ]);
  ("globals declaration",
   "VAR : foo i32 a := 3, b := 7 f64 c := 3.14 EXPORT \"PI\", END
    VAR u8 d END
    CONST : foo a = 4, b = 2 END",
   [
     (ToplevelDbg (VarDecl
                    (dl, ["foo"],
                     [((I32 dl),
                       [(dl, "a", (Some (Lit (LitInt (dl, 3)))), None);
                        (dl, "b", (Some (Lit (LitInt (dl, 7)))), None)]);
                      ((F64 dl),
                        [(dl, "c", (Some (Lit (LitFloat (dl, 3.14)))), Some "PI")])
                     ])));
     (ToplevelDbg (VarDecl
                    (dl, [],
                     [((U8 dl),
                       [(dl, "d", None, None)])])));
     (ToplevelDbg (ConstDecl
                    (dl, ["foo"],
                     (ConstType,
                      [(dl, "a", Some (Lit (LitInt (dl, 4))), None);
                       (dl, "b", Some (Lit (LitInt (dl, 2))), None)]))));
   ]);
 ("procedures",
  "PROC foo DO 1 END
   PROC foo : bar (i32 a, b, f32 c): i32 EXPORT \"ex\" DO 2 END
   PROC foo()
     i32 aa := 0, bb f64 cc := 1.0
     CONST ca = 1, cb = 3,
     VAR i64 aaa := 4,
     STATIC i32 bbb := 2
     DO 3 END
   PROC foo CONST a = 1 DO 4 END",
  [
    ToplevelDbg (Proc (dl, { name = "foo"; namespace = [] }, [], Void,
                       None, [], [Expr (Lit (LitInt (dl, 1)))]));

    ToplevelDbg (Proc (dl, { name = "foo"; namespace = ["bar"] },
                       [((I32 dl), ["a"; "b"]); ((F32 dl), ["c"])],
                       (I32 dl), Some "ex", [],
                       [Expr (Lit (LitInt (dl, 2)))]));

    ToplevelDbg (Proc (dl, { name = "foo"; namespace = [] }, [], Void,
                       None,
                       [(VarDeclType, [((I32 dl), [(dl, "aa",
                                                        Some (Lit (LitInt (dl, 0))),
                                                        None);
                                                   (dl, "bb", None, None)]);
                                       ((F64 dl), [(dl, "cc",
                                                        Some (Lit (LitFloat (dl, 1.0))),
                                                        None)])]);
                        (ConstDeclType, [(ConstType, [(dl, "ca", Some (Lit (LitInt (dl, 1))), None);
                                                      (dl, "cb", Some (Lit (LitInt (dl, 3))), None)])]);
                        (VarDeclType, [((I64 dl), [(dl, "aaa", Some (Lit (LitInt (dl, 4))), None)])]);
                        (StaticDeclType, [((I32 dl), [(dl, "bbb", Some (Lit (LitInt (dl, 2))), None)])]);
                       ],
                       [Expr (Lit (LitInt (dl, 3)))]));

    ToplevelDbg (Proc (dl, { name = "foo"; namespace = [] }, [], Void,
                       None,
                       [(ConstDeclType, [(ConstType, [(dl, "a", Some (Lit (LitInt (dl, 1))), None)])])],
                       [Expr (Lit (LitInt (dl, 4)))]));
  ]);
]


let () =
  Printf.printf "Running parser tests...\n\n";
  
  let results = List.map (fun (name, input, expected) ->
    run_test name input expected
  ) tests in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  let total = List.length tests in
  
  Printf.printf "\n%d/%d tests passed\n" passed total;
  
  if passed = total then
    exit 0
  else
    exit 1

