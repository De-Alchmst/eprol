open Eprol_compiler_lib
open Ast
open Lexer

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
   "37; 3.7; 3.; .7; \"Hello\";",
   [
     Expr (Lit (LitInt    (dl, 37     )));
     Expr (Lit (LitFloat  (dl, 3.7    )));
     Expr (Lit (LitFloat  (dl, 3.0    )));
     Expr (Lit (LitFloat  (dl, 0.7    )));
     Expr (Lit (LitString (dl, "Hello")));
     EmptyStmt
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

