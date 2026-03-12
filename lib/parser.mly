(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast
%}

%token <Location.loc * string> IDENT
%token <Location.loc * int> INT
%token <Location.loc * float> FLOAT
%token <Location.loc * string> STRING TYPE
%token <Location.loc> LROUND RROUND LCURLY RCURLY LSQUARE RSQUARE
%token <Location.loc> PROC DO END IF ELIF ELSE WHILE LOOP FOR FROM TO DOWNTO
%token <Location.loc> UNTIL STEP CASE OF IN RETURN BREAK NEXT GIVE
%token <Location.loc> VAR CONST STATIC
%token <Location.loc> ACCESSOR ENUM RECORD EXTENDS EXPORT USE IMPORT AS
%token <Location.loc> NOT BOOL NBOOL
%token <Location.loc> AND OR XOR NAND NOR NXOR
%token <Location.loc> TRUE FALSE
%token <Location.loc> SEMICOLON PERIOD COMMA COLON PIPE AT CARET
%token <Location.loc> PLUS MINUS STAR SLASH PERCENT
%token <Location.loc> EQ NEQ LT GT LEQ GEQ
%token <Location.loc> ASSIGN
%token <Location.loc> EOF

%left AND OR XOR NAND NOR NXOR
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left STAR SLASH PERCENT

(* %start <Ast.program> prog *)
%start <Ast.dbg_program> debug_prog
%%

debug_prog:
  | stmts = stmt_list; EOF
    { stmts }

(* prog: *)
(*   | stmts = list(top_level_stmt); EOF *)
(*     { stmts } *)


(* all this weird separation to have:
    a, SEMICOLON as a statement separator
    b, not require it after END
    b, allow trailing/repeating SEMICOLON
  *)
stmt_list:
  | first = stmt; lst = list(separated_stmt)
    { first :: lst }

separated_stmt:
  | nonempty_list(SEMICOLON); s = stmt
    { s }
  | nonempty_list(SEMICOLON)
    { EmptyStmt }


stmt:
  | e = expr
    { Expr e }

  | i = idnt; ASSIGN; e = expr
    { match i with | (l, v) -> Assign (l, v, e) }


expr:
  | i = INT
    { match i with | (l, v) -> Lit (LitInt (l, v)) }
  | f = FLOAT
    { match f with | (l, v) -> Lit (LitFloat (l, v)) }
  | s = STRING
    { match s with | (l, v) -> Lit (LitString (l, v)) }
  | i = idnt
    { match i with | (l, v) -> Var (l, v) }

idnt:
  (* accumulate strings of leading idents until the last one is the base *)
  | names = separated_nonempty_list(PERIOD, IDENT)
    { let rec aux acc = function
        | [] -> failwith "impossible"
        | [i] -> (match i with | (l, s) -> (l, { name = s;
                                                 namespace = (List.rev acc) }))
        | i :: rest -> aux ((match i with | (_, s) -> s) :: acc) rest
      in aux [] names }
