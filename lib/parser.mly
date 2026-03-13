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
    b, not required after END
    b, allow leading/trailing/repeating SEMICOLON
  *)
stmt_list:
  | list(SEMICOLON); lst = stmt_chain; list(SEMICOLON)
    { lst }

  | list(SEMICOLON)
    { [] }

stmt_chain:
  | s = stmt; nonempty_list(SEMICOLON); rest = stmt_list
    { s :: rest}

  | s = control_block; list(SEMICOLON); rest = stmt_list
    { s :: rest }

  | s = stmt; list(SEMICOLON)
    { [s] }

  | s = control_block; list(SEMICOLON)
    { [s] }


stmt:
  | e = expr
    { Expr e }

  | i = idnt; ASSIGN; e = expr
    { match i with | (l, v) -> Assign (l, v, e) }


control_block:
  | l = LOOP; option(DO); stmts = stmt_list; END
    { Loop (l, stmts) }


expr:
  | b = binops_and_the_like
    { b }

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

binops_and_the_like:
  (* cannot be factored, because percedence *)
  (* sad... *)
  | e1 = expr; op = PLUS; e2 = expr
    { Binop (Add (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = PLUS; e2 = expr
    { Binop (Add (op, true, false), e1, e2) }
  | e1 = expr; op = PLUS; PERIOD; e2 = expr
    { Binop (Add (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = PLUS; PERIOD; e2 = expr
    { Binop (Add (op, true, true), e1, e2) }

  | e1 = expr; op = MINUS; e2 = expr
    { Binop (Sub (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = MINUS; e2 = expr
    { Binop (Sub (op, true, false), e1, e2) }
  | e1 = expr; op = MINUS; PERIOD; e2 = expr
    { Binop (Sub (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = MINUS; PERIOD; e2 = expr
    { Binop (Sub (op, true, true), e1, e2) }

  | e1 = expr; op = STAR; e2 = expr
    { Binop (Mul (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = STAR; e2 = expr
    { Binop (Mul (op, true, false), e1, e2) }
  | e1 = expr; op = STAR; PERIOD; e2 = expr
    { Binop (Mul (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = STAR; PERIOD; e2 = expr
    { Binop (Mul (op, true, true), e1, e2) }

  | e1 = expr; op = SLASH; e2 = expr
    { Binop (Div (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = SLASH; e2 = expr
    { Binop (Div (op, true, false), e1, e2) }
  | e1 = expr; op = SLASH; PERIOD; e2 = expr
    { Binop (Div (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = SLASH; PERIOD; e2 = expr
    { Binop (Div (op, true, true), e1, e2) }

  | e1 = expr; op = PERCENT; e2 = expr
    { Binop (Mod (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = PERCENT; e2 = expr
    { Binop (Mod (op, true, false), e1, e2) }
  | e1 = expr; op = PERCENT; PERIOD; e2 = expr
    { Binop (Mod (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = PERCENT; PERIOD; e2 = expr
    { Binop (Mod (op, true, true), e1, e2) }

  | e1 = expr; op = EQ; e2 = expr
    { Binop (Eq (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = EQ; e2 = expr
    { Binop (Eq (op, true, false), e1, e2) }
  | e1 = expr; op = EQ; PERIOD; e2 = expr
    { Binop (Eq (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = EQ; PERIOD; e2 = expr
    { Binop (Eq (op, true, true), e1, e2) }

  | e1 = expr; op = NEQ; e2 = expr
    { Binop (Neq (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = NEQ; e2 = expr
    { Binop (Neq (op, true, false), e1, e2) }
  | e1 = expr; op = NEQ; PERIOD; e2 = expr
    { Binop (Neq (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = NEQ; PERIOD; e2 = expr
    { Binop (Neq (op, true, true), e1, e2) }

  | e1 = expr; op = LT; e2 = expr
    { Binop (Lt (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = LT; e2 = expr
    { Binop (Lt (op, true, false), e1, e2) }
  | e1 = expr; op = LT; PERIOD; e2 = expr
    { Binop (Lt (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = LT; PERIOD; e2 = expr
    { Binop (Lt (op, true, true), e1, e2) }

  | e1 = expr; op = GT; e2 = expr
    { Binop (Gt (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = GT; e2 = expr
    { Binop (Gt (op, true, false), e1, e2) }
  | e1 = expr; op = GT; PERIOD; e2 = expr
    { Binop (Gt (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = GT; PERIOD; e2 = expr
    { Binop (Gt (op, true, true), e1, e2) }

  | e1 = expr; op = LEQ; e2 = expr
    { Binop (Leq (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = LEQ; e2 = expr
    { Binop (Leq (op, true, false), e1, e2) }
  | e1 = expr; op = LEQ; PERIOD; e2 = expr
    { Binop (Leq (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = LEQ; PERIOD; e2 = expr
    { Binop (Leq (op, true, true), e1, e2) }

  | e1 = expr; op = GEQ; e2 = expr
    { Binop (Geq (op, false, false), e1, e2) }
  | e1 = expr; PERIOD; op = GEQ; e2 = expr
    { Binop (Geq (op, true, false), e1, e2) }
  | e1 = expr; op = GEQ; PERIOD; e2 = expr
    { Binop (Geq (op, false, true), e1, e2) }
  | e1 = expr; PERIOD; op = GEQ; PERIOD; e2 = expr
    { Binop (Geq (op, true, true), e1, e2) }

  | e1 = expr; op = AND; e2 = expr
    { Binop (And op, e1, e2) }
  | e1 = expr; op = OR; e2 = expr
    { Binop (Or op, e1, e2) }
  | e1 = expr; op = XOR; e2 = expr
    { Binop (Xor op, e1, e2) }
  | e1 = expr; op = NAND; e2 = expr
    { Binop (Nand op, e1, e2) }
  | e1 = expr; op = NOR; e2 = expr
    { Binop (Nor op, e1, e2) }
  | e1 = expr; op = NXOR; e2 = expr
    { Binop (Nxor op, e1, e2) }
