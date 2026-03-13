(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast
  open Location
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

%start <Ast.program> prog
%start <Ast.dbg_program> debug_prog
%%

debug_prog:
  | stmts = stmt_list; EOF
    { stmts }

  | stmts = nonempty_list(top_level_stmt); EOF
    { stmts |> List.map (fun stmt -> ToplevelDbg stmt) }

prog:
  | stmts = list(top_level_stmt); EOF
    { stmts }


top_level_stmt:
  | i = IMPORT; names = nonempty_list(STRING); AS; idnt = idnt_namespace;
    typ = import_type
    { Import (i, names |> List.map (function | (_, s) -> s), idnt, typ) }

  | v = VAR; nmsp = option(namespace); decls = decl_block_list; END
    { VarDecl (v, (match nmsp with | Some n -> n | None -> []), decls) }


(* all this weird separation to have:
    a, SEMICOLON as a statement separator
    b, not required after END
    b, allow leading/trailing/repeating SEMICOLON
*)
stmt_list:
  | list(SEMICOLON); lst = option(stmt_chain)
    { match lst with
      | Some chain -> chain
      | None -> [] }


stmt_chain:
  | s = stmt; nonempty_list(SEMICOLON); rest = stmt_chain
    { s :: rest}

  | s = control_block; list(SEMICOLON); rest = stmt_chain
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

  | r = RETURN; e = expr { Return (r, e) }
  | g = GIVE; e = expr { Give (g, e) }
  | b = BREAK { Break b }
  | n = NEXT { Next n }


control_block:
  | l = LOOP; option(DO); stmts = stmt_list; END
    { Loop (l, stmts) }

  | w = WHILE; e = expr; DO; s = stmt_list; END
    { While (w, e, s) }

  | i = IF; cond = expr; DO; body = stmt_list; elifs = list(if_elif);
    els = option(if_else); END
    { If (i, cond, body, elifs, match els with | Some els -> els | None -> []) }

  (* FOR with TO or DOWNTO *)
  | f = FOR; i = idnt; FROM; from_e = expr; upto = for_to;
    step = option(for_step); DO; stmts = stmt_list; END

    (* separate iteration variable *)
    { match i with | (il, iv) ->
      (* separate upto into comparator step, left side of exp*)
      match upto with | (cmp, to_stp, to_e) ->
      For (f, iv, from_e, (* loc, var_data, initial value *)
           (Binop (cmp, (Var (il, iv)), to_e)), (* until *)
           (* either use provided step, or TO default *)
           (match step with
              | Some step_e -> step_e
              | None -> Lit (LitInt (il, to_stp))),
           stmts)
    }

  (* FOR with UNTIL *)
  | f = FOR; i = idnt; FROM; from_e = expr; UNTIL; until_e = expr; 
    step = option(for_step); DO; stmts = stmt_list; END
   { match i with | (il, iv) ->
     For (f, iv, from_e, until_e,
          (match step with
             | Some step_e -> step_e
             | None -> Lit (LitInt (il, 1))),
          stmts)
    }


for_to:
  | TO;     e = expr { ((Lt (dummy_loc, false, false)),  1, e) }
  | DOWNTO; e = expr { ((Gt (dummy_loc, false, false)), -1, e) }

for_step:
  | STEP; e = expr { e }

if_elif:
  | e = ELIF; cond = expr; DO; body = stmt_list
    { (e, cond, body) }

if_else:
  | ELSE; option(DO); body = stmt_list
     { body }


import_type:
  | w = wtype
    { w }

wtype:
  | t = TYPE
    { match t with
      | (l, "i8")  -> I8  l | (l, "u8")  -> U8  l | (l, "s8")  -> S8  l
      | (l, "i16") -> I16 l | (l, "u16") -> U16 l | (l, "s16") -> S16 l
      | (l, "i32") -> I32 l | (l, "u32") -> U32 l | (l, "s32") -> S32 l
      | (l, "i64") -> I64 l | (l, "u64") -> U64 l | (l, "s64") -> S64 l
      | (l, "f32") -> F32 l | (l, "f64") -> F64 l
      | _ -> failwith "invalid type"
    }


decl_block_list:
  | l = nonempty_list(decl_block)
    { l }

decl_block:
  | w = wtype; l = decl_stmt_chain
    { (w, l) }

decl_stmt_chain:
      | s = decl_stmt; COMMA; rest = decl_stmt_chain
        { s :: rest }

      | s = decl_stmt; option(COMMA)
        { [s] }

decl_stmt:
  | i = IDENT; a = option(assign); ex = option(export)
    { match i with | (l, v) -> (l, v, a, ex) }

assign:
  | ASSIGN; e = expr
    { e }

export:
  | EXPORT; s = STRING
    { match s with | (_, s) -> s }


expr:
  | b = binops_and_the_like
    { b }

  | i = idnt; LROUND; args = separated_list(COMMA, expr); RROUND
    { match i with | (l, v) -> Call (l, v, args) }

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

idnt_namespace:
  | i = IDENT; n = option(namespace)
    { match i with | (l, s) ->
        match n with
        | Some n -> (l, { name = s; namespace = n })
        | None -> (l, { name = s; namespace = [] }) }

namespace:
  | COLON; i = separated_nonempty_list(PERIOD, IDENT)
    { i |> List.map (function | (_, s) -> s) }


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
