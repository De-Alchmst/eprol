(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast
  open Location
%}

%token <location * string> IDENT
%token <location * int> INT
%token <location * float> FLOAT
%token <location * string> STRING TYPE
%token <location> SEMICOLON PERIOD COMMA COLON PIPE AT CARET
%token <location> LROUND RROUND LCURLY RCURLY LSQUARE RSQUARE
%token <location> PROC DO END IF ELIF ELSE WHILE LOOP FOR FROM TO DOWNTO UNTIL
%token <location> STEP CASE OF IN RETURN BREAK NEXT GIVE
%token <location> VAR CONST ACCESSOR RECORD EXTENDS EXPORT IMPORT AS
%token <location> NOT BOOL NBOOL
%token <location> ASSIGN
%token <location> TRUE FALSE
%token <location> AND OR XOR NAND NOR NXOR
%token <location> EQ NEQ LT GT LEQ GEQ
%token <location> PLUS MINUS STAR SLASH PERCENT
%token <location> EOF

%left AND OR XOR NAND NOR NXOR
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left STAR SLASH PERCENT

(* %start <Ast.program> prog *)
(* %start <Ast.program> prog_debug *)
%%

