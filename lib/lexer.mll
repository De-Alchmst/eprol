{
  open Parser
  open Location
  exception LexError of string

  let in_debug = ref false
  let dummy_loc = { file = "<unknown>"; line = 0; column = 0 }

  let mk_loc lexbuf =
    if not !in_debug then
      let s = Lexing.lexeme_start_p lexbuf in
      {
        file   = s.pos_fname;
        line   = s.pos_lnum;
        column = s.pos_cnum - s.pos_bol;
      }
    else
      dummy_loc

  (* another block after tokenize *)
}


let whitespace = [' ' '\t' '\r']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_'] (* TODO: allow unicode in IDENT *)

rule tokenize = parse
  | whitespace+  { tokenize lexbuf }
  | '\n'         { Lexing.new_line lexbuf; tokenize lexbuf }

  | "PROC"     { PROC (mk_loc lexbuf) }
  | "DO"       { DO (mk_loc lexbuf) }
  | "END"      { END (mk_loc lexbuf) }
  | "IF"       { IF (mk_loc lexbuf) }
  | "ELIF"     { ELIF (mk_loc lexbuf) }
  | "ELSE"     { ELSE (mk_loc lexbuf) }
  | "WHILE"    { WHILE (mk_loc lexbuf) }
  | "LOOP"     { LOOP (mk_loc lexbuf) }
  | "FOR"      { FOR (mk_loc lexbuf) }
  | "FROM"     { FROM (mk_loc lexbuf) }
  | "TO"       { TO (mk_loc lexbuf) }
  | "DOWNTO"   { DOWNTO (mk_loc lexbuf) }
  | "UNTIL"    { UNTIL (mk_loc lexbuf) }
  | "STEP"     { STEP (mk_loc lexbuf) }
  | "CASE"     { CASE (mk_loc lexbuf) }
  | "OF"       { OF (mk_loc lexbuf) }
  | "IN"       { IN (mk_loc lexbuf) }
  | "RETURN"   { RETURN (mk_loc lexbuf) }
  | "BREAK"    { BREAK (mk_loc lexbuf) }
  | "NEXT"     { NEXT (mk_loc lexbuf) }
  | "GIVE"     { GIVE (mk_loc lexbuf) }
  | "VAR"      { VAR (mk_loc lexbuf) }
  | "CONST"    { CONST (mk_loc lexbuf) }
  | "STATIC"   { STATIC (mk_loc lexbuf) }
  | "ACCESSOR" { ACCESSOR (mk_loc lexbuf) }
  | "ENUM"     { ENUM (mk_loc lexbuf) }
  | "RECORD"   { RECORD (mk_loc lexbuf) }
  | "EXTENDS"  { EXTENDS (mk_loc lexbuf) }
  | "EXPORT"   { EXPORT (mk_loc lexbuf) }
  | "USE"      { USE (mk_loc lexbuf) }
  | "IMPORT"   { IMPORT (mk_loc lexbuf) }
  | "AS"       { AS (mk_loc lexbuf) }
  | "NOT"      { NOT (mk_loc lexbuf) }
  | "BOOL"     { BOOL (mk_loc lexbuf) }
  | "NBOOL"    { NBOOL (mk_loc lexbuf) }
  | "AND"      { AND (mk_loc lexbuf) }
  | "OR"       { OR (mk_loc lexbuf) }
  | "XOR"      { XOR (mk_loc lexbuf) }
  | "NAND"     { NAND (mk_loc lexbuf) }
  | "NOR"      { NOR (mk_loc lexbuf) }
  | "NXOR"     { NXOR (mk_loc lexbuf) }
  | "TRUE"     { TRUE (mk_loc lexbuf) }
  | "FALSE"    { FALSE (mk_loc lexbuf) }

  | (['i' 'I' 'f' 'F' 'u' 'U' 's' 'S']("8"|"16"|"32"|"64")) as t
    { TYPE (mk_loc lexbuf, String.lowercase_ascii t) }

  | digit+ as n                       { INT (mk_loc lexbuf, int_of_string n) }
  | (letter ( letter | digit )*) as i { IDENT (mk_loc lexbuf, i) }
  (* all 3.7, 3. and .7 are valid floats *)
  | ((digit+ '.' digit*) | ('.' digit+)) as f
    { FLOAT (mk_loc lexbuf, float_of_string f) }

  (* strip quotes from match *)
  | ('"' ([^'"'] | "\\\"" )* '"') as str
      { STRING (mk_loc lexbuf, String.sub str 1 ((String.length str) - 2)) }

  | ';'  { SEMICOLON (mk_loc lexbuf) }
  | '.'  { PERIOD (mk_loc lexbuf) }
  | ','  { COMMA (mk_loc lexbuf) }
  | ':'  { COLON (mk_loc lexbuf) }
  | '|'  { PIPE (mk_loc lexbuf) }
  | '@'  { AT (mk_loc lexbuf) }
  | '^'  { CARET (mk_loc lexbuf) }
  | '('  { LROUND (mk_loc lexbuf) }
  | ')'  { RROUND (mk_loc lexbuf) }
  | '{'  { LCURLY (mk_loc lexbuf) }
  | '}'  { RCURLY (mk_loc lexbuf) }
  | '['  { LSQUARE (mk_loc lexbuf) }
  | ']'  { RSQUARE (mk_loc lexbuf) }
  | '+'  { PLUS (mk_loc lexbuf) }
  | '-'  { MINUS (mk_loc lexbuf) }
  | '*'  { STAR (mk_loc lexbuf) }
  | '/'  { SLASH (mk_loc lexbuf) }
  | '%'  { PERCENT (mk_loc lexbuf) }
  | '='  { EQ (mk_loc lexbuf) }
  | "<>" { NEQ (mk_loc lexbuf) }
  | '<'  { LT (mk_loc lexbuf) }
  | '>'  { GT (mk_loc lexbuf) }
  | "<=" { LEQ (mk_loc lexbuf) }
  | ">=" { GEQ (mk_loc lexbuf) }
  | ":=" { ASSIGN (mk_loc lexbuf) }

  | eof { EOF (mk_loc lexbuf) }

  | _ as c {
    if not !in_debug then
      begin
        report_error (mk_loc lexbuf)
                     (Printf.sprintf "Unexpected character: '%c'" c);
        tokenize lexbuf (* skip unexpected character *)
      end
    else
      raise (LexError (Printf.sprintf "Unexpected character: %c" c))
  }


{
  let tokenize_debug lexbuf =
    in_debug := true;
    tokenize lexbuf
}
