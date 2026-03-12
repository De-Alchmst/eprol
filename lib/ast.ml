open Location

type var_data = {
  name: string;
  namespace: string list;
}

and binop =
  (* unsigned_left?, unsigned_right? *)
  | Add  of loc * bool * bool
  | Sub  of loc * bool * bool
  | Mul  of loc * bool * bool
  | Div  of loc * bool * bool
  | Mod  of loc * bool * bool
  | Eq   of loc * bool * bool
  | Neq  of loc * bool * bool
  | Lt   of loc * bool * bool
  | Gt   of loc * bool * bool
  | Leq  of loc * bool * bool
  | Geq  of loc * bool * bool
  | And  of loc
  | Or   of loc
  | Xor  of loc
  | Nand of loc
  | Nor  of loc
  | Nxor of loc

and unop =
  | Neg   of loc (* unary minus *)
  | Not   of loc
  | Bool  of loc
  | Nbool of loc

and ttype =
  | Void
  | I8  of loc | U8  of loc | S8  of loc
  | I16 of loc | U16 of loc | S16 of loc
  | I32 of loc | U32 of loc | S32 of loc
  | I64 of loc | U64 of loc | S64 of loc
  | F32 of loc | F64 of loc

and literal =
  | LitInt    of loc * int
  | LitFloat  of loc * float
  | LitString of loc * string

and expr =
  | Var    of loc * var_data
  | Lit    of literal
  | Binop  of binop * expr * expr
  | Unop   of unop * expr
  | Call   of loc * var_data * expr list
  (* ttype, type, if, body, [elif, body], [body] *)
  | If     of loc * ttype * expr * stmt list * (loc * expr * stmt list) list * stmt list
  (* type, case, [[of,,,], body]*)
  | Case   of loc * ttype * expr * (expr list * stmt list) list
  | Block  of loc * stmt list
  (* addr, index, type, in_bytes? *)
  | Access of loc * expr * expr * ttype * bool

and stmt =
  | EmptyStmt
  | Expr   of expr
  | Assign of loc * var_data * expr
  | Loop   of loc * stmt list
  | While  of loc * expr * stmt list
  (* var, from, until, step , body*)
  | For    of loc * var_data * expr * expr * expr * stmt list
  (* var, in, of *)
  | ForOf  of loc * var_data * expr * ttype * stmt list
  (* if, body, [elif, body], [body] *)
  | If     of loc * expr * stmt list * (loc * expr * stmt list) list * stmt list
  (* case, [[of,,,], body]*)
  | Case   of loc * expr * (expr list * stmt list) list
  | Return of loc * expr option
  | Give   of loc * expr option
  | Break  of loc
  | Next   of loc

and top_level_stmt =
  (* name/namespace, args, returnType, locals, body *)
  | Proc of loc * var_data * var_data list * ttype * (loc * var_data) list * stmt list
  | GlobalVar of loc * (loc * var_data * expr) list
  | Import of string list * var_data

and program = top_level_stmt list
and dbg_program = stmt list
