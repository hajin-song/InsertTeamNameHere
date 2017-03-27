(* Specification of an AST for bean *)
type ident = string

(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | Float

type typedef = (ident * beantype)

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type binop =
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_eq
  | Op_neq
  | Op_lt
  | Op_gt
  | Op_gteq
  | Op_lteq
  | Op_or
  | Op_and
  | Op_not

type unop =
  | Op_minus

type expr =
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * beantype)

type stmt =
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Ifthen of (expr * stmt list)
  | Ifthenelse of (expr * stmt list * stmt list)
  | While of (expr * stmt list)

type program = {
  decls : typedef list ;
  stmts : stmt list
}

type t = program
