(* Specification of an AST for bean *)
type ident = string

(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | Float

type typedef = (ident * beantype)

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

type unop =
  | Op_minus
  | Op_not

type paren =
  | L_paren
  | R_paren

type argument =
  | Val of typedef
  | Ref of typedef

type header = (ident * argument list)

type expr =
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | EId of ident
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Earray of (ident * expr list)

type lvalue =
  | LId of ident
  | Larray of (ident * expr list)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type range = (int * int)

type decl =
  | Dvar of (beantype * ident)
  | Darr of (beantype * ident * range list)

type stmt =
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | WriteS of string
  | Ifthen of (expr * stmt list)
  | Ifthenelse of (expr * stmt list * stmt list)
  | While of (expr * stmt list)
  | Proccall of (ident * expr list)

type proc = {
  header : header ;
  decls : decl list ;
  stmts : stmt list
}

type program = proc list

type t = program
