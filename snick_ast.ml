(*
 * snick_ast.ml
 * AST for SNACK language
 * Skeleton provided as part of assignment
 * Modified By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu Li
 * Last Modified: 08-APR-2017
 *)

(* Specification of an AST for bean *)
type ident = string

(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | Float

type typedef = (ident * beantype)

(* Operators and Precedents *)
type binop_type =
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

type unop_type =
  | Op_minus
  | Op_not

type op_prec =
  | Prec_or
  | Prec_and
  | Prec_not
  | Prec_eq
  | Prec_addsub
  | Prec_muldiv
  | Prec_uminus

type op_assoc =
  | Left_assoc
  | Right_assoc
  | Non_assoc

type binop = (binop_type * op_prec * op_assoc)

type unop = (unop_type * op_prec * op_assoc)

(* Procedure Argument is either Val or Ref of a variable*)
type argument =
  | Val of typedef
  | Ref of typedef

(* A Procedure Header is made of its identifier and its signature*)
type header = (ident * argument list)

(* An expression is a primative type or operation on primative types *)
type expr =
  | Ebool of (bool * int)
  | Eint of (int * int)
  | Efloat of (float * int)
  | EId of (ident * int)
  | Ebinop of (expr * binop * expr * int)
  | Eunop of (unop * expr * int)
  | Earray of (ident * expr list * int)

(* Left Hand Side of the assignment is either an identifier or array access *)
type lvalue =
  | LId of ident
  | Larray of (ident * expr list)

type range = (int * int)

(* Variable declaration can be a primative type or array of primative type *)
type decl =
  | Dvar of (beantype * ident)
  | Darr of (beantype * ident * range list)

(* Statements allowed in the SNACK language *)
(* Separate token for If-Else and If-Then-Else
 * (Most simple solution encountered)
 *)
type stmt =
  | Assign of (lvalue * expr)
  | Read of lvalue
  | Write of expr
  | WriteS of string
  | Ifthen of (expr * stmt list)
  | Ifthenelse of (expr * stmt list * stmt list)
  | While of (expr * stmt list)
  | Proccall of (ident * expr list)

(* Program is list of Procedures *)
type proc = {
  header : header ;
  decls : decl list ;
  stmts : stmt list
}

type program = proc list

type t = program
