open Snack_ast
open Format

let print_bean beantype =
 match beantype with
  | Bool -> print_string "bool"
  | Int -> print_string "int"
  | Float -> print_string "float";;

let print_typedef (ident, beantype) = print_bean beantype; print_string ident;;

let print_arg fmt arg  =
 match arg with
  | Val arg -> print_typedef arg
  | Ref arg -> print_typedef arg;;

let print_args fmt args = print_string "(" ; List.iter (print_arg fmt) args; print_string ")\n";;
let print_ident fmt ident = fprintf fmt "%s" ident;;

let print_proc_stmts fmt stmts = open_hovbox 2; print_string "stmts\n" ; close_box();;
let print_proc_decls fmt decls = open_hovbox 2; print_string "decls\n" ; close_box();;

let print_proc_header fmt (ident, args) = open_hovbox 1; print_string "proc "; print_ident fmt ident; print_args fmt args; close_box();;

let print_proc fmt proc =
 open_hovbox 1; print_proc_header fmt proc.header; print_proc_decls fmt proc.decls; print_proc_stmts fmt proc.stmts; close_box();;

let print_procs fmt prog = List.iter (print_proc fmt) prog;;

let rec print_program fmt prog = open_hovbox 1; print_procs fmt prog; close_box();;
