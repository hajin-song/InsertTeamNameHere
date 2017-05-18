open Snick_ast;;

type pass_by =
  | Value
  | Reference

type variable = {
  pass_by : pass_by ;
  var_ident : string ;
  var_t : beantype ;
  var_stack : int
}

type proc_t = {
  proc_ident : string ;
  args : variable list ;
  frame : int ref
}

type array = {
  arr_ident : string ;
  arr_t : beantype ;
  ranges : range list ;
  arr_stack : int ;
  last_ptr : int
}

type symbol_t =
  | Var of variable
  | Arr of array

type table_types =
  | Proc of proc_t
  | Sym of symbol_t

type proc_tbl_t = (string, proc_t) Hashtbl.t

type sym_tbl_t = (string, symbol_t) Hashtbl.t

type scope_tbl_t = (string, sym_tbl_t) Hashtbl.t

type type_tbl_t = (int, beantype) Hashtbl.t

let scopes : scope_tbl_t = Hashtbl.create 100;;
let procs : proc_tbl_t = Hashtbl.create 100;;
let types : type_tbl_t = Hashtbl.create 100;;

let insert_type id t =
  if Hashtbl.mem types id then
    (print_string "Duplicate attribute\n"; exit 0)
  else
    Hashtbl.add types id t;;

let lookup_type id =
  if Hashtbl.mem types id then
    (* Return symbol if is in current scope *)
    Hashtbl.find types id
  else
    (print_string "Error looking up attribute\n"; exit 0;);;

let update_type id t =
  if Hashtbl.mem types id then
    Hashtbl.replace types id t
  else
    (print_string "Error updating attribute\n"; exit 0;);;

let proc_scope proc =
  if Hashtbl.mem scopes proc then
    Hashtbl.find scopes proc
  else (
    let newProc : sym_tbl_t = Hashtbl.create 100 in
    Hashtbl.add scopes proc newProc;
    newProc);;

let insert_proc proc =
  if Hashtbl.mem procs proc.proc_ident then
    (print_string "Duplicate procedure\n"; exit 0;)
  else
    Hashtbl.add procs proc.proc_ident proc;;

let lookup_proc ident =
  (* If not, check if it is a procedure identifier *)
  if Hashtbl.mem procs ident then
    Hashtbl.find procs ident
  else
    (print_string "Uninitialised identifier used\n"; exit 0);;

let insert_symbol proc ident symbol =
  let procScope = proc_scope proc in
  if Hashtbl.mem procScope ident then
    (print_string "Duplicate variable\n"; exit 0;)
  else (
    Hashtbl.add procScope ident symbol;
  );;

let new_ptr ident =
  let proc = Hashtbl.find procs ident in
  incr proc.frame;
  !(proc.frame) - 1;;

let new_arr_ptr ident size =
  let proc = Hashtbl.find procs ident in
  let ptr = !(proc.frame) in
  proc.frame := ptr + size;
  ptr;;

let lookup_symbol proc ident =
  (* Get the current proc's scope *)
  let procScope = proc_scope proc in
  if Hashtbl.mem procScope ident then
    (* Return symbol if is in current scope *)
    Hashtbl.find procScope ident
  else (print_string "Uninitialised identifier used\n"; exit 0);;
