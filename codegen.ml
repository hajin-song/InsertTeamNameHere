(*
 * codegen.ml
 * Brill generator
 * Created By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu L
 * Modified By: Beaudan Campbell-Brown, Ha Jin Song, Mengyu Li
 * Last Modified: 11-MAY-2017
 *)

open Snick_ast;;
open Symbol;;
open Format;;

let indent = 4;;
let reg = ref (-1);;
let label_count = ref 0;;

let scope : string Stack.t = Stack.create ();;

let call proc =	Stack.push proc scope;;

let return () = Stack.pop scope;;

let this_scope () = Stack.top scope;;

(* ========================================================================== *)
(* Plain print formatting functions *)
(* used by generators for their brill command generation *)

(*	print_type
	* prints out type string
*)
let print_type t =
	match t with
	| Int -> sprintf "int";
	| Float -> sprintf "real";
	| Bool -> sprintf "bool";;


(* print_binop
	* prints out command for binary operator - OPERATION STRING ONLY
*)
let print_binop fmt op =
	match op with
	| Op_add -> fprintf fmt "add_";
	| Op_sub -> fprintf fmt "sub_";
	| Op_mul -> fprintf fmt "mul_";
	| Op_div -> fprintf fmt "div_";
	| Op_eq -> fprintf fmt "cmp_eq_";
	| Op_neq -> fprintf fmt "cmp_ne_";
	| Op_lt -> fprintf fmt "cmp_lt_";
	| Op_gt -> fprintf fmt "cmp_gt_";
	| Op_gteq -> fprintf fmt "cmp_ge_";
	| Op_lteq -> fprintf fmt "cmp_le_";
	| Op_and -> fprintf fmt "and";
	| Op_or -> fprintf fmt "or";;


(* print_unop
	* prints out command for unary operator - OPERATION STRING ONLY
*)
let print_unop fmt op =
	match op with
	| Op_not -> fprintf fmt "not";
	| Op_minus -> ();;


(* print_var_decl
	* print out brill command for variable storage
*)
let print_var_decl fmt stack t =
	match t with
	| Int ->	fprintf fmt "@,int_const r0, 0@,store %i, r0" stack;
	| Float ->	fprintf fmt "@,real_const r0, 0.0@,store %i, r0" stack;
	| Bool -> fprintf fmt "@,int_const r0, 0@,store %i, r0" stack;;


let rec print_arr_decl fmt t start_ptr end_ptr =
	match t with
	| Int ->
		fprintf fmt "@,int_const r0, 0%a"
		print_fill_arr (start_ptr, end_ptr);
	| Float ->
		fprintf fmt "@,real_const r0, 0.0%a"
		print_fill_arr (start_ptr, end_ptr);
	| Bool ->
		fprintf fmt "@,int_const r0, 0%a"
		print_fill_arr (start_ptr, end_ptr);

and print_fill_arr fmt (curr_ptr, end_ptr) =
	if curr_ptr <= end_ptr then
		fprintf fmt "@,store %i, r0%a"
		curr_ptr
		print_fill_arr (curr_ptr + 1, end_ptr);;


let print_arg_ref fmt ident =
	incr reg;
	match lookup_symbol (this_scope()) ident with
	| Var { var_stack = stack } ->
		fprintf fmt "@,load_address r%i, %i"
		!reg
		stack;
	| _ -> print_string "Not implemented\n"; exit 0;;


let print_binop_type fmt (t1, t2, t3) =
	match t1, t2, t3 with
	| Bool, _, Float -> fprintf fmt "real"
	| Bool, Float, _ -> fprintf fmt "real"
	| Bool, Int, Int -> fprintf fmt "int"
	| Int, _, _ -> fprintf fmt "int"
	| Float, _, _ -> fprintf fmt "real"
	| _ -> fprintf fmt "bool";;


(* print_binop_coerce
	* print out coercion command
*)
let print_binop_coerce fmt (lt, rt, lreg, rreg) =
	match lt, rt with
	| Int, Float -> fprintf fmt "@,int_to_real r%i, r%i" lreg lreg;
	| Float, Int -> fprintf fmt "@,int_to_real r%i, r%i" rreg rreg;
	| _, _ -> ();;


let print_coerce fmt ({id = id}, t, r) =
	let t2 = lookup_type id in
	if t = Float && t2 = Int then fprintf fmt "@,int_to_real r%i, r%i" r r;;

(* ========================================================================== *)


let print_bool_expr fmt value =
	incr reg;
	if value = true then
		fprintf fmt "@,int_const r%i, 1" !reg
	else
		fprintf fmt "@,int_const r%i, 0" !reg;;

let print_int_expr fmt value =
	incr reg;
	fprintf fmt "@,int_const r%i, %i" !reg value;;

let print_real_expr fmt value =
	incr reg;
	fprintf fmt "@,real_const r%i, %f" !reg value;;

let print_var_expr fmt ident =
	incr reg;
	match lookup_symbol (this_scope()) ident with
	| Var { pass_by = Value; var_stack = stack } ->
		fprintf fmt "@,load r%i, %i" !reg stack;
	| Var { pass_by = Reference; var_stack = stack } ->
		fprintf fmt "@,load r%i, %i@,load_indirect r%i, r%i"
		!reg stack !reg !reg;
	| _ -> print_string "Not implemented\n"; exit 0;;

(*
	* generate_binop
	* prints brills binary operator command
*)
let rec generate_binop fmt (id, op, { id = lid }, { id = rid }) =
	let rreg = !reg in
	decr reg;
	let lreg = !reg in
	let t = lookup_type id in
	let lt = lookup_type lid in
	let rt = lookup_type rid in
	match op with
	| Op_or ->
		fprintf fmt "@,or r%i, r%i, r%i"
		lreg lreg rreg;
	| Op_and ->
		fprintf fmt "@,and r%i, r%i, r%i"
		lreg lreg rreg;
	| _ ->
		fprintf fmt "%a@,%a%a r%i, r%i, r%i"
		print_binop_coerce (lt, rt, lreg, rreg)
		print_binop op
		print_binop_type (t, lt, rt)
		lreg lreg rreg;;

(* generate_unop
	* prints brills unary operator command
*)
let generate_unop fmt (op, { id = id }) =
	match op with
	| Op_not ->
		fprintf fmt "@,not r%i, r%i" !reg !reg
	| Op_minus ->
		let t = lookup_type id in
		fprintf fmt "@,%s_const r%i, -1@,mul_%s r%i, r%i, r%i"
		(print_type t) (!reg + 1)
		(print_type t) !reg !reg (!reg + 1);;

(* generate_expr
	* prints brill command for expressions
*)
let rec generate_expr fmt expr =
	match expr with
	| { expr = Ebool (value) } -> print_bool_expr fmt value;
	| { expr = Eint (value) } -> print_int_expr fmt value;
	| { expr = Efloat (value) } -> print_real_expr fmt value;
	| { expr = EId (ident) } ->	print_var_expr fmt ident;
	| { expr = Ebinop (lexpr, (op, _, _), rexpr); id = id } ->
		fprintf fmt "%a%a%a"
		generate_expr lexpr
		generate_expr rexpr
		generate_binop (id, op, lexpr, rexpr);
	| { expr = Eunop ((op, _, _), expr) } ->
		fprintf fmt "%a%a"
		generate_expr expr
		generate_unop (op, expr);
	| { expr = Earray (ident, exprs) } -> generate_arr_expr fmt ident exprs;

and generate_arr_expr fmt ident exprs =
	match lookup_symbol (this_scope()) ident with
	| Arr { arr_stack = stack; arr_t = t; ranges = ranges } ->
		fprintf fmt "%a@,load_indirect r%i, r%i"
		generate_arr_index (stack, t, ranges, exprs)
		(!reg + 1) (!reg + 1);
	| _ -> print_string "Not implemented\n"; exit 0;

and generate_arr_index fmt (stack, t, ranges, exprs) =
	incr reg;
	fprintf fmt "@,load_address r%i, %i%a@,sub_offset r%i, r%i, r%i"
	!reg stack
	print_arr_index (ranges, exprs)
	!reg !reg (!reg + 1);

and print_arr_index fmt (ranges, exprs) =
	match ranges, exprs with
	| (lo, hi)::rtail, e::etail ->
		fprintf fmt "%a@,int_const r%i, %i@,sub_int r%i, r%i, r%i%a@,mul_int r%i, r%i, r%i"
		generate_expr e
		(!reg + 2) lo
		(!reg + 1) (!reg + 1) (!reg + 2)
		print_arr_index (rtail, etail)
		(!reg + 1) (!reg + 1) (!reg + 2);
		decr reg;
	| _ -> fprintf fmt "@,int_const r%i, 1" (!reg + 1);;


let print_read_var fmt ident =
	match lookup_symbol (this_scope()) ident with
	| Var { var_stack = stack; var_t = var_t } ->
		fprintf fmt "@,@[<v 4># read@,call_builtin read_%s@,store %i, r0@]"
		(print_type var_t)
		stack;
	| _ -> print_string "Not implemented\n"; exit 0;;


let print_read_arr fmt ident exprs = ();;


let print_write fmt ({ id = id } as expr) =
	let t = lookup_type id in
	fprintf fmt "@,@[<v 4># write%a@,call_builtin print_%s@]"
	generate_expr expr
	(print_type t);
	decr reg;;


let print_writeS fmt str =
	let s = sprintf "string_const r0, \"%s\"@,call_builtin print_string" str in
	fprintf fmt "@,@[<v 4># write@,%s@]" s;;


let rec generate_assign fmt lvalue expr =
	fprintf fmt "@,@[<v 4># assignment%a%a"
	generate_expr expr
	generate_lvalue (lvalue, expr);

and generate_lvalue fmt (lvalue, expr) =
	match lvalue with
	| LId (ident) ->
		(match lookup_symbol (this_scope()) ident with
		| Var { pass_by = Value; var_stack = stack; var_t = t } ->
			fprintf fmt "%a@,store %i, r0@]"
			print_coerce (expr, t, !reg + 1)
			stack;
			decr reg;
		| Var { pass_by = Reference; var_stack = stack; var_t = t } ->
			fprintf fmt "%a@,load r1, %i@,store_indirect r1, r0@]"
			print_coerce (expr, t, !reg + 1)
			stack;
			decr reg;
		| _ -> print_string "Not implemented\n"; exit 0;)
	| Larray (ident, indexes) ->
		(match lookup_symbol (this_scope()) ident with
		| Arr { arr_stack = stack; arr_t = t; ranges = ranges } ->
			fprintf fmt "%a%a@,store_indirect r%i, r%i@]"
			print_coerce (expr, t, !reg + 1)
			generate_arr_index (stack, t, ranges, indexes)
			(!reg + 2) (!reg + 1);
			decr reg;
			decr reg;
		| _ -> print_string "Not implemented\n"; exit 0;);;


let rec generate_proccall fmt ident exprs =
	let { args = args } = lookup_proc ident in
	fprintf fmt "@,@[<v 4># proc call%a@,call proc_%s@]"
	generate_proccall_args (args, exprs)
	ident;
	reg := -1;

and generate_proccall_args fmt (args, exprs) =
	match args, exprs with
	| { pass_by = Value; var_t = var_t }::atail, expr::etail ->
		fprintf fmt "%a%a%a"
		generate_expr expr
		print_coerce (expr, var_t, !reg + 1)
		generate_proccall_args (atail, etail);
	| { pass_by = Reference }::atail, { expr = EId ident }::etail ->
		fprintf fmt "%a%a"
		print_arg_ref ident
		generate_proccall_args (atail, etail);
	| _ -> ();;


(* Statement Declaration
	* Recursively generates statements within the current proc
	* Each generator formats out its respective brill command
*)
let rec generate_stmts fmt stmts =
	match stmts with
	| (stmt::tail) -> generate_stmt fmt stmt; generate_stmts fmt tail;
	| [] -> ()

and generate_stmt fmt stmt =
	match stmt with
	| Read (LId ident) -> print_read_var fmt ident;
	| Read (Larray (ident, exprs)) -> print_read_arr fmt ident exprs;
	| Write expr -> print_write fmt expr;
	| WriteS str -> print_writeS fmt str;
	| Ifthen (guard, t) -> generate_ifthen fmt guard t;
	| Ifthenelse (guard, t, e) -> generate_ifthenelse fmt guard t e;
	| Assign (lvalue, expr) -> generate_assign fmt lvalue expr;
	| While (guard, stmts) -> generate_while fmt guard stmts;
	| Proccall (ident, exprs) -> generate_proccall fmt ident exprs;

and generate_ifthen fmt guard stmts = 
	let label = sprintf "label%i" !label_count in
	incr label_count;
	fprintf fmt "@,@[<v 4># if%a%a@,%s:"
	generate_guard (guard, label)
	generate_stmts stmts
	label;

and generate_ifthenelse fmt guard then_stmts else_stmts =
	let label1 = sprintf "label%i" !label_count
	and label2 = sprintf "label%i" (!label_count + 1) in
	incr label_count;
	incr label_count;
	let s = sprintf "    branch_uncond %s" label2 in
	fprintf fmt "@,@[<v 4># if%a%a@,%s@,%s:@,# else%a@,%s:"
	generate_guard (guard, label1)
	generate_stmts then_stmts
	s
	label1
	generate_stmts else_stmts
	label2;

and generate_while fmt guard stmts =
	let label1 = sprintf "label%i" !label_count
	and label2 = sprintf "label%i" (!label_count + 1) in
	incr label_count;
	incr label_count;
	fprintf fmt "@,# while@,@[<v 4>%s:%a%a@,    branch_uncond %s@,%s:"
	label1
	generate_guard (guard, label2)
	generate_stmts stmts
	label1
	label2;

and generate_guard fmt (expr, l) =
	fprintf fmt "%a@,branch_on_false r0, %s@]"
	generate_expr expr
	l;
	decr reg;;


(* Declaration generator
	* Recursively generates declaration within the current proc
	* Either Variable or Array declaration
*)
let rec generate_decls fmt decls =
	match decls with
	| (decl::tail) ->
		fprintf fmt "%a%a"
		generate_decl decl
		generate_decls tail;
	| [] -> ();

and generate_decl fmt decl =
	match decl with
	| Dvar x -> generate_var_decl fmt x;
	| Darr x -> generate_arr_decl fmt x;

and generate_var_decl fmt (t, ident) =
	match lookup_symbol (this_scope()) ident with
	| Var { var_stack = stack } -> print_var_decl fmt stack t;
	| _ -> print_string "Not implemented\n"; exit 0;

and generate_arr_decl fmt (t, ident, _) =
	match lookup_symbol (this_scope()) ident with
	| Arr { arr_stack = start_ptr; last_ptr = end_ptr} ->
		print_arr_decl fmt t start_ptr end_ptr;
	| _ -> print_string "Not implemented\n"; exit 0;;


(* Header generator
	* Generator for header of the proc
	* Recursively generates the proc's argument list
*)
let rec generate_header fmt (ident, args) =
	call ident;
	let { frame = frame } = lookup_proc ident in
	fprintf fmt "@,proc_%s:@,@[<v 4># prologue@,push_stack_frame %i%a"
	ident !frame generate_args args;
	reg := -1;

and generate_args fmt args =
	match args with
	| (Val (ident, _)::tail) ->
		fprintf fmt "@,%a%a"
		generate_arg ident
		generate_args tail;
	| (Ref (ident, _)::tail) ->
		fprintf fmt "@,%a%a"
		generate_arg ident
		generate_args tail;
	| [] -> ();

and generate_arg fmt ident =
	match lookup_symbol (this_scope()) ident with
	| Var { var_stack = stack } ->
		incr reg;
		fprintf fmt "store %i, r%i"
		stack
		!reg;
	| _ -> print_string "Not implemented\n"; exit 0;;


let rec generate_procs fmt prog =
	match prog with
	| { header = (proc, args) as header; decls = decls; stmts = stmts } :: tail ->
		reg := -1;
		fprintf fmt "%a%a@]%a%a%a"
		generate_header header
		generate_decls decls
		generate_stmts stmts
		generate_epilogue proc
		generate_procs tail;
	| [] -> ();

and generate_epilogue fmt proc =
	let { frame = frame } = lookup_proc proc in
	fprintf fmt "@,@[<v 4># epilogue@,pop_stack_frame %i@,return@,@]" !frame;;


let generate prog =
	let fmt = Format.std_formatter in
	fprintf fmt "@[<v>@[<v 4>    call proc_main@,halt@]%a@,@]"
	generate_procs prog;;
