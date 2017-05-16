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
let label = ref 0;;

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
let print_type fmt t =
	match t with
	| Int -> fprintf fmt "int";
	| Float -> fprintf fmt "real";
	| Bool -> fprintf fmt "bool";;

(* print_coercion
	* print out coercion command
*)
let print_coercion fmt (lt, rt, lreg, rreg) =
	match lt, rt with
	| Int, Float -> fprintf fmt "@,int_to_real r%i, r%i" lreg lreg;
	| Float, Int -> fprintf fmt "@,int_to_real r%i, r%i" rreg rreg;
	| _, _ -> ();;

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

(* print_var
	* print out brill command for variable storage
*)
let print_var fmt stack t =
	match t with
	| Int ->	fprintf fmt "int_const r0, 0@,store %i, r0" stack;
	| Float ->	fprintf fmt "real_const r0, 0@,store %i, r0" stack;
	| Bool -> fprintf fmt "int_const r0, 0@,store %i, r0" stack;;

(*
	* print_arg
	* print out brill command for arguments loading during function call
*)
let print_arg fmt t n =
	match t with
	| Int -> fprintf fmt "store %i, r%i" n n;
	| Float -> fprintf fmt "store %i, r%i" n n;
	| Bool -> fprintf fmt "store %i, r%i" n n;;

(* ========================================================================== *)

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
		print_coercion (lt, rt, lreg, rreg)
		print_binop op
		print_binop_type (t, lt, rt)
		lreg lreg rreg;

and print_binop_type fmt (t1, t2, t3) =
	match t1, t2, t3 with
	| Bool, _, Float -> fprintf fmt "float"
	| Bool, Float, _ -> fprintf fmt "float"
	| Bool, Int, Int -> fprintf fmt "int"
	| Int, _, _ -> fprintf fmt "int"
	| Float, _, _ -> fprintf fmt "float"
	| _ -> fprintf fmt "bool";;

(* generate_unop
	* prints brills unary operator command
*)
let generate_unop fmt (op, { id = id }) =
	match op with
	| Op_not ->
		fprintf fmt "@,not r%i, r%i" !reg !reg
	| Op_minus ->
		let t = lookup_type id in
		fprintf fmt "@,%a_const r%i, -1@,mul_%a r%i, r%i, r%i"
		print_type t (!reg + 1) print_type t !reg !reg (!reg + 1);;

(* generate_expr
	* prints brill command for expressions
*)
let rec generate_expr fmt expr =
	match expr with
	| { expr = Ebool (value); id = id } ->
		incr reg;
		if value = true then
			fprintf fmt "@,int_const r%i, 1" !reg
		else
			fprintf fmt "@,int_const r%i, 0" !reg;
	| { expr = Eint (value); id = id } ->
		incr reg;
		fprintf fmt "@,int_const r%i, %i" !reg value;
	| { expr = Efloat (value); id = id } ->
		incr reg;
		fprintf fmt "@,real_const r%i, %f" !reg value;
	| { expr = EId (ident); id = id } ->
		(match lookup_symbol (this_scope()) ident with
		| Var {var_stack = stack} ->
			incr reg;
			fprintf fmt "@,load r%i, %i" !reg stack;
		| _ -> print_string "Not implemented\n"; exit 0;)
	| { expr = Ebinop (lexpr, (op, _, _), rexpr); id = id } ->
		fprintf fmt "%a%a%a"
		generate_expr lexpr
		generate_expr rexpr
		generate_binop (id, op, lexpr, rexpr);
	| { expr = Eunop ((op, _, _), expr) } ->
		fprintf fmt "%a%a"
		generate_expr expr
		generate_unop (op, expr);
	| { expr = Earray (ident, exprs) } -> ();;

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
	| Write expr_value ->
		fprintf fmt "@,@[<v 4># write%a@]"
		generate_write expr_value;
	| WriteS string_value ->
		fprintf fmt "@,@[<v 4># write%a@]"
		generate_write_string string_value
	| Ifthen (expr, stmts) ->
		fprintf fmt "@,@[<v 4># if%a%a@,label%i:"
		generate_guard expr
		generate_stmts stmts
		!label;
		incr label;
	| Ifthenelse (expr_guard, stmts_then, stmts_else) ->
		fprintf fmt "@,@[<v 4># if%a%a@,    branch_uncond label%i@,label%i:@,# else%a@,label%i:"
		generate_guard expr_guard
		generate_stmts stmts_then
		(!label + 1)
		!label
		generate_stmts stmts_else
		(!label + 1);
		incr label;
		incr label;
	| Assign (LId (ident), expr) ->
		(match lookup_symbol (this_scope()) ident with
		| Var { var_stack = stack } ->
			fprintf fmt "@,@[<v 4># assignment%a@,store %i, r0@]"
			generate_expr expr
			stack;
			decr reg;
		| _ -> print_string "Not implemented\n"; exit 0;)
	| Assign (Larray (ident, exprs), expr) ->
		(match lookup_symbol (this_scope()) ident with
		| Arr { arr_stack = stack } -> ()
		| _ -> print_string "Not implemented\n"; exit 0;)
	| While (expr_guard, stmts) ->
		incr label;
		fprintf fmt "@,# while@,@[<v 4>label%i:%a%a@,    branch_uncond label%i"
		(!label - 1)
		generate_guard expr_guard
		generate_stmts stmts
		(!label -1);
		incr label;
	| Proccall (ident, exprs) ->
		let save_reg = !reg in
		fprintf fmt "@,@[<v 4># proc call%a@,call proc_%s@]"
		generate_arg_exprs exprs
		ident;
		reg := save_reg;
	| _ -> ();
and generate_write fmt ({ id = id } as expr) =
	(match lookup_type id with
	| Int -> fprintf fmt "%a@,call_builtin print_int" generate_expr expr;
	| Float -> fprintf fmt "%a@,call_builtin print_real" generate_expr expr;
	| Bool -> fprintf fmt "%a@,call_builtin print_bool" generate_expr expr;);
	decr reg;
and generate_write_string fmt string_value =
		fprintf fmt "@,string_const r0, \"%s\"@,call_builtin print_string" string_value
and generate_guard fmt expr =
	fprintf fmt "%a@,branch_on_false r0, label%i@]"
	generate_expr expr
	!label;
	decr reg;
and generate_arg_exprs fmt exprs =
	match exprs with
	| expr::tail ->
		fprintf fmt "%a%a"
		generate_expr expr
		generate_arg_exprs tail;
	| [] -> ();;

(* Declaration generator
	* Recursively generates declaration within the current proc
	* Either Variable or Array declaration
*)
let rec generate_decls fmt decls =
	match decls with
	| (decl::tail) ->
		fprintf fmt "@,%a%a"
		generate_decl decl
		generate_decls tail;
	| [] -> ();
and generate_decl fmt decl =
		match decl with
		| Dvar x -> generate_var fmt x;
		| Darr x -> generate_arr fmt x;
and generate_var fmt (t, ident) =
	match lookup_symbol (this_scope()) ident with
	| Var {var_stack = stack} -> print_var fmt stack t;
	| _ -> print_string "Not implemented\n"; exit 0;
and generate_arr fmt arr = ();;

(* Header generator
	* Generator for header of the proc
	* Recursively generates the proc's argument list
*)
let rec generate_header fmt (ident, args) =
	call ident;
	let {frame = frame} = lookup_proc ident in
	fprintf fmt "@,proc_%s:@,@[<v 4># prologue@,push_stack_frame %i%a"
	ident !frame generate_args args;
	reg := -1;
and generate_args fmt args =
	match args with
	| (arg::tail) ->
		fprintf fmt "@,%a%a"
		generate_arg arg
		generate_args tail;
	| [] -> ();
and generate_arg fmt arg =
	match arg with
	| Val (ident, t) ->
		(match lookup_symbol (this_scope()) ident with
		| Var {var_stack = stack} ->
			incr reg;
			print_arg fmt t stack;
		| _ -> print_string "Not implemented\n"; exit 0;)
	| Ref (ident, t) -> ();;

let rec generate_procs fmt prog =
	match prog with
	| {header = (proc, args) as header; decls = decls; stmts = stmts} :: tail ->
		reg := -1;
		fprintf fmt "%a%a@]%a%a%a"
		generate_header header
		generate_decls decls
		generate_stmts stmts
		generate_epilogue proc
		generate_procs tail;
	| [] -> ();
and generate_epilogue fmt proc =
	let {frame = frame} = lookup_proc proc in
	fprintf fmt "@,@[<v 4># epilogue@,pop_stack_frame %i@,return@,@]" !frame;;

let generate prog =
	let fmt = Format.std_formatter in
	fprintf fmt "@[<v>@[<v 4>    call proc_main@,halt@]%a@,@]"
	generate_procs prog;;
