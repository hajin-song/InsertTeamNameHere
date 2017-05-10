open Snick_ast;;
open Symbol;;
open Format;;

let indent = 4;;
let reg = ref (-1);;

let scope : string Stack.t = Stack.create ();;

let call proc =	Stack.push proc scope;;

let return () = Stack.pop scope;;

let this_scope () = Stack.top scope;;

let type_str fmt t =
	match t with
	| Int -> fprintf fmt "int";
	| Float -> fprintf fmt "real";
	| Bool -> fprintf fmt "bool";;

let cooerce fmt (lt, rt, lreg, rreg) =
	match lt, rt with
	| Int, Float ->
		fprintf fmt "@,int_to_real r%i, r%i"
		lreg lreg;
	| Float, Int ->
		fprintf fmt "@,int_to_real r%i, r%i"
		rreg rreg;
	| _, _ -> ();;

let binop_str fmt op =
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

let unop_str fmt op =
	match op with
	| Op_not -> fprintf fmt "not";
	| Op_minus -> ();;

let print_binop fmt (id, op, { id = lid }, { id = rid }) =
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
		cooerce (lt, rt, lreg, rreg)
		binop_str op
		type_str t
		lreg lreg rreg;;

let print_unop fmt (op, { id = id }) =
	match op with
	| Op_not ->
		fprintf fmt "@,not r%i, r%i" !reg !reg
	| Op_minus ->
		let t = lookup_type id in
		fprintf fmt "@,%a_const r%i, -1@,mul_%a r%i, r%i, r%i"
		type_str t (!reg + 1) type_str t !reg !reg (!reg + 1);;

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
		| Var {stack = stack} ->
			incr reg;
			fprintf fmt "@,load r%i, %i" !reg stack;
		| _ -> print_string "Not implemented\n"; exit 0;)
	| { expr = Ebinop (lexpr, (op, _, _), rexpr); id = id } ->
		fprintf fmt "%a%a%a"
		generate_expr lexpr
		generate_expr rexpr
		print_binop (id, op, lexpr, rexpr);
	| { expr = Eunop ((op, _, _), expr) } ->
		fprintf fmt "%a%a"
		generate_expr expr
		print_unop (op, expr);
	| { expr = Earray (ident, exprs) } -> ();;

let print_write fmt ({ id = id } as expr) =
	(match lookup_type id with
	| Int -> fprintf fmt "%a@,call_builtin print_int" generate_expr expr;
	| Float -> fprintf fmt "%a@,call_builtin print_real" generate_expr expr;
	| Bool -> fprintf fmt "%a@,call_builtin print_bool" generate_expr expr;);
	decr reg;;

let rec generate_stmts fmt stmts =
	match stmts with
	| (stmt::tail) ->
		generate_stmt fmt stmt;
		generate_stmts fmt tail;
	| [] -> ();

and generate_stmt fmt stmt =
	match stmt with
	| Write expr ->
		fprintf fmt "@,@[<v 4># write%a@]"
		print_write expr;
	| _ -> ();;

let print_var fmt stack t =
	match t with
	| Int ->
		fprintf fmt "int_const r0, 0@,store %i, r0" stack;
	| Float ->
		fprintf fmt "real_const r0, 0@,store %i, r0" stack;
	| Bool ->
		fprintf fmt "bool_const r0, 0@,store %i, r0" stack;;

let generate_var fmt (t, ident) =
	match lookup_symbol (this_scope()) ident with
	| Var {stack = stack} -> print_var fmt stack t;
	| _ -> print_string "Not implemented\n"; exit 0;;

let generate_arr fmt arr = ();;

let generate_decl fmt decl =
	match decl with
	| Dvar x -> generate_var fmt x;
	| Darr x -> generate_arr fmt x;;

let rec generate_decls fmt decls =
	match decls with
	| (decl::tail) ->
		fprintf fmt "@,%a%a"
		generate_decl decl
		generate_decls tail;
	| [] -> ();;

let rec generate_args fmt (n, args) =
	match args with
	| (arg::tail) ->
		fprintf fmt "@,%a%a"
		generate_arg (n, arg)
		generate_args (n, tail);
	| [] -> ();

and generate_arg fmt (n, arg) = 
	match arg with
	| Val (_, t) -> print_arg fmt t n;
	| Ref (_, t) -> ();

and print_arg fmt t n =
	match t with
	| Int ->
		fprintf fmt "store %i, r%i" n n;
	| Float ->
		fprintf fmt "store %i, r%i" n n;
	| Bool ->
		fprintf fmt "store %i, r%i" n n;;

let generate_header fmt (ident, args) =
	call ident;
	let {frame = frame} = lookup_proc ident in
	fprintf fmt "proc_%s:@,@[<v 4># prologue@,push_stack_frame %i%a"
	ident !frame generate_args (1, args);;

let rec generate_procs fmt prog =
	match prog with
	| {header = header; decls = decls; stmts = stmts} :: tail ->
		fprintf fmt "%a%a@]%a%a"
		generate_header header
		generate_decls decls
		generate_stmts stmts
		generate_procs tail
	| [] -> ();;

let generate prog =
	let fmt = Format.std_formatter in
	fprintf fmt "@[<v>@[<v 4>    call proc_main@,halt@]@,%a@,@]"
	generate_procs prog;;