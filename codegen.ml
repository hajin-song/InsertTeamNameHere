open Snick_ast;;
open Symbol;;
open Format;;

let indent = 4;;
let reg = ref (-1);;

let scope : string Stack.t = Stack.create ();;

let call proc =	Stack.push proc scope;;

let return () = Stack.pop scope;;

let this_scope () = Stack.top scope;;

let print_binop fmt (op, lt, rt) =
	let rreg = !reg in
	decr reg;
	let lreg = !reg in
	match op with
	| Op_add -> (
		match lt, rt with
		| Int, Int -> fprintf fmt "@,add_int r%i, r%i, r%i" lreg lreg rreg;
		| Int, Float ->
			fprintf fmt "@,int_to_real r%i, r%i@,add_real r%i, r%i, r%i"
			lreg lreg lreg lreg rreg
		| Float, Int ->
			fprintf fmt "@,int_to_real r%i, r%i@,add_real r%i, r%i, r%i"
			rreg rreg lreg lreg rreg
		| Float, Float -> fprintf fmt "@,add_real r%i, r%i, r%i" lreg lreg rreg;
		| _, _ -> ())
	| Op_sub -> ()
	| Op_mul -> ()
	| Op_div -> ()
	| Op_eq -> ()
	| Op_neq -> ()
	| Op_lt -> ()
	| Op_gt -> ()
	| Op_gteq -> ()
	| Op_lteq -> ()
	| Op_or -> ()
	| Op_and -> ()

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
	| { expr = Ebinop (({id = lid} as lexpr), (op, _, _), ({id = rid} as rexpr))} ->
		generate_expr fmt lexpr;
		generate_expr fmt rexpr;
		let lt = lookup_type lid in
		let rt = lookup_type rid in
		print_binop fmt (op, lt, rt);
	| { expr = Eunop ((unop, _, _), expr); id = id } -> ();
	| { expr = Earray (ident, exprs); id = id } -> ();;

let print_write fmt ({ expr = e; id = id } as expr) =
	generate_expr fmt expr;
	decr reg;
	match lookup_type id with
	| Int -> fprintf fmt "@,call_builtin print_int";
	| Float -> fprintf fmt "@,call_builtin print_real";
	| Bool -> fprintf fmt "@,call_builtin print_bool";;

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
	fprintf fmt "@[<v>@[<v 4>    call proc_main@,halt@]@,%a@,@]" generate_procs prog;;