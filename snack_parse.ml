type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (float)
  | IDENT of (string)
  | BOOL
  | INT
  | FLOAT
  | WRITE
  | READ
  | ASSIGN
  | WHILE
  | DO
  | OD
  | IF
  | THEN
  | ELSE
  | FI
  | LPAREN
  | RPAREN
  | EQ
  | NEQ
  | LT
  | GT
  | GTEQ
  | LTEQ
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | NOT
  | SEMICOLON
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "snack_parse.mly"
open Snack_ast
# 42 "snack_parse.ml"
let yytransl_const = [|
  261 (* BOOL *);
  262 (* INT *);
  263 (* FLOAT *);
  264 (* WRITE *);
  265 (* READ *);
  266 (* ASSIGN *);
  267 (* WHILE *);
  268 (* DO *);
  269 (* OD *);
  270 (* IF *);
  271 (* THEN *);
  272 (* ELSE *);
  273 (* FI *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* GT *);
  280 (* GTEQ *);
  281 (* LTEQ *);
  282 (* PLUS *);
  283 (* MINUS *);
  284 (* MUL *);
  285 (* DIV *);
  286 (* AND *);
  287 (* OR *);
  288 (* NOT *);
  289 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* FLOAT_CONST *);
  260 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\002\000\002\000\005\000\005\000\005\000\003\000\
\003\000\006\000\007\000\007\000\007\000\010\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000\000\000\001\000\001\000\001\000\002\000\
\000\000\002\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\031\000\000\000\005\000\006\000\007\000\000\000\
\003\000\000\000\015\000\000\000\000\000\008\000\000\000\000\000\
\000\000\016\000\017\000\018\000\000\000\000\000\019\000\000\000\
\011\000\010\000\000\000\002\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\010\000\014\000\015\000\023\000\
\024\000\041\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\083\255\000\000\000\000\000\000\051\255\
\000\000\252\254\000\000\001\255\003\255\000\000\240\254\014\255\
\248\254\000\000\000\000\000\000\001\255\001\255\000\000\059\255\
\000\000\000\000\001\255\000\000\050\255\000\000\001\255\001\255\
\001\255\001\255\001\255\001\255\001\255\001\255\001\255\059\255\
\000\000\000\000\065\255\065\255\065\255\065\255\065\255\065\255\
\255\254\255\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\255\
\000\000\000\000\029\255\030\255\031\255\032\255\033\255\035\255\
\245\254\020\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\255\255\000\000"

let yytablesize = 266
let yytable = "\017\000\
\009\000\018\000\019\000\020\000\011\000\001\000\011\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\026\000\016\000\021\000\029\000\030\000\020\000\025\000\027\000\
\028\000\040\000\039\000\022\000\001\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\023\000\
\024\000\025\000\026\000\028\000\021\000\027\000\011\000\012\000\
\014\000\000\000\012\000\013\000\000\000\023\000\024\000\025\000\
\026\000\028\000\000\000\027\000\042\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\005\000\
\006\000\007\000\037\000\038\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\009\000\009\000"

let yycheck = "\004\001\
\000\000\001\001\002\001\003\001\004\001\001\000\004\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\033\001\008\000\018\001\021\000\022\000\033\001\013\000\010\001\
\033\001\027\000\028\001\027\001\000\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\019\001\
\019\001\019\001\019\001\019\001\033\001\019\001\004\001\033\001\
\033\001\255\255\008\001\009\001\255\255\033\001\033\001\033\001\
\033\001\033\001\255\255\033\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\005\001\
\006\001\007\001\026\001\027\001\028\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\255\255\255\255\
\008\001\009\001"

let yynames_const = "\
  BOOL\000\
  INT\000\
  FLOAT\000\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  WHILE\000\
  DO\000\
  OD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  GTEQ\000\
  LTEQ\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  AND\000\
  OR\000\
  NOT\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  FLOAT_CONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 36 "snack_parse.mly"
              ( { decls = List.rev _1 ; stmts = List.rev _2 } )
# 251 "snack_parse.ml"
               : Snack_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 39 "snack_parse.mly"
                             ( (_2, _1) )
# 259 "snack_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 42 "snack_parse.mly"
               ( _2 :: _1 )
# 267 "snack_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "snack_parse.mly"
    ( [] )
# 273 "snack_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "snack_parse.mly"
         ( Bool )
# 279 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "snack_parse.mly"
        ( Int )
# 285 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "snack_parse.mly"
          ( Float )
# 291 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 52 "snack_parse.mly"
               ( _2 :: _1 )
# 299 "snack_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "snack_parse.mly"
    ( [] )
# 305 "snack_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 56 "snack_parse.mly"
                      ( _1 )
# 312 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 59 "snack_parse.mly"
                ( Read _2 )
# 319 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "snack_parse.mly"
               ( Write _2 )
# 326 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 61 "snack_parse.mly"
                         ( Assign (_1, _3) )
# 334 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "snack_parse.mly"
         ( Rexpr _1 )
# 341 "snack_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "snack_parse.mly"
          ( LId _1 )
# 348 "snack_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 70 "snack_parse.mly"
               ( Ebool _1 )
# 355 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "snack_parse.mly"
              ( Eint _1 )
# 362 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 72 "snack_parse.mly"
                ( Efloat _1 )
# 369 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 73 "snack_parse.mly"
           ( Elval _1 )
# 376 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "snack_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 384 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "snack_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 392 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "snack_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 400 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "snack_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 408 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "snack_parse.mly"
                  ( Ebinop (_1, Op_neq, _3) )
# 416 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "snack_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 424 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "snack_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 432 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "snack_parse.mly"
                   ( Ebinop (_1, Op_lteq, _3) )
# 440 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "snack_parse.mly"
                   ( Ebinop (_1, Op_gteq, _3) )
# 448 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "snack_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 455 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "snack_parse.mly"
                       ( _2 )
# 462 "snack_parse.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Snack_ast.program)
