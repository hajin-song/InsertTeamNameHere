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
\003\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\010\000\009\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000\000\000\001\000\001\000\001\000\002\000\
\000\000\002\000\005\000\007\000\005\000\002\000\002\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\034\000\000\000\005\000\006\000\007\000\000\000\
\003\000\000\000\018\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\019\000\020\000\021\000\000\000\000\000\
\000\000\022\000\014\000\000\000\000\000\010\000\000\000\002\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\009\000\000\000\016\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\000\000\000\000\013\000\009\000\011\000\000\000\012\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\010\000\016\000\017\000\025\000\
\026\000\047\000"

let yysindex = "\003\000\
\000\000\000\000\000\000\001\255\000\000\000\000\000\000\156\255\
\000\000\005\255\000\000\014\255\006\255\014\255\014\255\000\000\
\236\254\004\255\245\254\000\000\000\000\000\000\014\255\014\255\
\161\255\000\000\000\000\095\255\109\255\000\000\014\255\000\000\
\152\255\000\000\014\255\014\255\014\255\014\255\014\255\014\255\
\014\255\014\255\014\255\000\000\000\000\161\255\000\000\000\000\
\078\255\078\255\078\255\078\255\078\255\078\255\007\255\007\255\
\000\000\034\255\134\255\000\000\000\000\000\000\145\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\255\000\000\000\000\
\021\255\069\255\070\255\075\255\080\255\081\255\037\255\053\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\232\255\000\000\000\000\000\000\000\000\244\255\
\248\255\000\000"

let yytablesize = 271
let yytable = "\018\000\
\009\000\028\000\029\000\001\000\027\000\005\000\006\000\007\000\
\019\000\011\000\033\000\034\000\030\000\031\000\020\000\021\000\
\022\000\011\000\046\000\058\000\059\000\032\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\023\000\
\026\000\001\000\043\000\026\000\063\000\011\000\000\000\026\000\
\024\000\012\000\013\000\015\000\014\000\017\000\060\000\015\000\
\023\000\018\000\018\000\023\000\000\000\026\000\018\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\024\000\000\000\000\000\024\000\000\000\023\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\027\000\028\000\000\000\027\000\028\000\024\000\029\000\027\000\
\028\000\029\000\000\000\031\000\030\000\029\000\031\000\030\000\
\000\000\000\000\031\000\030\000\000\000\027\000\028\000\041\000\
\042\000\043\000\044\000\029\000\000\000\000\000\000\000\000\000\
\031\000\030\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\045\000\000\000\000\000\000\000\000\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\011\000\000\000\000\000\000\000\012\000\013\000\000\000\
\014\000\000\000\000\000\015\000\011\000\061\000\062\000\000\000\
\012\000\013\000\000\000\014\000\000\000\000\000\015\000\011\000\
\000\000\064\000\000\000\012\000\013\000\000\000\014\000\000\000\
\000\000\015\000\048\000\035\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\009\000\009\000\000\000\009\000\000\000\000\000\009\000"

let yycheck = "\008\000\
\000\000\014\000\015\000\001\000\013\000\005\001\006\001\007\001\
\004\001\004\001\023\000\024\000\033\001\010\001\001\001\002\001\
\003\001\004\001\031\000\044\000\045\000\033\001\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\018\001\
\012\001\000\000\028\001\015\001\061\000\004\001\255\255\019\001\
\027\001\008\001\009\001\033\001\011\001\033\001\013\001\014\001\
\012\001\058\000\059\000\015\001\255\255\033\001\063\000\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\012\001\255\255\255\255\015\001\255\255\033\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\012\001\012\001\255\255\015\001\015\001\033\001\012\001\019\001\
\019\001\015\001\255\255\012\001\012\001\019\001\015\001\015\001\
\255\255\255\255\019\001\019\001\255\255\033\001\033\001\026\001\
\027\001\028\001\012\001\033\001\255\255\255\255\255\255\255\255\
\033\001\033\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\015\001\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\004\001\255\255\255\255\255\255\008\001\009\001\255\255\
\011\001\255\255\255\255\014\001\004\001\016\001\017\001\255\255\
\008\001\009\001\255\255\011\001\255\255\255\255\014\001\004\001\
\255\255\017\001\255\255\008\001\009\001\255\255\011\001\255\255\
\255\255\014\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\255\255\255\255\
\008\001\009\001\255\255\011\001\255\255\255\255\014\001"

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
# 256 "snack_parse.ml"
               : Snack_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 39 "snack_parse.mly"
                             ( (_2, _1) )
# 264 "snack_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 42 "snack_parse.mly"
               ( _2 :: _1 )
# 272 "snack_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "snack_parse.mly"
    ( [] )
# 278 "snack_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "snack_parse.mly"
         ( Bool )
# 284 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "snack_parse.mly"
        ( Int )
# 290 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "snack_parse.mly"
          ( Float )
# 296 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 52 "snack_parse.mly"
               ( _2 :: _1 )
# 304 "snack_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "snack_parse.mly"
    ( [] )
# 310 "snack_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 56 "snack_parse.mly"
                        ( _1 )
# 317 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 57 "snack_parse.mly"
                          ( Ifthen (_2, _4) )
# 325 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 58 "snack_parse.mly"
                                     ( Ifthenelse (_2, _4, _6) )
# 334 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 59 "snack_parse.mly"
                           ( While (_2, _4) )
# 342 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 62 "snack_parse.mly"
                ( Read _2 )
# 349 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "snack_parse.mly"
               ( Write _2 )
# 356 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 64 "snack_parse.mly"
                         ( Assign (_1, _3) )
# 364 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "snack_parse.mly"
         ( Rexpr _1 )
# 371 "snack_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "snack_parse.mly"
          ( LId _1 )
# 378 "snack_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 73 "snack_parse.mly"
               ( Ebool _1 )
# 385 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "snack_parse.mly"
              ( Eint _1 )
# 392 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 75 "snack_parse.mly"
                ( Efloat _1 )
# 399 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 76 "snack_parse.mly"
           ( Elval _1 )
# 406 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "snack_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 414 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "snack_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 422 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "snack_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 430 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "snack_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 438 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "snack_parse.mly"
                  ( Ebinop (_1, Op_neq, _3) )
# 446 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "snack_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 454 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "snack_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 462 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "snack_parse.mly"
                   ( Ebinop (_1, Op_lteq, _3) )
# 470 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "snack_parse.mly"
                   ( Ebinop (_1, Op_gteq, _3) )
# 478 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "snack_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 485 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "snack_parse.mly"
                       ( _2 )
# 492 "snack_parse.ml"
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
