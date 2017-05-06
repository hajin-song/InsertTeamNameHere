type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (float)
  | IDENT of (string)
  | STRING of (string)
  | PROC
  | VAL
  | REF
  | END
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
  | LBRACKET
  | RBRACKET
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
  | RANGE
  | AND
  | OR
  | NOT
  | SEMICOLON
  | COMMA
  | QUOTE
  | EOF

open Parsing;;
let _ = parse_error;;
# 10 "snick_parse.mly"
open Snick_ast
let expr_count = ref 0
# 53 "snick_parse.ml"
let yytransl_const = [|
  262 (* PROC *);
  263 (* VAL *);
  264 (* REF *);
  265 (* END *);
  266 (* BOOL *);
  267 (* INT *);
  268 (* FLOAT *);
  269 (* WRITE *);
  270 (* READ *);
  271 (* ASSIGN *);
  272 (* WHILE *);
  273 (* DO *);
  274 (* OD *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* LBRACKET *);
  282 (* RBRACKET *);
  283 (* EQ *);
  284 (* NEQ *);
  285 (* LT *);
  286 (* GT *);
  287 (* GTEQ *);
  288 (* LTEQ *);
  289 (* PLUS *);
  290 (* MINUS *);
  291 (* MUL *);
  292 (* DIV *);
  293 (* RANGE *);
  294 (* AND *);
  295 (* OR *);
  296 (* NOT *);
  297 (* SEMICOLON *);
  298 (* COMMA *);
  299 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* FLOAT_CONST *);
  260 (* IDENT *);
  261 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\007\000\007\000\007\000\004\000\
\009\000\009\000\008\000\008\000\008\000\010\000\010\000\005\000\
\005\000\012\000\011\000\011\000\006\000\006\000\013\000\013\000\
\013\000\013\000\014\000\014\000\014\000\014\000\014\000\016\000\
\016\000\017\000\017\000\017\000\018\000\018\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\001\000\002\000\000\000\005\000\001\000\001\000\001\000\004\000\
\003\000\003\000\003\000\001\000\000\000\003\000\006\000\002\000\
\000\000\003\000\003\000\001\000\002\000\000\000\002\000\005\000\
\007\000\005\000\002\000\002\000\002\000\003\000\004\000\001\000\
\004\000\003\000\001\000\000\000\003\000\001\000\001\000\001\000\
\001\000\001\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\059\000\000\000\000\000\002\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\012\000\005\000\006\000\
\007\000\000\000\000\000\016\000\000\000\000\000\008\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\021\000\000\000\
\000\000\000\000\010\000\009\000\011\000\000\000\000\000\039\000\
\040\000\041\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\023\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\057\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\022\000\000\000\000\000\
\000\000\020\000\031\000\000\000\033\000\000\000\000\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\043\000\026\000\022\000\024\000\018\000\
\015\000\019\000\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\008\000\010\000\018\000\019\000\013\000\
\014\000\020\000\081\000\082\000\031\000\032\000\059\000\033\000\
\058\000\060\000"

let yysindex = "\015\000\
\000\000\000\000\000\000\045\255\049\255\000\000\037\255\000\000\
\002\255\011\255\011\255\011\255\238\254\000\000\000\000\000\000\
\000\000\244\255\065\255\000\000\069\255\070\255\000\000\002\255\
\031\255\000\000\027\255\090\255\032\255\032\255\000\000\066\255\
\101\255\232\254\000\000\000\000\000\000\032\255\032\255\000\000\
\000\000\000\000\081\255\000\000\032\255\032\255\032\255\071\000\
\102\255\000\000\239\255\003\000\000\000\032\255\132\255\000\000\
\071\000\239\254\071\000\100\255\032\255\037\000\000\000\096\000\
\032\255\032\255\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\032\255\032\255\000\000\000\000\071\000\103\255\
\112\255\000\000\000\000\032\255\000\000\032\255\120\255\000\000\
\235\254\235\254\235\254\235\254\235\254\235\254\029\255\029\255\
\000\000\000\000\096\000\084\000\070\000\036\000\142\255\106\255\
\132\255\071\000\071\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\141\000\000\000\000\000\000\000\000\000\
\240\254\078\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\133\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\255\000\000\000\000\
\000\000\000\000\051\255\000\000\000\000\000\000\000\000\110\255\
\114\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\140\255\000\000\000\000\000\000\000\000\111\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\127\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\119\255\139\255\147\255\167\255\175\255\195\255\071\255\091\255\
\000\000\000\000\203\255\223\255\000\000\000\000\000\000\000\000\
\000\000\072\255\148\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\183\255\051\000\000\000\
\145\000\000\000\000\000\065\000\000\000\000\000\229\255\144\000\
\000\000\114\000"

let yytablesize = 388
let yytable = "\048\000\
\055\000\051\000\052\000\101\000\102\000\023\000\083\000\013\000\
\011\000\012\000\057\000\071\000\072\000\073\000\074\000\001\000\
\056\000\062\000\063\000\064\000\015\000\016\000\017\000\024\000\
\084\000\013\000\079\000\040\000\041\000\042\000\043\000\044\000\
\040\000\041\000\042\000\043\000\115\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\045\000\005\000\036\000\007\000\038\000\045\000\039\000\
\106\000\035\000\107\000\009\000\046\000\021\000\022\000\073\000\
\074\000\046\000\047\000\042\000\034\000\036\000\042\000\047\000\
\035\000\036\000\042\000\035\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\044\000\
\042\000\042\000\044\000\042\000\042\000\049\000\044\000\034\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\061\000\053\000\045\000\044\000\044\000\045\000\044\000\
\044\000\034\000\045\000\054\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\085\000\039\000\056\000\
\045\000\045\000\056\000\045\000\045\000\080\000\056\000\048\000\
\056\000\104\000\048\000\103\000\001\000\086\000\048\000\112\000\
\048\000\108\000\113\000\032\000\056\000\056\000\028\000\056\000\
\056\000\105\000\032\000\049\000\048\000\048\000\049\000\048\000\
\048\000\086\000\049\000\050\000\049\000\038\000\050\000\030\000\
\037\000\114\000\050\000\050\000\050\000\037\000\087\000\000\000\
\049\000\049\000\000\000\049\000\049\000\038\000\000\000\051\000\
\050\000\050\000\051\000\050\000\050\000\037\000\051\000\053\000\
\051\000\000\000\053\000\000\000\000\000\000\000\053\000\000\000\
\053\000\000\000\000\000\000\000\051\000\051\000\000\000\051\000\
\051\000\000\000\000\000\052\000\053\000\053\000\052\000\053\000\
\053\000\000\000\052\000\055\000\052\000\000\000\055\000\000\000\
\000\000\000\000\055\000\000\000\055\000\000\000\000\000\000\000\
\052\000\052\000\000\000\052\000\052\000\000\000\000\000\054\000\
\055\000\055\000\054\000\055\000\055\000\000\000\054\000\025\000\
\054\000\000\000\000\000\000\000\026\000\000\000\000\000\077\000\
\027\000\028\000\000\000\029\000\000\000\054\000\030\000\054\000\
\054\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\000\000\075\000\076\000\078\000\000\000\
\000\000\000\000\000\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\025\000\
\075\000\076\000\000\000\025\000\000\000\000\000\000\000\000\000\
\027\000\028\000\000\000\029\000\027\000\028\000\030\000\029\000\
\110\000\111\000\030\000\000\000\088\000\116\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\025\000\075\000\076\000\000\000\000\000\000\000\000\000\
\000\000\022\000\027\000\028\000\000\000\029\000\022\000\109\000\
\030\000\000\000\022\000\022\000\000\000\022\000\000\000\000\000\
\022\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\000\000\075\000\076\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\000\000\075\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\073\000\074\000"

let yycheck = "\027\000\
\025\001\029\000\030\000\077\000\078\000\024\001\024\001\024\001\
\007\001\008\001\038\000\033\001\034\001\035\001\036\001\001\000\
\041\001\045\000\046\000\047\000\010\001\011\001\012\001\042\001\
\042\001\042\001\054\000\001\001\002\001\003\001\004\001\005\001\
\001\001\002\001\003\001\004\001\110\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\023\001\006\001\024\001\004\001\023\001\023\001\025\001\
\084\000\024\001\086\000\023\001\034\001\011\000\012\000\035\001\
\036\001\034\001\040\001\017\001\004\001\042\001\020\001\040\001\
\004\001\004\001\024\001\042\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\017\001\
\038\001\039\001\020\001\041\001\042\001\004\001\024\001\024\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\025\001\041\001\017\001\038\001\039\001\020\001\041\001\
\042\001\042\001\024\001\015\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\026\001\025\001\017\001\
\038\001\039\001\020\001\041\001\042\001\002\001\024\001\017\001\
\026\001\026\001\020\001\037\001\000\000\042\001\024\001\002\001\
\026\001\026\001\041\001\015\001\038\001\039\001\041\001\041\001\
\042\001\042\001\041\001\017\001\038\001\039\001\020\001\041\001\
\042\001\042\001\024\001\017\001\026\001\026\001\020\001\041\001\
\024\000\105\000\024\001\028\000\026\001\026\001\061\000\255\255\
\038\001\039\001\255\255\041\001\042\001\042\001\255\255\017\001\
\038\001\039\001\020\001\041\001\042\001\042\001\024\001\017\001\
\026\001\255\255\020\001\255\255\255\255\255\255\024\001\255\255\
\026\001\255\255\255\255\255\255\038\001\039\001\255\255\041\001\
\042\001\255\255\255\255\017\001\038\001\039\001\020\001\041\001\
\042\001\255\255\024\001\017\001\026\001\255\255\020\001\255\255\
\255\255\255\255\024\001\255\255\026\001\255\255\255\255\255\255\
\038\001\039\001\255\255\041\001\042\001\255\255\255\255\017\001\
\038\001\039\001\020\001\041\001\042\001\255\255\024\001\004\001\
\026\001\255\255\255\255\255\255\009\001\255\255\255\255\017\001\
\013\001\014\001\255\255\016\001\255\255\039\001\019\001\041\001\
\042\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\255\255\038\001\039\001\020\001\255\255\
\255\255\255\255\255\255\255\255\255\255\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\004\001\
\038\001\039\001\255\255\004\001\255\255\255\255\255\255\255\255\
\013\001\014\001\255\255\016\001\013\001\014\001\019\001\016\001\
\021\001\022\001\019\001\255\255\024\001\022\001\255\255\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\004\001\038\001\039\001\255\255\255\255\255\255\255\255\
\255\255\004\001\013\001\014\001\255\255\016\001\009\001\018\001\
\019\001\255\255\013\001\014\001\255\255\016\001\255\255\255\255\
\019\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\255\255\038\001\039\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\255\255\038\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001"

let yynames_const = "\
  PROC\000\
  VAL\000\
  REF\000\
  END\000\
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
  LBRACKET\000\
  RBRACKET\000\
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
  RANGE\000\
  AND\000\
  OR\000\
  NOT\000\
  SEMICOLON\000\
  COMMA\000\
  QUOTE\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  FLOAT_CONST\000\
  IDENT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procs) in
    Obj.repr(
# 46 "snick_parse.mly"
               ( List.rev _1 )
# 345 "snick_parse.ml"
               : Snick_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 49 "snick_parse.mly"
               ( _2 :: _1 )
# 353 "snick_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "snick_parse.mly"
    ( [] )
# 359 "snick_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'header) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 52 "snick_parse.mly"
                                   ( { header = _2 ; decls = List.rev _3 ; stmts = List.rev _4 } )
# 368 "snick_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "snick_parse.mly"
         ( Bool )
# 374 "snick_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "snick_parse.mly"
        ( Int )
# 380 "snick_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "snick_parse.mly"
          ( Float )
# 386 "snick_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 60 "snick_parse.mly"
                                  ( (_1, List.rev _3) )
# 394 "snick_parse.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "snick_parse.mly"
                       ( Ref (_3, _2) )
# 402 "snick_parse.ml"
               : 'argument))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "snick_parse.mly"
                       ( Val (_3, _2) )
# 410 "snick_parse.ml"
               : 'argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arguments) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argument) in
    Obj.repr(
# 67 "snick_parse.mly"
                             ( _3 :: _1 )
# 418 "snick_parse.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argument) in
    Obj.repr(
# 68 "snick_parse.mly"
             ( [_1] )
# 425 "snick_parse.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "snick_parse.mly"
    ( [] )
# 431 "snick_parse.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "snick_parse.mly"
                             ( Dvar (_1, _2) )
# 439 "snick_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'range_list) in
    Obj.repr(
# 73 "snick_parse.mly"
                                                          ( Darr (_1, _2, _4) )
# 448 "snick_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 76 "snick_parse.mly"
               ( _2 :: _1 )
# 456 "snick_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "snick_parse.mly"
    ( [] )
# 462 "snick_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 80 "snick_parse.mly"
                              ( (_1, _3) )
# 470 "snick_parse.ml"
               : 'range))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'range_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 83 "snick_parse.mly"
                           ( _3 :: _1 )
# 478 "snick_parse.ml"
               : 'range_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 84 "snick_parse.mly"
          ( [_1] )
# 485 "snick_parse.ml"
               : 'range_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 88 "snick_parse.mly"
               ( _2 :: _1 )
# 493 "snick_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "snick_parse.mly"
    ( [] )
# 499 "snick_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 92 "snick_parse.mly"
                        ( _1 )
# 506 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 93 "snick_parse.mly"
                          ( Ifthen (_2, List.rev _4) )
# 514 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 94 "snick_parse.mly"
                                     ( Ifthenelse (_2, List.rev _4, List.rev _6) )
# 523 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 95 "snick_parse.mly"
                           ( While (_2, List.rev _4) )
# 531 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 98 "snick_parse.mly"
                ( Read _2 )
# 538 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "snick_parse.mly"
               ( Write _2 )
# 545 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "snick_parse.mly"
                 ( WriteS _2 )
# 552 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "snick_parse.mly"
                       ( Assign (_1, _3) )
# 560 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 102 "snick_parse.mly"
                                  ( Proccall (_1, List.rev _3) )
# 568 "snick_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "snick_parse.mly"
          ( LId _1 )
# 575 "snick_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arr_list) in
    Obj.repr(
# 106 "snick_parse.mly"
                                     ( Larray (_1, List.rev _3) )
# 583 "snick_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "snick_parse.mly"
                         ( _3 :: _1 )
# 591 "snick_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "snick_parse.mly"
         ( [_1] )
# 598 "snick_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "snick_parse.mly"
    ( [] )
# 604 "snick_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "snick_parse.mly"
                        ( _3 :: _1 )
# 612 "snick_parse.ml"
               : 'arr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "snick_parse.mly"
         ( [_1] )
# 619 "snick_parse.ml"
               : 'arr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 119 "snick_parse.mly"
               ( Ebool _1 )
# 626 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "snick_parse.mly"
              ( Eint _1 )
# 633 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 121 "snick_parse.mly"
                ( Efloat _1 )
# 640 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "snick_parse.mly"
          ( EId _1 )
# 647 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arr_list) in
    Obj.repr(
# 123 "snick_parse.mly"
                                     ( incr expr_count; Earray (_1, List.rev _3, !expr_count) )
# 655 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "snick_parse.mly"
                   (incr expr_count; Ebinop (_1, (Op_add, Prec_addsub, Left_assoc), _3, !expr_count) )
# 663 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "snick_parse.mly"
                    (incr expr_count; Ebinop (_1, (Op_sub, Prec_addsub, Left_assoc), _3, !expr_count) )
# 671 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "snick_parse.mly"
                  (incr expr_count; Ebinop (_1, (Op_mul, Prec_muldiv, Left_assoc), _3, !expr_count) )
# 679 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "snick_parse.mly"
                  (incr expr_count; Ebinop (_1, (Op_div, Prec_muldiv, Left_assoc), _3, !expr_count) )
# 687 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "snick_parse.mly"
                 (incr expr_count; Ebinop (_1, (Op_eq, Prec_eq, Non_assoc), _3, !expr_count) )
# 695 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "snick_parse.mly"
                  (incr expr_count; Ebinop (_1, (Op_neq, Prec_eq, Non_assoc), _3, !expr_count) )
# 703 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "snick_parse.mly"
                 (incr expr_count; Ebinop (_1, (Op_lt, Prec_eq, Non_assoc), _3, !expr_count) )
# 711 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "snick_parse.mly"
                 (incr expr_count; Ebinop (_1, (Op_gt, Prec_eq, Non_assoc), _3, !expr_count) )
# 719 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "snick_parse.mly"
                   (incr expr_count; Ebinop (_1, (Op_lteq, Prec_eq, Non_assoc), _3, !expr_count) )
# 727 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "snick_parse.mly"
                   (incr expr_count; Ebinop (_1, (Op_gteq, Prec_eq, Non_assoc), _3, !expr_count) )
# 735 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "snick_parse.mly"
                 (incr expr_count; Ebinop (_1, (Op_or, Prec_or, Left_assoc), _3, !expr_count) )
# 743 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "snick_parse.mly"
                  (incr expr_count; Ebinop (_1, (Op_and, Prec_and, Left_assoc), _3, !expr_count) )
# 751 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "snick_parse.mly"
             (incr expr_count; Eunop ((Op_not, Prec_not, Left_assoc), _2, !expr_count) )
# 758 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "snick_parse.mly"
                            (incr expr_count; Eunop ((Op_minus, Prec_uminus, Non_assoc), _2, !expr_count) )
# 765 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 139 "snick_parse.mly"
                       ( _2 )
# 772 "snick_parse.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Snick_ast.program)
