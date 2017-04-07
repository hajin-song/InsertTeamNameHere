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
# 3 "snack_parse.mly"
open Snack_ast
# 52 "snack_parse.ml"
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
\013\000\013\000\014\000\014\000\014\000\014\000\014\000\017\000\
\016\000\016\000\018\000\018\000\018\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\000\000"

let yylen = "\002\000\
\001\000\002\000\000\000\005\000\001\000\001\000\001\000\004\000\
\003\000\003\000\003\000\001\000\000\000\003\000\006\000\002\000\
\000\000\003\000\003\000\001\000\002\000\000\000\002\000\005\000\
\007\000\005\000\002\000\002\000\002\000\003\000\004\000\001\000\
\001\000\004\000\003\000\001\000\000\000\001\000\001\000\001\000\
\001\000\004\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\058\000\000\000\000\000\002\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\012\000\005\000\006\000\
\007\000\000\000\000\000\016\000\000\000\000\000\008\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\021\000\000\000\
\000\000\000\000\010\000\009\000\011\000\000\000\000\000\038\000\
\039\000\040\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\023\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\056\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\022\000\000\000\030\000\000\000\
\000\000\020\000\031\000\000\000\034\000\000\000\057\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\045\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\042\000\026\000\022\000\024\000\018\000\015\000\019\000\
\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\008\000\010\000\018\000\019\000\013\000\
\014\000\020\000\081\000\082\000\031\000\032\000\057\000\033\000\
\079\000\058\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\001\255\047\255\000\000\242\254\000\000\
\055\255\005\255\005\255\005\255\238\254\000\000\000\000\000\000\
\000\000\049\000\052\255\000\000\060\255\061\255\000\000\055\255\
\029\255\000\000\027\255\065\255\032\255\032\255\000\000\018\255\
\079\255\101\255\000\000\000\000\000\000\032\255\032\255\000\000\
\000\000\000\000\102\255\000\000\032\255\032\255\032\255\059\000\
\115\255\000\000\221\255\238\255\000\000\032\255\145\255\000\000\
\059\000\072\255\112\255\032\255\016\000\000\000\084\000\032\255\
\032\255\032\255\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\032\255\000\000\000\000\059\000\000\000\104\255\
\148\255\000\000\000\000\032\255\000\000\156\255\000\000\234\254\
\234\254\234\254\234\254\234\254\234\254\038\255\038\255\000\000\
\000\000\084\000\072\000\053\000\015\000\149\255\114\255\145\255\
\059\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\168\000\000\000\000\000\000\000\000\000\
\092\255\066\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\154\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\124\255\168\255\000\000\
\000\000\000\000\051\255\000\000\000\000\000\000\000\000\129\255\
\131\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\000\000\168\255\000\000\000\000\111\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\134\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\119\255\
\139\255\147\255\167\255\175\255\195\255\071\255\091\255\000\000\
\000\000\203\255\240\254\000\000\000\000\000\000\000\000\000\000\
\120\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\201\255\095\000\000\000\
\152\000\000\000\000\000\075\000\000\000\000\000\229\255\155\000\
\000\000\010\000"

let yytablesize = 376
let yytable = "\048\000\
\053\000\051\000\052\000\053\000\001\000\023\000\005\000\053\000\
\009\000\053\000\070\000\071\000\072\000\073\000\015\000\016\000\
\017\000\061\000\062\000\063\000\100\000\101\000\053\000\024\000\
\053\000\053\000\078\000\040\000\041\000\042\000\043\000\044\000\
\040\000\041\000\042\000\043\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\059\000\045\000\007\000\038\000\113\000\039\000\045\000\034\000\
\105\000\036\000\053\000\036\000\046\000\011\000\012\000\035\000\
\036\000\046\000\047\000\041\000\049\000\086\000\041\000\047\000\
\072\000\073\000\041\000\036\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\043\000\
\041\000\041\000\043\000\041\000\041\000\054\000\043\000\083\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\021\000\022\000\044\000\043\000\043\000\044\000\043\000\
\043\000\084\000\044\000\013\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\055\000\060\000\055\000\
\044\000\044\000\055\000\044\000\044\000\013\000\055\000\047\000\
\055\000\085\000\047\000\039\000\102\000\056\000\047\000\035\000\
\047\000\035\000\080\000\037\000\055\000\055\000\110\000\055\000\
\055\000\084\000\111\000\048\000\047\000\047\000\048\000\047\000\
\047\000\035\000\048\000\049\000\048\000\037\000\049\000\001\000\
\033\000\028\000\049\000\033\000\049\000\103\000\032\000\037\000\
\048\000\048\000\112\000\048\000\048\000\106\000\050\000\050\000\
\049\000\049\000\050\000\049\000\049\000\104\000\050\000\052\000\
\050\000\037\000\052\000\000\000\000\000\084\000\052\000\000\000\
\052\000\000\000\000\000\000\000\050\000\050\000\000\000\050\000\
\050\000\037\000\000\000\051\000\052\000\052\000\051\000\052\000\
\052\000\000\000\051\000\054\000\051\000\000\000\054\000\000\000\
\000\000\000\000\054\000\000\000\054\000\000\000\000\000\000\000\
\051\000\051\000\000\000\051\000\051\000\076\000\000\000\000\000\
\054\000\054\000\000\000\054\000\054\000\000\000\000\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\077\000\074\000\075\000\000\000\000\000\000\000\000\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\025\000\074\000\075\000\000\000\025\000\000\000\
\000\000\000\000\000\000\027\000\028\000\000\000\029\000\027\000\
\028\000\030\000\029\000\108\000\109\000\030\000\000\000\087\000\
\114\000\000\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\025\000\074\000\075\000\000\000\
\025\000\026\000\000\000\000\000\000\000\027\000\028\000\000\000\
\029\000\027\000\028\000\030\000\029\000\022\000\107\000\030\000\
\000\000\000\000\022\000\000\000\000\000\000\000\022\000\022\000\
\000\000\022\000\000\000\000\000\022\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\000\000\
\074\000\075\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\000\000\074\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000"

let yycheck = "\027\000\
\017\001\029\000\030\000\020\001\001\000\024\001\006\001\024\001\
\023\001\026\001\033\001\034\001\035\001\036\001\010\001\011\001\
\012\001\045\000\046\000\047\000\076\000\077\000\039\001\042\001\
\041\001\042\001\054\000\001\001\002\001\003\001\004\001\005\001\
\001\001\002\001\003\001\004\001\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\039\000\023\001\004\001\023\001\108\000\025\001\023\001\004\001\
\084\000\024\001\041\001\026\001\034\001\007\001\008\001\004\001\
\004\001\034\001\040\001\017\001\004\001\060\000\020\001\040\001\
\035\001\036\001\024\001\042\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\017\001\
\038\001\039\001\020\001\041\001\042\001\015\001\024\001\024\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\011\000\012\000\017\001\038\001\039\001\020\001\041\001\
\042\001\042\001\024\001\024\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\025\001\025\001\017\001\
\038\001\039\001\020\001\041\001\042\001\042\001\024\001\017\001\
\026\001\026\001\020\001\025\001\037\001\041\001\024\001\024\001\
\026\001\026\001\002\001\024\001\038\001\039\001\002\001\041\001\
\042\001\042\001\041\001\017\001\038\001\039\001\020\001\041\001\
\042\001\042\001\024\001\017\001\026\001\042\001\020\001\000\000\
\015\001\041\001\024\001\041\001\026\001\026\001\041\001\024\000\
\038\001\039\001\104\000\041\001\042\001\026\001\028\000\017\001\
\038\001\039\001\020\001\041\001\042\001\042\001\024\001\017\001\
\026\001\026\001\020\001\255\255\255\255\042\001\024\001\255\255\
\026\001\255\255\255\255\255\255\038\001\039\001\255\255\041\001\
\042\001\042\001\255\255\017\001\038\001\039\001\020\001\041\001\
\042\001\255\255\024\001\017\001\026\001\255\255\020\001\255\255\
\255\255\255\255\024\001\255\255\026\001\255\255\255\255\255\255\
\038\001\039\001\255\255\041\001\042\001\017\001\255\255\255\255\
\038\001\039\001\255\255\041\001\042\001\255\255\255\255\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\020\001\038\001\039\001\255\255\255\255\255\255\255\255\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\004\001\038\001\039\001\255\255\004\001\255\255\
\255\255\255\255\255\255\013\001\014\001\255\255\016\001\013\001\
\014\001\019\001\016\001\021\001\022\001\019\001\255\255\024\001\
\022\001\255\255\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\004\001\038\001\039\001\255\255\
\004\001\009\001\255\255\255\255\255\255\013\001\014\001\255\255\
\016\001\013\001\014\001\019\001\016\001\004\001\018\001\019\001\
\255\255\255\255\009\001\255\255\255\255\255\255\013\001\014\001\
\255\255\016\001\255\255\255\255\019\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\255\255\
\038\001\039\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\255\255\038\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001"

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
# 37 "snack_parse.mly"
               ( List.rev _1 )
# 340 "snack_parse.ml"
               : Snack_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 40 "snack_parse.mly"
               ( _2 :: _1 )
# 348 "snack_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "snack_parse.mly"
    ( [] )
# 354 "snack_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'header) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 43 "snack_parse.mly"
                                   ( { header = _2 ; decls = List.rev _3 ; stmts = List.rev _4 } )
# 363 "snack_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "snack_parse.mly"
         ( Bool )
# 369 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "snack_parse.mly"
        ( Int )
# 375 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "snack_parse.mly"
          ( Float )
# 381 "snack_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 51 "snack_parse.mly"
                                  ( (_1, List.rev _3) )
# 389 "snack_parse.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "snack_parse.mly"
                       ( Ref (_3, _2) )
# 397 "snack_parse.ml"
               : 'argument))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "snack_parse.mly"
                       ( Val (_3, _2) )
# 405 "snack_parse.ml"
               : 'argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arguments) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argument) in
    Obj.repr(
# 58 "snack_parse.mly"
                             ( _3 :: _1 )
# 413 "snack_parse.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argument) in
    Obj.repr(
# 59 "snack_parse.mly"
             ( [_1] )
# 420 "snack_parse.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "snack_parse.mly"
    ( [] )
# 426 "snack_parse.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 63 "snack_parse.mly"
                             ( Dvar (_1, _2) )
# 434 "snack_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'range_list) in
    Obj.repr(
# 64 "snack_parse.mly"
                                                          ( Darr (_1, _2, _4) )
# 443 "snack_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 67 "snack_parse.mly"
               ( _2 :: _1 )
# 451 "snack_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "snack_parse.mly"
    ( [] )
# 457 "snack_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "snack_parse.mly"
                              ( (_1, _3) )
# 465 "snack_parse.ml"
               : 'range))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'range_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 74 "snack_parse.mly"
                           ( _3 :: _1 )
# 473 "snack_parse.ml"
               : 'range_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 75 "snack_parse.mly"
          ( [_1] )
# 480 "snack_parse.ml"
               : 'range_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "snack_parse.mly"
               ( _2 :: _1 )
# 488 "snack_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "snack_parse.mly"
    ( [] )
# 494 "snack_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 83 "snack_parse.mly"
                        ( _1 )
# 501 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 84 "snack_parse.mly"
                          ( Ifthen (_2, List.rev _4) )
# 509 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 85 "snack_parse.mly"
                                     ( Ifthenelse (_2, List.rev _4, List.rev _6) )
# 518 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 86 "snack_parse.mly"
                           ( While (_2, List.rev _4) )
# 526 "snack_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 89 "snack_parse.mly"
                ( Read _2 )
# 533 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "snack_parse.mly"
               ( Write _2 )
# 540 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "snack_parse.mly"
                 ( WriteS _2 )
# 547 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 92 "snack_parse.mly"
                         ( Assign (_1, _3) )
# 555 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 93 "snack_parse.mly"
                                  ( Proccall (_1, List.rev _3) )
# 563 "snack_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "snack_parse.mly"
         ( Rexpr _1 )
# 570 "snack_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "snack_parse.mly"
          ( LId _1 )
# 577 "snack_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 100 "snack_parse.mly"
                                      ( Larray (_1, List.rev _3) )
# 585 "snack_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "snack_parse.mly"
                         ( _3 :: _1 )
# 593 "snack_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "snack_parse.mly"
         ( [_1] )
# 600 "snack_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "snack_parse.mly"
    ( [] )
# 606 "snack_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 108 "snack_parse.mly"
               ( Ebool _1 )
# 613 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 109 "snack_parse.mly"
              ( Eint _1 )
# 620 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 110 "snack_parse.mly"
                ( Efloat _1 )
# 627 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "snack_parse.mly"
          ( EId _1 )
# 634 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 112 "snack_parse.mly"
                                      ( Earray (_1, List.rev _3) )
# 642 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "snack_parse.mly"
                   ( Ebinop (_1, Op(Binop_left(Op_add)), _3) )
# 650 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "snack_parse.mly"
                    ( Ebinop (_1, Op(Binop_left(Op_sub)), _3) )
# 658 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "snack_parse.mly"
                  ( Ebinop (_1, Op(Binop_left(Op_mul)), _3) )
# 666 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "snack_parse.mly"
                  ( Ebinop (_1, Op(Binop_left(Op_div)), _3) )
# 674 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "snack_parse.mly"
                 ( Ebinop (_1, Op(Binop_left(Op_eq)), _3) )
# 682 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "snack_parse.mly"
                  ( Ebinop (_1, Op(Binop_left(Op_neq)), _3) )
# 690 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "snack_parse.mly"
                 ( Ebinop (_1, Op(Binop_left(Op_lt)), _3) )
# 698 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "snack_parse.mly"
                 ( Ebinop (_1, Op(Binop_left(Op_gt)), _3) )
# 706 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "snack_parse.mly"
                   ( Ebinop (_1, Op(Binop_left(Op_lteq)), _3) )
# 714 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "snack_parse.mly"
                   ( Ebinop (_1, Op(Binop_left(Op_gteq)), _3) )
# 722 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "snack_parse.mly"
                 ( Ebinop (_1, Op(Binop_left(Op_or)), _3) )
# 730 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "snack_parse.mly"
                  ( Ebinop (_1, Op(Binop_left(Op_and)), _3) )
# 738 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "snack_parse.mly"
             ( Eunop (Op(Unop_left(Op_not)), _2) )
# 745 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "snack_parse.mly"
                            ( Eunop (Op(Unop_left(Op_minus)), _2) )
# 752 "snack_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 128 "snack_parse.mly"
                       ( _2 )
# 759 "snack_parse.ml"
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
