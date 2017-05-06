{
open Snack_parse
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
rule token = parse
    [' ' '\t']                  { token lexbuf }     (* skip blanks *)
  | '#'[^'\n']*'\n'             { Lexing.new_line lexbuf ; token lexbuf } (* skip comments *)
  | '\n'                        { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?digit+ as lxm           { INT_CONST(int_of_string lxm) }
  | '-'?digit+'.'digit+ as lxm  { FLOAT_CONST(float_of_string lxm) }
  (* keywords *)
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "fi"    { FI }
  | "while" { WHILE }
  | "do"    { DO }
  | "od"    { OD }
  | "bool"  { BOOL }
  | "int"   { INT }
  | "float" { FLOAT }
  | "proc"  { PROC }
  | "ref"   { REF }
  | "val"   { VAL }
  | "end"   { END }
  | "true"  { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read"  { READ }
  | "write" { WRITE }
  | "and"   { AND }
  | "or"    { OR }
  | "not"   { NOT }
  | ":="    { ASSIGN }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '['     { LBRACKET }
  | ']'     { RBRACKET }
  | ".."    { RANGE }
  | '='     { EQ }
  | "!="    { NEQ }
  | '<'     { LT }
  | '>'     { GT }
  | "<="    { LTEQ }
  | ">="    { GTEQ }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { MUL }
  | '/'     { DIV }
  | ';'     { SEMICOLON }
  | ','     { COMMA }
  | '\"'    { QUOTE }
  | ident as lxm { IDENT lxm }
  | eof { EOF }
