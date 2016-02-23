type token =
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EQ
  | LEQ
  | LT
  | GEQ
  | GT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | ASSG
  | COMP
  | SKIP
  | IF
  | ELSE
  | WHILE
  | PRINT
  | INPUT
  | VAR of (string)
  | NUMBER of (int)
  | EOL

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

open Ast
# 39 "parser.ml"
let yytransl_const = [|
  257 (* TRUE *);
  258 (* FALSE *);
  259 (* AND *);
  260 (* OR *);
  261 (* NOT *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* MOD *);
  267 (* EQ *);
  268 (* LEQ *);
  269 (* LT *);
  270 (* GEQ *);
  271 (* GT *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* LBRACE *);
  275 (* RBRACE *);
  276 (* ASSG *);
  277 (* COMP *);
  278 (* SKIP *);
  279 (* IF *);
  280 (* ELSE *);
  281 (* WHILE *);
  282 (* PRINT *);
  283 (* INPUT *);
  286 (* EOL *);
    0|]

let yytransl_block = [|
  284 (* VAR *);
  285 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000\002\000\001\000\003\000\002\000\005\000\
\007\000\007\000\003\000\002\000\001\000\003\000\005\000\007\000\
\007\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\018\000\019\000\000\000\000\000\001\000\000\000\
\000\000\003\000\014\000\037\000\038\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\006\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\023\000\024\000\000\000\033\000\035\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\015\000\000\000\000\000\009\000\016\000\010\000\017\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\032\000\033\000"

let yysindex = "\007\000\
\194\255\000\000\194\255\000\000\253\254\000\255\138\255\001\255\
\000\000\248\254\037\255\185\255\004\255\036\255\036\255\138\255\
\138\255\000\000\000\000\000\000\227\255\138\255\000\000\194\255\
\194\255\000\000\000\000\000\000\000\000\036\255\036\255\002\255\
\217\255\008\255\251\254\183\255\138\255\138\255\138\255\138\255\
\138\255\227\255\000\000\000\000\000\000\025\255\171\255\036\255\
\036\255\194\255\138\255\138\255\138\255\138\255\138\255\194\255\
\000\000\251\254\251\254\000\000\000\000\251\254\000\000\000\000\
\000\000\033\255\057\255\227\255\227\255\227\255\227\255\227\255\
\000\000\000\000\194\255\194\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\255\000\000\000\000\060\255\
\064\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\063\255\000\000\000\000\000\000\000\000\000\000\
\000\000\175\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\085\255\107\255\000\000\000\000\129\255\000\000\000\000\
\000\000\000\000\000\000\051\255\152\255\154\255\157\255\159\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\047\000\207\255\220\255\243\255\249\255"

let yytablesize = 237
let yytable = "\021\000\
\066\000\034\000\039\000\040\000\048\000\049\000\073\000\001\000\
\035\000\036\000\048\000\049\000\014\000\067\000\042\000\015\000\
\045\000\046\000\050\000\074\000\022\000\023\000\027\000\047\000\
\056\000\077\000\079\000\048\000\049\000\058\000\059\000\060\000\
\061\000\062\000\064\000\065\000\028\000\029\000\078\000\080\000\
\030\000\063\000\016\000\068\000\069\000\070\000\071\000\072\000\
\005\000\013\000\012\000\031\000\012\000\028\000\028\000\012\000\
\075\000\024\000\026\000\005\000\000\000\012\000\018\000\019\000\
\020\000\026\000\026\000\028\000\026\000\026\000\043\000\044\000\
\026\000\026\000\026\000\026\000\026\000\026\000\004\000\026\000\
\076\000\026\000\007\000\026\000\000\000\000\000\026\000\021\000\
\021\000\004\000\021\000\021\000\026\000\007\000\021\000\021\000\
\021\000\021\000\021\000\021\000\000\000\021\000\000\000\021\000\
\000\000\021\000\000\000\000\000\021\000\022\000\022\000\000\000\
\022\000\022\000\021\000\000\000\022\000\022\000\022\000\022\000\
\022\000\022\000\000\000\022\000\000\000\022\000\000\000\022\000\
\000\000\000\000\022\000\025\000\025\000\000\000\025\000\025\000\
\022\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\
\016\000\025\000\000\000\025\000\000\000\025\000\000\000\000\000\
\025\000\017\000\029\000\029\000\030\000\030\000\025\000\031\000\
\031\000\032\000\032\000\000\000\018\000\019\000\020\000\000\000\
\029\000\000\000\030\000\000\000\000\000\031\000\000\000\032\000\
\037\000\038\000\039\000\040\000\041\000\051\000\052\000\053\000\
\054\000\055\000\000\000\057\000\037\000\038\000\039\000\040\000\
\041\000\011\000\000\000\011\000\000\000\000\000\011\000\057\000\
\000\000\000\000\003\000\000\000\011\000\025\000\004\000\005\000\
\000\000\006\000\007\000\003\000\008\000\000\000\000\000\004\000\
\005\000\000\000\006\000\007\000\000\000\008\000\037\000\038\000\
\039\000\040\000\041\000\051\000\052\000\053\000\054\000\055\000\
\037\000\038\000\039\000\040\000\041\000"

let yycheck = "\007\000\
\050\000\015\000\008\001\009\001\003\001\004\001\056\000\001\000\
\016\000\017\000\003\001\004\001\016\001\050\000\022\000\016\001\
\030\000\031\000\017\001\056\000\020\001\030\001\019\001\031\000\
\017\001\075\000\076\000\003\001\004\001\037\000\038\000\039\000\
\040\000\041\000\048\000\049\000\001\001\002\001\075\000\076\000\
\005\001\017\001\007\001\051\000\052\000\053\000\054\000\055\000\
\019\001\003\000\019\001\016\001\021\001\003\001\004\001\024\001\
\024\001\021\001\012\000\030\001\255\255\030\001\027\001\028\001\
\029\001\003\001\004\001\017\001\006\001\007\001\024\000\025\000\
\010\001\011\001\012\001\013\001\014\001\015\001\019\001\017\001\
\024\001\019\001\019\001\021\001\255\255\255\255\024\001\003\001\
\004\001\030\001\006\001\007\001\030\001\030\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\017\001\255\255\019\001\
\255\255\021\001\255\255\255\255\024\001\003\001\004\001\255\255\
\006\001\007\001\030\001\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\255\255\019\001\255\255\021\001\
\255\255\255\255\024\001\003\001\004\001\255\255\006\001\007\001\
\030\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\007\001\017\001\255\255\019\001\255\255\021\001\255\255\255\255\
\024\001\016\001\003\001\004\001\003\001\004\001\030\001\003\001\
\004\001\003\001\004\001\255\255\027\001\028\001\029\001\255\255\
\017\001\255\255\017\001\255\255\255\255\017\001\255\255\017\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\006\001\007\001\008\001\009\001\
\010\001\019\001\255\255\021\001\255\255\255\255\024\001\017\001\
\255\255\255\255\018\001\255\255\030\001\021\001\022\001\023\001\
\255\255\025\001\026\001\018\001\028\001\255\255\255\255\022\001\
\023\001\255\255\025\001\026\001\255\255\028\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\006\001\007\001\008\001\009\001\010\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  EQ\000\
  LEQ\000\
  LT\000\
  GEQ\000\
  GT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  ASSG\000\
  COMP\000\
  SKIP\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  PRINT\000\
  INPUT\000\
  EOL\000\
  "

let yynames_block = "\
  VAR\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'comlist) in
    Obj.repr(
# 34 "parser.mly"
                ( _1 )
# 240 "parser.ml"
               : Ast.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comlist) in
    Obj.repr(
# 38 "parser.mly"
                     ( Comp (_1, _3) )
# 248 "parser.ml"
               : 'comlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'combr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comlist) in
    Obj.repr(
# 39 "parser.mly"
                  ( Comp (_1, _2) )
# 256 "parser.ml"
               : 'comlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'com) in
    Obj.repr(
# 40 "parser.mly"
             ( _1 )
# 263 "parser.ml"
               : 'comlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'com) in
    Obj.repr(
# 41 "parser.mly"
        ( _1 )
# 270 "parser.ml"
               : 'comlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'combr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comlist) in
    Obj.repr(
# 42 "parser.mly"
                       ( Comp (_1, _3) )
# 278 "parser.ml"
               : 'comlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'combr) in
    Obj.repr(
# 43 "parser.mly"
               ( _1 )
# 285 "parser.ml"
               : 'comlist))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'com) in
    Obj.repr(
# 47 "parser.mly"
                                 ( While (_3, _5) )
# 293 "parser.ml"
               : 'com))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bexp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'com) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'com) in
    Obj.repr(
# 48 "parser.mly"
                                       ( Cond (_3, _5, _7) )
# 302 "parser.ml"
               : 'com))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bexp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'combr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'com) in
    Obj.repr(
# 49 "parser.mly"
                                         ( Cond (_3, _5, _7) )
# 311 "parser.ml"
               : 'com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 50 "parser.mly"
                  ( Assg (_1, _3) )
# 319 "parser.ml"
               : 'com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 51 "parser.mly"
               ( Print _2 )
# 326 "parser.ml"
               : 'com))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
         ( Skip )
# 332 "parser.ml"
               : 'com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'comlist) in
    Obj.repr(
# 56 "parser.mly"
                          ( _2 )
# 339 "parser.ml"
               : 'combr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'combr) in
    Obj.repr(
# 57 "parser.mly"
                                   ( While (_3, _5) )
# 347 "parser.ml"
               : 'combr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bexp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'com) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'combr) in
    Obj.repr(
# 58 "parser.mly"
                                         ( Cond (_3, _5, _7) )
# 356 "parser.ml"
               : 'combr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bexp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'combr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'combr) in
    Obj.repr(
# 59 "parser.mly"
                                           ( Cond (_3, _5, _7) )
# 365 "parser.ml"
               : 'combr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
        ( Var _1 )
# 372 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
           ( Number _1 )
# 379 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    Obj.repr(
# 65 "parser.mly"
                       ( _2 )
# 386 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 66 "parser.mly"
                   ( Plus (_1, _3) )
# 394 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 67 "parser.mly"
                    ( Minus (_1, _3) )
# 402 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 68 "parser.mly"
                    ( Times (_1, _3) )
# 410 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 69 "parser.mly"
                  ( Div (_1, _3) )
# 418 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 70 "parser.mly"
                  ( Mod (_1, _3) )
# 426 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 71 "parser.mly"
               ( Minus (Number 0, _2) )
# 433 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
          ( Input )
# 439 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 76 "parser.mly"
                 ( Eq (_1, _3) )
# 447 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 77 "parser.mly"
                  ( Leq (_1, _3) )
# 455 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 78 "parser.mly"
                 ( Lt (_1, _3) )
# 463 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 79 "parser.mly"
                  ( Leq (_3, _1) )
# 471 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 80 "parser.mly"
                 ( Lt (_3, _1) )
# 479 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 81 "parser.mly"
                       ( _2 )
# 486 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 82 "parser.mly"
             ( Not _2 )
# 493 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 83 "parser.mly"
                  ( And (_1, _3) )
# 501 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 84 "parser.mly"
                 ( Or (_1, _3) )
# 509 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
         ( True )
# 515 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
          ( False )
# 521 "parser.ml"
               : 'bexp))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.com)
;;
# 89 "parser.mly"

# 548 "parser.ml"
