#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(define-lex-abbrev name (:: (:or alphabetic #\_) (:* (:or alphabetic numeric #\_))))
(define-lex-abbrev string
  (:: "\""
      (:* (:or "\\\"" (complement (:: any-string "\"" any-string))))
      "\""))
(define (string-decode s)
  (define p (open-input-string (~a s)))
  (read p))

(define-tokens basic-tokens (id num string sym typo))
(define-empty-tokens punct-tokens
  (eof colon semicolon comma lparen rparen lbracket rbracket pipe mapsto))
(define-empty-tokens operator-tokens
  (dot eq gt lt ge le ne plus minus times div mod assign))
(define-empty-tokens keyword-tokens
  (and case else end if in lambda let of or then type wildcard))

(define lex
  (lexer-src-pos
   [(eof) (token-eof)]
   [whitespace
    (return-without-pos (lex input-port))]
   [(:: "(*" (complement (:: any-string "*)" any-string)) "*)")
    (return-without-pos (lex input-port))]
   [":" (token-colon)]
   [";" (token-semicolon)]
   ["," (token-comma)]
   ["." (token-dot)]
   ["=" (token-eq)]
   [">" (token-gt)]
   ["<" (token-lt)]
   [">=" (token-ge)]
   ["<=" (token-le)]
   ["<>" (token-ne)]
   ["(" (token-lparen)]
   [")" (token-rparen)]
   ["[" (token-lbracket)]
   ["]" (token-rbracket)]
   ["|" (token-pipe)]
   ["+" (token-plus)]
   ["-" (token-minus)]
   ["*" (token-times)]
   ["/" (token-div)]
   ["%" (token-mod)]
   ["->" (token-mapsto)]
   [":=" (token-assign)]
   ["_" (token-wildcard)]
   ["and" (token-and)]
   ["case" (token-case)]
   ["else" (token-else)]
   ["end" (token-end)]
   ["if" (token-if)]
   ["in" (token-in)]
   ["lambda" (token-lambda)]
   ["let" (token-let)]
   ["of" (token-of)]
   ["or" (token-or)]
   ["then" (token-then)]
   ["type" (token-type)]
   [name (token-id (string->symbol lexeme))]
   [(:: "#" name) (token-sym (string->symbol (substring lexeme 1)))]
   [(:* numeric) (token-num (string->number lexeme))]
   [string (token-string (string-decode lexeme))]
   [any-char (token-typo lexeme)]
   ))

(define parse
  (parser
   (src-pos)
   (start prog)
   (end eof)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (if tok-ok?
                (display
                 (format "error ~a ~s line: ~a col: ~a\n\n"
                         tok-name tok-value
                         (position-line start-pos)
                         (position-col start-pos)))
                (display "error\n"))))
   (tokens basic-tokens punct-tokens operator-tokens keyword-tokens)
   (grammar
    [prog
     [(defs) `(,@$1 (main))]]
    [defs
     [(def) (list $1)]
     [(def defs) (cons $1 $2)]]
    [def
     [(proto eq expr) (if (symbol? $1) `(define ,$1 ,$3) `(define ,(car $1) (lambda ,@(cdr $1) ,$3)))]]
    [proto
     [(wildcard) (gensym "#@")]
     [(arg) $1]
     [(id signature) (list $1 $2)]]
    [signature
     ;[(args) (cons $1 '?)]
     ;[(args colon id) (cons $1 $3)]]
     [(args) $1]
     [(args colon id) $1]]
    [args
     [(lparen rparen) '()]
     [(lparen arglist rparen) $2]]
    [arglist
     [(arg) (list $1)]
     [(arg comma arglist) (cons $1 $3)]]
    [arg
     ;[(id) (cons $1 '?)]
     ;[(id colon id) (cons $1 $3)]]
     [(id) $1]
     [(id colon id) $1]]
    [expr
     [(disjunction) $1]
     [(if expr then expr else expr) `(if ,$2 ,$4 ,$6)]
     [(let vals in expr) `(letrec ,$2 ,$4)]
     [(lambda signature mapsto expr) `(lambda ,$2 ,$4)]
     [(case expr of clauses end) `(case ,$2 ,$4)]
     [(lparen seq rparen) `(begin ,@$2)]
     ]
    [vals
     [(val) (list $1)]
     [(val vals) (cons $1 $2)]]
    [val
     [(proto eq expr) (if (symbol? $1) (list $1 $3) `(,(car $1) (lambda ,@(cdr $1) ,$3)))]]
    [seq
     [(expr semicolon expr) (list $1 $3)]
     [(expr semicolon seq) (cons $1 $3)]]
    [clauses
     [(clause) (list $1)]
     [(clause pipe clauses) (cons $1 $3)]]
    [clause
     [(constructor mapsto expr) (list $1 $3)]]
    [constructor
     [(expr) $1]
     [(expr id) `(bind ,$1, $2)]
     [(wildcard) '()]]
    [disjunction
     [(conjunction) $1]
     [(conjunction or disjunction) `(or ,$1 ,$3)]]
    [conjunction
     [(boolean) $1]
     [(boolean and conjunction) `(and ,$1 ,$3)]]
    [boolean
     [(sum) $1]
     [(sum eq sum) `(= ,$1 ,$3)]
     [(sum gt sum) `(> ,$1 ,$3)]
     [(sum ge sum) `(>= ,$1 ,$3)]
     [(sum lt sum) `(< ,$1 ,$3)]
     [(sum le sum) `(<= ,$1 ,$3)]
     [(sum ne sum) `(<> ,$1 ,$3)]]
    [sum
     [(product) $1]
     [(product plus sum) `(+ ,$1 ,$3)]
     [(product minus sum) `(- ,$1 ,$3)]]
    [product
     [(factor) $1]
     [(factor times product) `(* ,$1 ,$3)]
     [(factor div product) `(/ ,$1 ,$3)]
     [(factor mod product) `(% ,$1 ,$3)]]
    [factor
     [(num) $1]
     [(id) $1]
     [(lparen rparen) '(void)]
     [(string) $1]
     [(sym) `(sym ,$1)]
     [(id lparen exprs rparen) (cons $1 $3)]
     [(lparen expr rparen) $2]
     [(lbracket exprs rbracket) `(list ,@$2)]
     [(factor dot id) `(member ,$1 ,$3)]
     [(factor lbracket expr rbracket) `(index ,$1 ,$3)] ]
    [exprs
     [() '()]
     [(exprlist) $1]]
    [exprlist
     [(expr) (list $1)]
     [(expr comma exprlist) (cons $1 $3)]]
    )))

(define (read-syntax path port)
  (let*
      ((data (parse (lambda () (lex port))))
       (module-data `(module polecat-module "polecat.rkt" ,@data)))
    (datum->syntax #f module-data)))

(define-syntax-rule (module-begin form ...)
  (#%module-begin
     form ...))

(provide (rename-out [module-begin #%module-begin]))
(provide read-syntax define letrec lambda if begin + - * / = < > >= <= void #%app #%datum list first rest cons)
(provide (rename-out [display print] [displayln println] [null? empty]))





