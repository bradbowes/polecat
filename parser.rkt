#lang racket

(require parser-tools/yacc)
(require "lexer.rkt")

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
   (tokens value-tokens tokens)
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
     [(begin seq end) `(begin ,@$2)]
     ]
    [vals
     [(val) (list $1)]
     [(val vals) (cons $1 $2)]]
    [val
     [(proto eq expr) (if (symbol? $1) (list $1 $3) `(,(car $1) (lambda ,@(cdr $1) ,$3)))]]
    [seq
     [(expr) (list $1)]
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
     [(disjunction or conjunction) `(or ,$1 ,$3)]]
    [conjunction
     [(comparison) $1]
     [(conjunction and comparison) `(and ,$1 ,$3)]]
    [comparison
     [(composition) $1]
     [(composition eq composition) `(= ,$1 ,$3)]
     [(composition gt composition) `(> ,$1 ,$3)]
     [(composition ge composition) `(>= ,$1 ,$3)]
     [(composition lt composition) `(< ,$1 ,$3)]
     [(composition le composition) `(<= ,$1 ,$3)]
     [(composition ne composition) `(<> ,$1 ,$3)]]
    [composition
     [(sum) $1]
     [(sum cons composition) `(:: ,$1 ,$3)]
     ]
    [sum
     [(product) $1]
     [(sum plus product) `(+ ,$1 ,$3)]
     [(sum minus product) `(- ,$1 ,$3)]]
    [product
     [(factor) $1]
     [(product times factor) `(* ,$1 ,$3)]
     [(product div factor) `(/ ,$1 ,$3)]
     [(product mod factor) `(% ,$1 ,$3)]]
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

(define (polecat-parse port)
   (parse (lambda () (lex port))))

(provide polecat-parse)

