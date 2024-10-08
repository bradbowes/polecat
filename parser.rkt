#lang racket

(require parser-tools/yacc)
(require polecat/lexer)

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
   ;(debug "polecat-parser.debug")
   (tokens value-tokens tokens)
   (grammar
    [prog
     [(defs) (cons '@prog $1)]]
    [defs
      [(def) (list $1)]
      [(def defs) (cons $1 $2)]]
    [def
      [(id signature eq expr) `(@fundecl ,(list $1 $2) ,$4)]
      [(param eq expr) `(@vardecl ,$1 ,$3)]
      [(wildcard eq expr) $3]]
    [proto
     [(param) $1]
     [(id signature) (list $1 $2)]]
    [signature
     [(params) `(@fun ,$1 ?)]
     [(params colon fnty) `(@fun ,$1 ,$3)]]
    [params
     [(lparen rparen) '()]
     [(lparen paramlist rparen) $2]]
    [paramlist
     [(param) (list $1)]
     [(param comma paramlist) (cons $1 $3)]]
    [param
     [(id) `(@var ,$1 ?)]
     [(id colon fnty) `(@var ,$1 ,$3)]]
    [fnty
     [(ty) $1]
     [(ty mapsto fnty) `(@fnty ,$1 ,$3)]]
    [ty
     [(id) `(@tyid ,$1)]
     [(tyvar) `(@tyvar ,$1)]
     [(id of ty) `(@tydat ,$1 ,$3)]
     [(lparen rparen) 'unit]
     [(lparen tupty rparen) `(@tupty ,@$2)]]
    [tupty
     [(fnty comma fnty) (list $1 $3)]
     [(fnty comma tupty) (cons $1 $3)]]
    [expr
     [(disjunction) $1]
     [(if expr then expr else expr) `(@if ,$2 ,$4 ,$6)]
     [(let defs in expr) `(@let ,$2 ,$4)]
     [(lambda signature eq expr) `(@lambda ,$2 ,$4)]
     [(case expr of clauses end) `(@case ,$2 ,$4)]
     [(begin seq end) `(@begin ,@$2)]]
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
     [(composition eq composition) `(@app (@id eq?) (,$1 ,$3))]
     [(composition gt composition) `(@app (@id >) (,$1 ,$3))]
     [(composition ge composition) `(@app (@id >=) (,$1 ,$3))]
     [(composition lt composition) `(@app (@id <) (,$1 ,$3))]
     [(composition le composition) `(@app (@id <=) (,$1 ,$3))]
     [(composition ne composition) `(@app (@id <>) (,$1 ,$3))]]
    [composition
     [(sum) $1]
     [(sum cons composition) `(@app (@id ::) (,$1 ,$3))]]
    [sum
     [(product) $1]
     [(sum plus product) `(@app (@id +) (,$1 ,$3))]
     [(sum minus product) `(@app (@id -) (,$1 ,$3))]
     [(sum cat product) `(@app (@id ^) (,$1 ,$3))]]
    [product
     [(factor) $1]
     [(product times factor) `(@app (@id *) (,$1 ,$3))]
     [(product div factor) `(@app (@id /) (,$1 ,$3))]
     [(product mod factor) `(@app (id mod) (,$1 ,$3))]]
    [factor
     [(atom) $1]
     [(minus atom) `(@minus ,$2)]
     [(factor lparen exprs rparen) `(@app ,$1 ,$3)]
     [(lparen tuple rparen) `(@tuple ,@$2)]
     [(lbracket exprs rbracket) `(@list ,@$2)]
     ;[(factor dot id) `(member ,$1 ,$3)]
     [(factor dot num) `(@field ,$1 ,(- $3 1))]
     ;[(factor lbracket expr rbracket) `(@index ,$1 ,$3)]
     ]
    [atom
     [(num) `(@num ,$1)]
     [(bool) `(@bool ,$1)]
     [(id) `(@id ,$1)]
     [(lparen rparen) '(@unit)]
     [(string) `(@string ,$1)]
     [(sym) `(@sym ,$1)]
     [(lparen expr rparen) $2]]
    [exprs
     [() '()]
     [(exprlist) $1]]
    [exprlist
     [(expr) (list $1)]
     [(expr comma exprlist) (cons $1 $3)]]
    [tuple
     [(expr comma expr) (list $1 $3)]
     [(expr comma tuple) (cons $1 $3)]]
    )))

(define (polecat-parse port)
   (parse (lambda () (lex port))))

(provide polecat-parse)

