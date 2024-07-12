#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrev name (:: (:or alphabetic #\_) (:* (:or alphabetic numeric #\_))))
(define-lex-abbrev string
  (:: "\""
      (:* (:or "\\\"" (complement (:: any-string "\"" any-string))))
      "\""))
(define (string-decode s)
  (define p (open-input-string (~a s)))
  (read p))

(define-tokens value-tokens (id num string bool sym typo))
(define-empty-tokens tokens
  (begin eof colon semicolon comma lparen rparen lbracket rbracket pipe mapsto
   cat cons dot eq gt lt ge le ne plus minus times div mod assign
   and case else end if in lambda let of or then type wildcard))

(define lex
  (lexer-src-pos
   [(eof) (token-eof)]
   [whitespace
    (return-without-pos (lex input-port))]
   [(:: "(*" (complement (:: any-string "*)" any-string)) "*)")
    (return-without-pos (lex input-port))]
   [":" (token-colon)]
   ["::" (token-cons)]
   [";" (token-semicolon)]
   ["," (token-comma)]
   ["." (token-dot)]
   ["^" (token-cat)]
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
   ["mod" (token-mod)]
   ["->" (token-mapsto)]
   [":=" (token-assign)]
   ["_" (token-wildcard)]
   ["and" (token-and)]
   ["begin" (token-begin)]
   ["case" (token-case)]
   ["else" (token-else)]
   ["end" (token-end)]
   ["false" (token-bool #f)]
   ["if" (token-if)]
   ["in" (token-in)]
   ["lambda" (token-lambda)]
   ["let" (token-let)]
   ["of" (token-of)]
   ["or" (token-or)]
   ["then" (token-then)]
   ["true" (token-bool #t)]
   ["type" (token-type)]
   [name (token-id (string->symbol lexeme))]
   [(:: "#" name) (token-sym (string->symbol (substring lexeme 1)))]
   [(:* numeric) (token-num (string->number lexeme))]
   [string (token-string (string-decode lexeme))]
   [any-char (token-typo lexeme)]
   ))

(provide lex tokens value-tokens position-line position-col)
