#lang racket

(require "parser.rkt")

(define (read-syntax path port)
  (let*
      ((data (polecat-parse port))
       (module-data `(module polecat-module "polecat.rkt" ,@data)))
    (datum->syntax #f module-data)))

(define-syntax-rule (module-begin form ...)
  (#%module-begin
     form ...))

(provide (rename-out [module-begin #%module-begin]))

; (provide #%module-begin)
(provide read-syntax define letrec lambda if begin + - * / = < > >= <= 
         void #%app #%datum list first rest)
(provide (rename-out [display print] [displayln println] [null? empty]
                     [cons ::] [string-append ..]))
