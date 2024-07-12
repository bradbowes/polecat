#lang racket
(require polecat/parser)

(define (read-syntax path port)
  (let*
      ((data (polecat-parse port))
       (module-data `(module polecat-module polecat/reader ,@data)))
    (datum->syntax #f module-data)))

(define-syntax-rule (module-begin form ...)
  (#%module-begin
     form ...))

(define-syntax-rule (@tuple item ...)
    #(item ...))

(define-syntax-rule (@num num)
  num)

(define-syntax-rule (@bool bool)
  bool)

(define-syntax-rule (@id id)
  id)

(define-syntax-rule (@string string)
  string)

(define-syntax-rule (@sym sym)
  sym)

(define-syntax @app
  (syntax-rules ()
    [(_ f ()) (f)]
    [(_ f (arg ...)) (f arg ...)]))


(define-syntax @lambda
  (syntax-rules (@annot)
    [(_ (@annot ((@annot id ty) ...) ret) body) (lambda (id ...) body)]))

(define-syntax @define
  (syntax-rules (@annot)
    [(_ (@annot id ty) value) (define id value)]
    [(_ id value) (define id value)]))


(provide @tuple @num @bool @id @string @sym @app @lambda @define)
(provide (rename-out [module-begin #%module-begin]))
(provide #%top-interaction)
; (provide #%module-begin)
(provide read-syntax + - * / < > >= <= eq?
         #%app #%datum first rest)
(provide (rename-out [display print]
                     [displayln println]
                     [null? empty]
                     [cons ::]
                     [string-append ^]
                     [vector-ref @field]
                     [void @unit]
                     ;[define @define]
                     ;[lambda @lambda]
                     [letrec @letrec]
                     [if @if]
                     [begin @begin]
                     [list @list]))
