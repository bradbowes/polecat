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
  (syntax-rules (@fun @var)
    [(_ (@fun ((@var id ty) ...) ret) body) (lambda (id ...) body)]))

(define-syntax @tuple
  (syntax-rules ()
    [(_ item ...) (list->vector (list item ...))]))

(define-syntax @vardecl
  (syntax-rules (@var)
    [(_ (@var id ty) value) (define id value)]))

(define-syntax @fundecl
  (syntax-rules (@fun @var)
    [(_ (name (@fun () ret)) body) (define (name) body)]
    [(_ (name (@fun ((@var id ty) ...) ret)) body) (define (name id ...) body)]))

(define-syntax @let
  (syntax-rules ()
    [(_ (def ...) body) ((lambda () def ... body))]))

(provide @tuple @num @bool @id @string @sym @app @lambda @fundecl @vardecl @let)
(provide (rename-out [module-begin #%module-begin]))
(provide #%top-interaction)

(provide read-syntax + - * / < > >= <= eq?
         #%app #%datum first rest)
(provide (rename-out [display print]
                     [displayln println]
                     [null? empty]
                     [cons ::]
                     [string-append ^]
                     [vector-ref @field]
                     [if @if]
                     [begin @begin]
                     [list @list]))
