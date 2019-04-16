#lang play
(require "machine.rkt")
(print-only-errors #t) 
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <s-expr>} [: <type>] <expr>}
         | {<expr> <expr>}         
 
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))


(define (parse-type s-expr)
  (match s-expr
    ['() (error "Parse error")]
    [(quote Num) (TNum)]
    ['(Num) (TNum)]
    [(list arg-expr '-> res-expr) (TFun (parse-type arg-expr) (parse-type res-expr))]
    [(list arg-expr '->) (error "Parse error")]
    [(list '-> res-expr) (error "Parse error")]))
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? id?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))
;TODO funciones y with...

(define (prettify t-expr)
  (match t-expr
  [(TNum) 'Num]
  [(TFun t-arg t-ret) (list (prettify t-arg) '-> (prettify t-ret))]))
(define (deBruijn expr)#f)

(define (compile expr) #f)

(define (typeof expr) #f)

(define (typecheck s-expr) #f)

(define (typed-compile s-expr) #f)