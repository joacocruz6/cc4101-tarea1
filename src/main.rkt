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
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body) ;tambien se usa en la p3
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))
#|
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Type
  (TNum)
  (TFun arg ret))
#|
Definición del ADT ambiente de variables con sus tipos
|#
(deftype TypeEnv
  (mtEnv)
  (tpEnv id type next))
(define empty-type-env (mtEnv))
(define extend-type-env tpEnv)
#|
lookup-type-env ::= id TypeEnv -> Type (o error)
Busca el tipo de la variable x en el ambiente, si no esta tira error de identificador libre.
|#
(define (lookup-type-env x env)
  (match env
    [(mtEnv) (error "Type error: free identifier:" x)]
    [(tpEnv id type next)
     (if (equal? x id)
         type
         (lookup-type-env x next))]))


#|
parse-type ::= List[Symbol] -> Type (o error)
Parsea el tipo de el contrato de una función. Puede generar error de tipos
si al declarar una funcion uno de los dos no es declarado.
|#
(define (parse-type s-expr)
  (match s-expr
    ['() (error "Parse error")]
    [(quote Num) (TNum)]
    ['(Num) (TNum)]
    [(list arg-expr '-> res-expr) (TFun (parse-type arg-expr) (parse-type res-expr))]
    [(list arg-expr '->) (error "Parse error")]
    [(list '-> res-expr) (error "Parse error")]))

#|
parse ::= List[Symbol] -> Expr
Parsea una lista de simbolos a un AST del lenguaje con tipos.
|#
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)] ;parseo de numeros
    [(? symbol?) (id s-expr)] ;parseo de identificadores
    [(list '+ l r) (add (parse l) (parse r))] ;parseo de suma
    [(list '- l r) (sub (parse l) (parse r))] ;parseo de resta
    [(list 'fun {list args ': args-type} ': ret-type body) ;parseo de funciones
     (fun args (parse-type args-type) (parse body) (parse-type ret-type))]
    [(list 'fun {list args ': args-type} body) ;parseo de funciones sin retorno
     (fun args (parse-type args-type) (parse body) #f)]
    [(list 'with {list args ': args-type expr-value} body) ;parseo del with
     (app (parse {list 'fun {list args ': args-type} body}) (parse expr-value))]
    [(list expr1 expr2) (app (parse expr1) (parse expr2))]))
#|
prettify ::= Type -> List[Symbol] (o un symbolo si es TNum)
Dado un tipo del lenguaje se transforma en sintaxis concreta.
|#
(define (prettify t-expr)
  (match t-expr
  [(TNum) 'Num]
  [(TFun t-arg t-ret) (list (prettify t-arg) '-> (prettify t-ret))]))

#|
typeof-ext ::= Expr TypeEnv -> Type (o error)
Dada una expresion parseada y un ambiente, determina el tipo de esa expresión.
Puede generar error de tipos en caso de que haya en el programa escrito
|#
(define (typeof-ext expr [env empty-type-env])
  (match expr
    [(num n) (TNum)]
    [(id x) (lookup-type-env x env)]
    [(add l r) (def type-right (typeof-ext r env))
                (def type-left (typeof-ext l env))
                (if (and (equal? type-right (TNum)) (equal? type-left (TNum)))
                   (TNum)
                   (if (not (equal? type-right (TNum)))
                       (error "Type error in expression + position 2: expected Num found" (prettify type-right))
                       (error "Type error in expression + position 1: expected Num found" (prettify type-left))))]
    [(sub l r) (def type-right (typeof-ext r env))
                (def type-left (typeof-ext l env))
                (if (and (equal? type-right (TNum)) (equal? type-left (TNum)))
                   (TNum)
                   (if (not (equal? type-right (TNum)))
                       (error "Type error in expression - position 2: expected Num found" (prettify type-right))
                       (error "Type error in expression - position 1: expected Num found" (prettify type-left))))]
    [(fun args args-type body ret-type)
     (def body-type (typeof-ext body (extend-type-env args args-type env)))
     (if (or (equal? ret-type body-type) (equal? ret-type #f))
         (TFun args-type body-type)
         (error "Type error in expression fun position 1: expected" (prettify ret-type) 'found (prettify body-type)))] ;TODO,el error que se debe lanzar y ver el caso cuando ret-type vale falso.
    [(app expr1 expr2) ;aplicacion de funciones
     (match expr1 ;veo pattern maching de la primera expresion
       [(id x) ;si es un id, lo busco en el ambiente y debe ser tipo funcion
        (match (lookup-type-env x env)
          [(TNum) (error "Type error in expression app position 1: expected (T -> S) found Num")]
          [(TFun args-type ret-type) (if (equal? args-type (typeof-ext expr2 env))
                                                 ret-type
                                                 (error "Type error in expression app position 2: expected" (prettify args-type) 'found (prettify (typeof-ext expr2 env))))])]; si x es TNum, esta mal, si es un TFun veo que el tipo de de args sea el tipo de expr2 y retorno el tipo de su body.
       [(fun args args-type body #f) ;si es tipo funcion sin retorno, veo si los argumentos calzan y devuelvo el typo funcion con el tipo de su body
        (if (equal? args-type (typeof-ext expr2 env))
            (typeof-ext body (extend-type-env args args-type env))
            (error "Type error in expression app position 2: expected" (prettify args-type) 'found (prettify (typeof-ext expr2 env))))] ; veo si el tipo de argumentos es el mismo que el de los parametros y extiendo el ambiente
       [(fun args args-type body body-type) ;si tiene tipo funcion anotado, veo si los argumentos y el body calzan
        (if (equal? args-type (typeof-ext expr2 env))
            (if (equal? body-type (typeof-ext body (extend-type-env args args-type env)))
                body-type
                (error "Type error in expression app position 1: expected" (prettify body-type) 'found (prettify (typeof-ext body env))))
            (error "Type error in expression app position 2: expected" (prettify args-type) 'found (prettify (typeof-ext expr2 env)))
            )]       
       [ _ (error "Type error in expression app position 1: expected (T -> S) found" (prettify (typeof-ext expr1 env)))] ;cualquier otra cosa esta mala
    )]))
#|
typeof ::= Expr -> Type (o error)
Dada una expresion parseada, determina el tipo de esa expresión.
Puede generar error de tipos en caso de encontrarlo.
|#
(define (typeof expr)
  (typeof-ext expr))
#|
typecheck ::= List[Symbol] -> List[Symbol] (o error)
Dada una lista de simbolos que representan un programa en el lenguaje definido, se entrega una lista de symbolos.
Puede generar error de tipos en caso que se de en el programa escrito.
|#
(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))


;;;;;;;;;
;;;P3;;;;
;;;;;;;;;

#|
Declaración sobre el ambiente de bruijn
|#
(deftype Env
  (BruijnEnv id next))
(define empty-bruijn-env (mtEnv))
(define extend-bruijn-env BruijnEnv)
#|
lookup-bruijn-env ::= <id> Env -> <acc> (o error)
Busca en el ambiente de bruijn index el id x, y da la expression acc con
su indice asociado en la primera aparición. Genera error al no encontrar el identificadore en el ambiente
asociandolo a un identificador libre.
|#
(define (lookup-bruijn-env x env [pos 0])
  (match env
    [(mtEnv) (error "Free identifier:" x)]
    [(BruijnEnv id next)
     (if (equal? id x)
         (acc pos)
         (lookup-bruijn-env x next (+ pos 1)))]))
#|
deBruijn-ext :: Expr Env -> Expr (o error) 
Dada un expresion parseada y un ambiente genera la representacion con indices deBruijn de la expresion.
Puede generar error de identificador libre en caso de que encuentre uno.
|#
(define (deBruijn-ext expr [env empty-bruijn-env])
  (match expr
    [(num n) (num n)]
    [(id x) (lookup-bruijn-env x env)]
    [(add l r) (add (deBruijn-ext l env) (deBruijn-ext r env))]
    [(sub l r) (sub (deBruijn-ext l env) (deBruijn-ext r env))]
    [(fun args args-type body body-type) (fun-db (deBruijn-ext body (extend-bruijn-env args env)))]
    [(app (fun args args-type body body-type) expr2) (app (fun-db (deBruijn-ext body (extend-bruijn-env args env))) (deBruijn-ext expr2 env))]
    [(app (id x) expr2) (app (lookup-bruijn-env x env) (deBruijn-ext expr2 env))]))
#|
deBruijn :: Expr -> Expr (error)
Dada una expresion parseada genera la representación con indices deBruijn de esa expresion. Genera error de identificador libre
cuando sea necesario.
|#
(define (deBruijn expr)
  (deBruijn-ext expr))
#|
compile :: Expr -> List[Instructions]
Toma una lista del lenguajes con indices deBruijn ya puestos y la convierte
a una lista del set de instrucciones de maquina a utilizar en esta tarea.
|#
(define (compile expr)
  (match expr
    [(num n) (list (INT-CONST n))]
    [(acc n) (list (ACCESS n))]
    [(add l r) (append (compile r) (compile l) (list (ADD)))]
    [(sub l r) (append (compile r) (compile l) (list (SUB)))]
    [(fun-db body) (list (CLOSURE (append (compile body) (list (RETURN)))))]
    [(app expr1 expr2) (append (compile expr2) (compile expr1) (list (APPLY)))]))
#|
typed-compile :: List[Symbol] -> List[Instructions] (o error)
Dada una lista de symbolos en el lenguaje, se genera una lista de instrucciones de la maquina dada por enunciado.
Esto además parsea al lenguaje y comprueba los tipos de las expresiones en pasos intermedios.
|#
(define (typed-compile s-expr)
  (def parsed-expr (parse s-expr))
  (def type-check-expr (typecheck s-expr))
  (compile (deBruijn parsed-expr)))