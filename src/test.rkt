#lang play
;;Alumno : Joaquin Cruz
(require "main.rkt")
(require "machine.rkt")
;;emppty-type-env
(test empty-type-env (mtEnv))
;;extend-type-env
(test (extend-type-env 'x (TNum) (extend-type-env 'y (TFun (TNum) (TNum)) empty-type-env)) (tpEnv 'x (TNum) (tpEnv 'y (TFun (TNum) (TNum)) (mtEnv))))

;;lookup-type-env
(test (lookup-type-env 'x (extend-type-env 'x (TNum) empty-type-env)) (TNum))
(test (lookup-type-env 'x  (extend-type-env 'x (TFun (TNum) (TNum)) empty-type-env)) (TFun (TNum) (TNum)))
(test/exn (lookup-type-env 'x empty-type-env) "Type error: free identifier: x")
(test/exn (lookup-type-env 'y (extend-type-env 'x (TNum) (extend-type-env 'z (TFun (TNum) (TNum)) empty-type-env))) "Type error: free identifier: y")

;; parse-type
(test (parse-type '{Num}) (TNum))
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test (parse-type '{{Num -> Num} -> Num}) (TFun (TFun (TNum) (TNum)) (TNum)))
(test (parse-type '{{Num -> Num} -> {Num -> Num}}) (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum))))
(test/exn (parse-type '{Num -> }) "Parse error")
(test/exn (parse-type '{ -> Num}) "Parse error")
(test/exn (parse-type '{{Num -> Num} -> {Num -> }}) "Parse error")

;; parse
(test (parse '3) (num 3))
(test (parse '{+ 1 3}) (add (num 1) (num 3)))
(test (parse '{- 1 3}) (sub (num 1) (num 3)))
(test (parse '{with {y : Num 2} {+ x y}}) (app (fun 'y (TNum) (add (id 'x) (id 'y)) #f) (num 2)))
(test (parse '{with {y : Num 2} y}) (app (fun 'y (TNum) (id 'y) #f) (num 2)))
(test (parse '{y 2}) (app (id 'y) (num 2)))
(test (parse '{{fun {x : Num} : Num {+ x 10}} 2}) (app (fun 'x (TNum) (add (id 'x) (num 10)) (TNum)) (num 2)))
(test (parse '{fun {x : Num} : Num {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)))
(test (parse '{with {y : Num 2} {+ x y}}) (app (fun 'y (TNum) (add (id 'x) (id 'y)) #f) (num 2)))
(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))
(test (parse '{{fun {x : Num} : Num {+ x 1}} 2}) (app (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)) (num 2)))
(test (parse '{with {y : {Num -> Num} {fun {x : Num} : Num {+ x x}}} {y 2}}) (app (fun 'y (TFun (TNum) (TNum)) (app (id 'y) (num 2)) #f) (fun 'x (TNum) (add (id 'x) (id 'x)) (TNum))))

;;prettify tests
(test (prettify (TNum)) 'Num)
(test (prettify (TFun (TNum) (TNum))) '{Num -> Num})
(test (prettify (TFun (TFun (TNum) (TNum)) (TNum))) '{{Num -> Num} -> Num})
(test (prettify (TFun (TNum) (TFun (TNum) (TNum)))) '{Num -> {Num -> Num}})
;;typeof-ext
(test (typeof-ext (parse '3)) (TNum)) ;Test para verificar tipo de los numeros
(test (typeof-ext (parse '{+ 1 3})) (TNum)) ;Test para verificar tipo de la adicion numerica
(test (typeof-ext (parse '{fun {x : Num} : Num 5})) (TFun (TNum) (TNum))) ;Test para verificar 
(test (typeof-ext (parse '{{fun {x : Num} x} 1})) (TNum)) ;test para verificar correctitud de aplicaciones
(test (typeof-ext (parse '{{fun {f : {Num -> Num}} {f 12}} {fun {x : Num} {+ x x}}})) (TNum))
(test/exn (typeof-ext (parse 'y)) "Type error: free identifier: y") ;test error para variables libres
(test/exn (typeof-ext (parse '{fun {x : Num} : {Num -> Num} 10})) "Type error in expression fun position 1: expected (Num -> Num) found Num")
(test/exn (typeof-ext (parse '{1 2})) "Type error in expression app position 1: expected (T -> S) found Num")
(test/exn (typeof-ext (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}})) "Type error in expression app position 2: expected Num found (Num -> Num)")
(test/exn (typeof-ext (parse '{y 2})) "Type error: free identifier: y")
(test/exn (typeof-ext (parse '{- {fun {x : Num} : Num {+ x x}} 2})) "Type error in expression - position 1: expected Num found (Num -> Num)")

;;typeof (son los mismos test que antes, verificando que hace lo correcto)
(test (typeof (parse '3)) (TNum)) 
(test (typeof (parse '{+ 1 3})) (TNum)) ;Test para verificar tipo de la adicion numerica
(test (typeof (parse '{fun {x : Num} : Num 5})) (TFun (TNum) (TNum))) ;Test para verificar 
(test (typeof (parse '{{fun {x : Num} x} 1})) (TNum)) ;test para verificar correctitud de aplicaciones
(test (typeof (parse '{{fun {f : {Num -> Num}} {f 12}} {fun {x : Num} {+ x x}}})) (TNum))
(test (typeof (parse '{{fun {x : Num} : Num {+ x 10}} 2})) (TNum))
(test (typeof (parse '{+ 1 {{fun {x : Num} : Num {+ x 10}} 2}})) (TNum))
(test/exn (typeof (parse 'y)) "Type error: free identifier: y") ;test error para variables libres
(test/exn (typeof (parse '{fun {x : Num} : {Num -> Num} 10})) "Type error in expression fun position 1: expected (Num -> Num) found Num")
(test/exn (typeof (parse '{1 2})) "Type error in expression app position 1: expected (T -> S) found Num")
(test/exn (typeof (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}})) "Type error in expression app position 2: expected Num found (Num -> Num)")
(test/exn (typeof (parse '{y 2})) "Type error: free identifier: y")
(test/exn (typeof (parse '{- {fun {x : Num} : Num {+ x x}} 2})) "Type error in expression - position 1: expected Num found (Num -> Num)")

;;typecheck
(test (typecheck '3) 'Num)
(test (typecheck '{fun {f : Num} : Num 10}) '(Num -> Num))
(test/exn (typecheck  '{+ 2 {fun {x : Num} : Num x}}) "Type error in expression + position 2: expected Num found (Num -> Num)") ;TODO este test no pasa
(test/exn (typecheck '{+ 1 {with {a : Num 1}
                                 {with {b : Num 1}
                                       {with {c : Num 1}
                                             {with {d : Num (fun {x : Num} : {Num -> Num} 1)}
                                                   (+ a a)}}}}}) "Type error in expression fun position 1: expected (Num -> Num) found Num")
;;empty-bruijn-env
(test empty-bruijn-env (mtEnv))
;;extend-bruijn-env
(test (extend-bruijn-env 'x (extend-bruijn-env 'y empty-bruijn-env)) (BruijnEnv 'x (BruijnEnv 'y empty-bruijn-env)))
;;extend-brujn-env
;; lookup-bruijn-env
(test (lookup-bruijn-env 'x (extend-bruijn-env 'y (extend-bruijn-env 'x empty-bruijn-env))) (acc 1))
(test (lookup-bruijn-env 'x (extend-bruijn-env 'z (extend-bruijn-env 'r (extend-bruijn-env 'x (extend-bruijn-env 'y (extend-bruijn-env 'x empty-bruijn-env)))))) (acc 2))
(test/exn (lookup-bruijn-env 'x (extend-bruijn-env 'y (extend-bruijn-env 'z empty-bruijn-env))) "Free identifier: x")
(test/exn (lookup-bruijn-env 'x empty-bruijn-env) "Free identifier: x")

;; deBruijn-ext
(test (deBruijn-ext (num 3)) (num 3))
(test (deBruijn-ext (parse '{+ 3 3})) (add (num 3) (num 3)))
(test (deBruijn-ext (parse '{- 3 3})) (sub (num 3) (num 3)))
(test (deBruijn-ext (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test (deBruijn-ext (parse '{with {x : {Num -> Num} {fun {y : Num} : Num (+ y y)}}
                              {with {y : Num 2}
                                    {x y}}})) (app (fun-db (app (fun-db (app (acc 1) (acc 0))) (num 2))) (fun-db (add (acc 0) (acc 0)))))
(test (deBruijn-ext (fun 'x (TNum) (add (id 'x) (num 1)) (TNum))) (fun-db (add (acc 0) (num 1))))
(test (deBruijn-ext (parse '(fun {x : Num} : {Num -> Num} 10 )))  (fun-db (num 10)))
(test/exn (deBruijn-ext (parse 'x)) "Free identifier: x")

;; deBruijn (son los mismos test pues esta funcion debe actuar igual que la anterior)
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{+ 3 3})) (add (num 3) (num 3)))
(test (deBruijn (parse '{- 3 3})) (sub (num 3) (num 3)))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test (deBruijn (parse '{with {x : {Num -> Num} {fun {y : Num} : Num (+ y y)}}
                              {with {y : Num 2}
                                    {x y}}})) (app (fun-db (app (fun-db (app (acc 1) (acc 0))) (num 2))) (fun-db (add (acc 0) (acc 0)))))
(test (deBruijn (fun 'x (TNum) (add (id 'x) (num 1)) (TNum))) (fun-db (add (acc 0) (num 1))))
(test (deBruijn (parse '(fun {x : Num} : {Num -> Num} 10 )))  (fun-db (num 10)))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")


;;compile
(test (compile (num 1)) (list (INT-CONST 1)))
(test (compile (sub (num 1) (num 1))) (list (INT-CONST 1) (INT-CONST 1) (SUB)))
(test (compile (add (num 2) (num 1))) (list (INT-CONST 1) (INT-CONST 2) (ADD)))
(test (compile (deBruijn (parse '{with {x : Num 2} {+ x x}}))) (list (INT-CONST 2) (CLOSURE (list (ACCESS 0) (ACCESS 0) (ADD) (RETURN))) (APPLY)))
(test (compile (deBruijn (parse '{{fun {x : Num} : Num {+ x 10}} {+ 2 3}}))) (list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN))) (APPLY))) 

;;typed-compile
(test (typed-compile '{+ 1 2}) (list (INT-CONST 2) (INT-CONST 1) (ADD)))
(test (typed-compile '{+ 1 {{fun {x : Num} : Num {+ x 10}} 2}}) (list (INT-CONST 2) (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN))) (APPLY) (INT-CONST 1) (ADD)))
(test (typed-compile '{with {y : {Num -> Num} {fun {x : Num} : Num {+ x x}}}
                        {with {x : {Num -> Num} {fun {y : Num} : Num {+ y 4}}}
                              {- {y 8} {x 3}}}}) (list
 (CLOSURE (list (ACCESS 0) (ACCESS 0) (ADD) (RETURN)))
 (CLOSURE (list (CLOSURE (list (INT-CONST 4) (ACCESS 0) (ADD) (RETURN))) (CLOSURE (list (INT-CONST 3) (ACCESS 0) (APPLY) (INT-CONST 8) (ACCESS 1) (APPLY) (SUB) (RETURN))) (APPLY) (RETURN)))
 (APPLY)))
(test/exn (typed-compile '{+ 1 {fun {x : Num} : Num {+ x 10}}}) "Type error in expression + position 2: expected Num found (Num -> Num)")
(test/exn (typed-compile '{+ 1 x}) "Type error: free identifier: x")
