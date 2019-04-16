#lang play
(require "main.rkt")
(require "machine.rkt")






;; parse-type
(test (parse-type '{Num}) (TNum))
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test (parse-type '{{Num -> Num} -> Num}) (TFun (TFun (TNum) (TNum)) (TNum)))
(test (parse-type '{{Num -> Num} -> {Num -> Num}}) (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum))))
(test/exn (parse-type '{Num -> }) "Parse error")
(test/exn (parse-type '{ -> Num}) "Parse error")
(test/exn (parse-type '{{Num -> Num} -> {Num -> }}) "Parse error" )

;;prettify tests
(test (prettify (TNum)) 'Num)
(test (prettify (TFun (TNum) (TNum))) '{Num -> Num})
(test (prettify (TFun (TFun (TNum) (TNum)) (TNum))) '{{Num -> Num} -> Num})

;; parse
(test (parse '{+ 1 3}) (add (num 1) (num 3)))
;(test (parse '{}))
(test (parse '{with {y : Num 2} y}) (app (fun 'y (TNum) (id 'y) #f) (num 2)))
; TODO: (test (parse '{with {y : {Num -> Num} {fun {x : Num} : Num {+ x x}}} {y 2}}) (app (fun 'y (TFun (TNum) (TNum)))
(test (parse '{fun {x : Num} : Num {+ x 1}}) (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)))
(test (parse '{with {y : Num 2} {+ x y}}) (app (fun 'y (TNum) (add (id 'x) (id 'y)) #f) (num 2)))
(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))
#|
;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")
|#
#|
;;compile
(test (compile (add (num 2) (num 1))) (list  (INT-CONST 1) (INT-CONST 2) (ADD)))


;;typeof
(test (typeof (parse '{+ 1 3})) (TNum))



;typecheck
(test (typecheck '3) 'Num)
|#