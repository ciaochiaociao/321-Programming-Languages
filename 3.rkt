#lang plai

(print-only-errors)

#|
<FunDef> ::= {deffun {<id> <id>} <F1WAE>}
<F1WAE> ::= <num>
         | {+ <F1WAE> <F1WAE>}
         | {- <F1WAE> <F1WAE>}
         | {with {<id> <F1WAE>} <F1WAE>}
         | <id>
         | {<id> <F1WAE>}
|#
(define-type F1WAE
  [num (n number?)]
  [add (l F1WAE?) (r F1WAE?)]
  [sub (l F1WAE?) (r F1WAE?)]
  [with (name symbol?)
        (named-expr F1WAE?)
        (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)
       (arg-expr F1WAE?)]
  )

(test (app 'g (add (num 1) (num 0))) (app 'g (add (num 1) (num 0))))

(define-type FunDef 
  [fundef (fun-name symbol?) (param-name symbol?) (f1wae F1WAE?)]
  )

;; parser : sexp -> WAE?
(define (parse exp)
  (match exp
    ;[(list '+ l r) (add (parse l) (parse r))]
    [`{+ ,l ,r} (add (parse l) (parse r))]
    [`{- ,l ,r} (sub (parse l) (parse r))]
    [(? number?) (num exp)]
    [(? symbol?) (id exp)]
    [`{with ,args-list ,body}
     (with (first args-list) (parse (second args-list)) (parse body))]
    [`{,func-name ,arg}
     (app func-name (parse arg))]  ; must be at the end coz this will catch any list of two elements
    )
  )

(test (parse `{+ 3 {- x y}})
      (add (num 3) (sub (id `x) (id `y)))
      )

(test (parse `{with {x 3} {+ x 2}})
      (with 'x (num 3) (add (id 'x) (num 2))))


(test (parse `1)
      (num 1))
(test (parse `x)
      (id 'x))
(test (parse `{+ 1 2})
      (add (num 1) (num 2)))
(test (parse `{- 1 2})
      (sub (num 1) (num 2)))
(test (parse `{+ 1 {+ 2 3}})
      (add (num 1) (add (num 2) (num 3))))
(test (parse `{with {x 3} {+ x 2}})
      (with 'x (num 3) (add (id 'x) (num 2))))
(parse `{f 10})
(test (parse `{f 10})
      (app 'f (num 10)))

;(println (parse `{f 1}))
(test (parse `1) (num 1))
(test (parse `{f 1}) (app 'f (num 1)))
(test (parse `{f 1}) (app 'f (num 1)))

;; find-fundef : symbol (listof FunDef?) -> FunDef?
(define (find-fundef name fundefs)
  (for/or ([a-fundef (in-list fundefs)])
    (and (equal? (fundef-fun-name a-fundef) name)
         a-fundef)
    ))

;; interp : F1WAE? (listof FunDefs?) -> number?
(define (interp an-f1wae fundefs)
  (type-case F1WAE an-f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs) (interp r fundefs))]
    [sub (l r) (- (interp l fundefs) (interp r fundefs))]
    [with (name named-expr body)
          (interp (subst body name (interp named-expr fundefs)) fundefs)]
    [id (name) (error 'interp "free identifier: ~a" name)]
    [app (fun-name arg-expr)
         (type-case FunDef (find-fundef fun-name fundefs)
           [fundef (f x body)
                   (interp
                    (subst body x (interp arg-expr fundefs))
                    fundefs
                    )])
         ]))

;; subst : WAE? symbol? number? -> WAE?
(define (subst expr name value)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l name value)
                    (subst r name value))]
    [sub (l r) (sub (subst l name value)
                    (subst r name value))]
    [with (name2 named-expr body)
          (with name2
                (subst named-expr name value)
                (if (equal? name2 name) ; shadowing!
                    body
                    (subst body name value)))]
    [id (name2) (if (equal? name2 name)
                    (num value)
                    expr)]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr name value))
         ]
    ))

;; 5
(test (interp (num 5) '())
      5)
;;
(test (fundef 'twice 'y
                   (parse `{+ y y}))
      (fundef 'twice 'y (add (id 'y) (id 'y)))
      )

;;
;; 
(test (interp (parse `{f 10})
              (list (fundef 'f 'x
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice 'y
                            (parse `{+ y y}))))
      -20)
;; ;; {+ 1 2}
;; (test (interp (add (num 1) (num 2)))
;;       3)
;; 
;; ;; {- 3 2}
;; (test (interp (sub (num 3) (num 2)))
;;       1)
;; 
;; ;; {+ 1 {- 3 2}}
;; (test (interp (add (num 1) (sub (num 3) (num 2))))
;;       2)
;; 
;; #|
;; {with {x {+ 1 2}}
;;       {+ x x}}
;; |#
;; (test (interp (with 'x (add (num 1) (num 2))
;;                     (add (id 'x) (id 'x))))
;;       6)
;; 
;; #|
;; x
;; |#
;; (test/exn (interp (id 'x))
;;           "free identifier")
;; 
;; #|
;; {+ {with {x {+ 1 2}}
;;          {+ x x}}
;;    {with {x {- 4 3}}
;;          {+ x x}}}}
;; |#
;; (test (interp (add (with 'x (add (num 1) (num 2))
;;                          (add (id 'x) (id 'x)))
;;                    (with 'x (sub (num 4) (num 3))
;;                          (add (id 'x) (id 'x)))))
;;       8)
;; 
;; #|
;; {+ {with {x {+ 1 2}}
;;          {+ x x}}
;;    {with {y {- 4 3}}
;;          {+ y y}}}}
;; |#
;; (test (interp (add (with 'x (add (num 1) (num 2))
;;                          (add (id 'x) (id 'x)))
;;                    (with 'y (sub (num 4) (num 3))
;;                          (add (id 'y) (id 'y)))))
;;       8)
;; 
;; #|
;; {with {x {+ 1 2}}
;;       {with {x {- 4 3}}
;;             {+ x x}}}
;; |#
;; (test (interp (with 'x (add (num 1) (num 2))
;;                     (with 'x (sub (num 4) (num 3))
;;                           (add (id 'x) (id 'x)))))
;;       2)
;; 
;; #|
;; {with {x {+ 1 2}}
;;       {with {y {- 4 3}}
;;             {+ x x}}}
;; |#
;; (test (interp (with 'x (add (num 1) (num 2))
;;                     (with 'y (sub (num 4) (num 3))
;;                           (add (id 'x) (id 'x)))))
;;       6)
;; 
;; #|
;; substitute 10 for x in {+ 1 x}
;; |#
;; (test (subst (add (num 1) (id 'x))
;;              'x
;;              10)
;;       (add (num 1) (num 10)))
;; 
;; #|
;; substitute 10 for x in y
;; |#
;; (test (subst (id 'y) 'x 10)
;;       (id 'y))
;; 
;; #|
;; substitute 10 for x in {- x 1}
;; |#
;; (test (subst (sub (id 'x) (num 1))
;;              'x 10)
;;       (sub (num 10) (num 1)))
;; 
;; #|
;; substitute 10 for x in {with {y 17} x}
;; |#
;; (test (subst (with 'y (num 17) (id 'x))
;;              'x 10)
;;       (with 'y (num 17) (num 10)))
;; 
;; #|
;; substitute 10 for x in {with {y x} y}
;; |#
;; (test (subst (with 'y (id 'x) (id 'y))
;;              'x 10)
;;       (with 'y (num 10) (id 'y)))
;; 
;; #|
;; substitute 10 for x in {with {x y} x}
;; |#
;; (test (subst (with 'x (id 'y) (id 'x))
;;              'x 10)
;;       (with 'x (id 'y) (id 'x)))
;; 
;; #|
;; substitute 10 for x in 
;;   {with {x {+ x 1}}
;;     x}}
;; |#
;; (test (subst (with 'x (add (id 'x) (num 1)) (id 'x))
;;              'x 10)
;;       (with 'x (add (num 10) (num 1)) (id 'x)))
;; 
