#lang plai

(print-only-errors)

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
        (bound-expr F1WAE?)
        (body-expr F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)
       (arg-expr (listof F1WAE?))])

(test (app 'f (list (num 2) (num 3))) (app 'f (list (num 2) (num 3))))

(define-type FunDef
  [fundef (fun-name symbol?)
          (param-names (listof symbol?))
          (body F1WAE?)]
  )

(define (check-pieces s-exp n-pieces expected)
  (unless (and (list? s-exp)
               (= (length s-exp) n-pieces))
    (error 'parse "expected ~a, got: ~a" expected s-exp)))

(define (is-a-list s-exp)
  (unless (list? s-exp)
    (error 'parse "expected a list, got: ~a" s-exp)))
#|
<FunDef> ::= {deffun {<id> <id>} <F1WAE>}
<F1WAE> ::= <num>
         | {+ <F1WAE> <F1WAE>}
         | {- <F1WAE> <F1WAE>}
         | {with {<id> <F1WAE>} <F1WAE>}
         | <id>
         | {<id> <F1WAE>*}
|#

;; parse : s-expression -> F1WAE?
(define (parse s-exp)
  (cond [(number? s-exp) (num s-exp)]
        [(symbol? s-exp) (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid F1WAE"))
         (case (first s-exp)
           [(+)
            (check-pieces s-exp 3 "addition")
            (add (parse (second s-exp))
                 (parse (third s-exp)))]
           [(-)
            (check-pieces s-exp 3 "subtraction")
            (sub (parse (second s-exp))
                 (parse (third s-exp)))]
           [(with)
            (check-pieces s-exp 3 "with")
            (check-pieces (second s-exp) 2 "with binding pair")
            (unless (symbol? (first (second s-exp)))
              (error 'parse "expected variable name, got: ~a" (first (second s-exp))))
            (with (first (second s-exp))
                  (parse (second (second s-exp)))
                  (parse (third s-exp)))]
           [else
            (unless (symbol? (first s-exp))
              (error 'parse "expected a function name, got: ~a" (first s-exp)))
            (app (first s-exp)  ; 'twice
                 (map parse (rest s-exp)))])]  ; (id 'x)
        [else
         (error 'parse "expected an F1WAE, got: ~a" s-exp)]))


(test
 (fundef 'f (list 'x 'y) (add (id 'x) (id 'y)))
 (fundef 'f (list 'x 'y) (add (id 'x) (id 'y)))
 )

; parse-defn : listOf symbol? or number? -> F1WAE 
(define (parse-defn s-exp)
  (match s-exp
    [`{deffun ,args ,body}
       ((lambda (args body)
         ; error handling for multiple same-name arguments
         ;(printf "s-exp: ~a\n" s-exp)
         (define my-set (list))
         (for ([symbol (rest args)])
           ;(print "1:")
           ;(println my-set)
           ;(printf "symbol: ~a my-set: ~a\n" symbol my-set)
           (if (member symbol my-set)
               (error "bad syntax: ~a" fundef)
               (set! my-set (cons symbol my-set))
               )
         )
         
         ; return function def
         (fundef (first args) (rest args) (parse body))
         ) args body)
     ]
    ))
(test (parse-defn '{deffun {f x y} {+ x y}})
    (fundef 'f '(x y) (add (id 'x) (id 'y)))
)
(test (parse-defn '{deffun {f x} x})
    (fundef 'f '(x) (id 'x))
)
(test (parse-defn '{deffun {f x} {+ x 1}})
    (fundef 'f '(x) (add (id 'x) (num 1)))
)
; test parse-defn
(test (parse-defn `{deffun {f x} {twice x}})
    (fundef 'f '(x) (app 'twice (list (id 'x)))))


;; ----------------------------------------------------------------------

;; interp : F1WAE? (listof FunDef?) -> number?
(define (interp an-f1wae fundefs)
  ;(println an-f1wae)
  (type-case F1WAE an-f1wae
    [num (n) n]
    [add (lhs rhs)
         (+ (interp lhs fundefs)
            (interp rhs fundefs))]
    [sub (lhs rhs)
         (- (interp lhs fundefs)
            (interp rhs fundefs))]
    [with (name named-expr body)
          (interp (subst body
                         name
                         (interp named-expr fundefs))
                  fundefs)]
    [id (name)
        (error 'interp "free identifier: ~a" name)]
    [app (fun-name values)
         (define the-fundef (lookup-fundef fun-name fundefs))
         (define body (fundef-body the-fundef))
         (define formal-params (fundef-param-names the-fundef))

         ; map interp to all values and get the actual numbers
         (define actual-nums (map
                              (lambda (x)
                                (interp x fundefs))
                              values ))

          ; iteratively substitute all formal parameters found in body with the actual values
          (define final-ae (for/fold ([init-body body])  ;
                                ([value actual-nums]  ; actual parameter. real numbers
                                 [param formal-params]  ; formal parameters 
                                 )
                        (subst init-body param value)  ; -> AE
                        )) ; -> AE
          
         ; wrong arity error handling
         (and (not (= (length values) (length formal-params)))
             (error "wrong arity: ~a" values))

          ; interp
          (define result (interp final-ae fundefs))

         result
         ]
    )
  )

;; subst-everything: F1WAE? (listOf symbol?) (listOf number?) -> F1WAE?
#|(define (subst-everything a-f1wae names values)
  (for/fold (wae a-f1wae)
            ([name names] [value values])
            (app fun-name (subst wae name value))
    )
  )
|#

(define (lookup-fundef name fundefs)
  (cond [(empty? fundefs)
         (error 'interp "undefined function: ~a" name)]
        [(equal? name (fundef-fun-name (first fundefs)))
         (first fundefs)]
        [else
         (lookup-fundef name (rest fundefs))]))

;; subst : F1WAE? symbol? number? -> F1WAE?
(define (subst a-f1wae name value)
  ;(print "in subst")
  ;(println a-f1wae)
  (type-case F1WAE a-f1wae
    [num (n)
         a-f1wae]
    [add (l r)
         (add (subst l name value)
              (subst r name value))]
    [sub (l r)
         (sub (subst l name value)
              (subst r name value))]
    [with (name2 named-expr body)
          (with name2 (subst named-expr name value)
                (if (equal? name name2)
                    body
                    (subst body name value)))]
    [id (name2)
        (if (equal? name name2)
            (num value)
            a-f1wae)]
    [app (fun-name arg-exprs)
           (define new-args 
             (for/list ([arg arg-exprs])
               (subst arg name value)
               ))
           (app fun-name new-args)
         ]
    ))



#|
;; {deffun {neg-5 y} {- 10 5}}
;; {neg-5 8}
(test (interp (parse `{neg-5 8})
              (list (fundef 'neg-5 (list 'x)
                            (parse `-5))))
      -5)

;; {deffun {shout} {}}
;; {nines}
(test (interp (parse `{nines})
              (list (fundef 'nines '()
                            (parse `999999999))))
      999999999)


;; failed
;; {deffun {add1 x} {+ x 1}}
;; {add1 {add1 10}}
(test (interp (parse `{add1 {add1 10}})
              (list (parse-defn '{deffun {add1 x} {+ x 1}})))
      12
      )

;; {deffun {twice y} {+ y y}}
;; {twice 0}
(test (interp (parse `{twice 10})
              (list (fundef 'twice (list 'y)
                            (parse `{+ y y}))))
      20)

;; {deffun {sum x y} {+ x y}}
;; {sum 5 6}
(test (interp (parse `{sum 5 6})
              (list (fundef 'sum (list 'x 'y)
                            (parse `{+ x y}))))
      11)

(test (interp (parse `{twice 10})
              (list (fundef 'twice (list 'y)
                            (parse `{+ y y}))
                    (fundef 'sum (list 'x 'y)
                            (parse `{+ x y}))))
      20)

; official tests
(test (interp (parse '{f 1 2})
              (list (parse-defn '{deffun {f x y} {+ x y}})))
      3)
(test (interp (parse '{+ {f} {f}})
              (list (parse-defn '{deffun {f} 5})))
      10)
|#
; success :)
(test (interp (parse `{f 10})
              (list (parse-defn `{deffun {f x} {twice x}})
                    (parse-defn `{deffun {twice y} {+ y y}})
              )
              )
        20)


; test cases for error handling for the grade
(test/exn (interp (parse '{with {x y} 1})
(list))
"free identifier")
(test/exn (interp (parse '{f 1 1})
(list (parse-defn '{deffun {f x x} {+ x x}})))
"bad syntax")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {g a b c} c})))
"undefined function")
(test/exn (interp (parse '{f 1})
(list (parse-defn '{deffun {f x y} {+ x y}})))
"wrong arity")

(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {f a b c} c})))
"free identifier")

; test no overloading function
(test/exn (interp (parse `{f 3 4}) (list (parse-defn '{deffun {f a} 5})
(parse-defn '{deffun {f a b} {+ a b}}))) "wrong arity")

#|


(test (interp (parse `{f 10})
              (list (parse-defn `{deffun {f x} {- 20 {twice x}}})
                    (parse-defn `{deffun {twice y} {+ y y}})
              )
              )
        0)

;; {deffun {f x}     {- 20 {twice {twice x}}}}
;; {deffun {twice y} {+ y y}}
;; {f 10}
(test (interp (parse `{f 10})
              (list (parse-defn `{deffun {f x} {- 20 {twice {twice x}}}})
                    (parse-defn `{deffun {twice y} {+ y y}})
              )
              )
        20)

(test (interp (parse `{f 10})
              (list (fundef 'f (list 'x)
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice (list 'y)
                            (parse `{+ y y}))))
      -20)

;; 5 -> 5
(test (interp (parse `5) '())
      5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '())
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '())
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '())
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '())
      6)
#|
x
|#
(test/exn (interp (parse `x) '())
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {x {- 4 3}}
                               {+ x x}}})
              '())
      8)
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {y {- 4 3}}
                               {+ y y}}})
              '())
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '())
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '())
      6)


#|
substitute 10 for x in {+ 1 x}
|#
(test (subst (add (num 1) (id 'x))
             'x
             10)
      (add (num 1) (num 10)))
#|
substitute 10 for x in y
|#
(test (subst (id 'y) 'x 10)
      (id 'y))
#|
substitute 10 for x in {- 1 x}
|#
(test (subst (sub (num 1) (id 'x))
             'x
             10)
      (sub (num 1) (num 10)))
#|
substitute 10 for x in {with {y 17} x}
|#
(test (subst (with 'y (num 17) (id 'x))
             'x
             10)
      (with 'y (num 17) (num 10)))
#|
substitute 10 for x in {with {y x} y}
|#
(test (subst (with 'y (id 'x) (id 'y))
             'x
             10)
      (with 'y (num 10) (id 'y)))
#|
substitute 10 for x in {with {x y} x}
|#
(test (subst (with 'x (id 'y) (id 'x))
             'x
             10)
      (with 'x (id 'y) (id 'x)))

(interp (parse `(f 10)) (list (fundef 'f (list 'x) (parse `(- 20 (twice (twice x))))) (fundef 'twice (list 'y) (parse `(+ y y)))))

|#