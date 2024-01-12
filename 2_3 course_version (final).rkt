#lang plai

(print-only-errors)

; shadowing

;;; (define (foo x y)
;;;      (local ((define x 3))
;;;         (+ x y)
;;;      )
;;; )

;;; (foo 1 2)



; Syntax
#| <WAE> ::= <num>
        | {+ <WAE> <WAE>}
        | {- <WAE> <WAE>}
        | {with {<id> <WAE>} <WAE>}
        | <id>
   <num>  ::=  numbers
   <id>  ::=  identifiers
|#

(define-type WAE
    [num (n number?)]
    [add (l WAE?) (r WAE?)]
    [sub (l WAE?) (r WAE?)]
    ;;; [with (name (id name)) (value WAE?) (body WAE?)]
    [with (name symbol?) (value WAE?) (body WAE?)]
    [id (x symbol?)]
)

(test (num 2) (num 2))
(test (with 'x (num 2) (num 3)) (with 'x (num 2) (num 3)))

(define (interp an-wae)
    (type-case WAE an-wae
        [num (n) n]
        [add (l r) (+ (interp l) (interp r))]
        [sub (l r) (- (interp l) (interp r))]
        [with (name value-wae body) 
            (interp 
                (subst 
                    body
                    name 
                    (interp value-wae) )
            )]
        [id (x) (error 'interp "free identifier: ~a" x)]
    )
)

(define (subst body name value )  ; (id 'x)  1
    (type-case WAE body
        [num (n) body]
        [add (l r) (add 
            (subst l name value )
            (subst r name value ))]
        [sub (l r) (sub
            (subst l name value )
            (subst r name value ))]
        [with (name2  ; (id 'y)
            value-wae2  ; (num 2)
            body2)  ; (id 'x)
            (with name2
                (subst value-wae2 name value )

                (if (equal? name name2)
                    body2; shadowing
                    (subst body2 name value ))
                )
        ]  ; 
        [id (name2)
            (if (equal? name2 name)
                (num value) 
                body)]
    )
)


(test (subst
    (add (id 'x) (num 1))
    'x
    2)
    (add (num 2) (num 1)))
(test (subst 
    (add (id 'x) (num 1))
    'y
    3)
    (add (id 'x) (num 1)))

(test (subst 
    (with 'y
        (num 2)
        (id 'x)
        )
    'x
    1
        )
    (with 'y
        (num 2)
        (num 1)
        )
)
(test (subst 
    (with 'x
        (num 2)
        (id 'x)
        )
    'x
    1
        )
    (with 'x
        (num 2)
        (id 'x)
        )
)
(test (interp (num 2)) 2)
(test (interp (num -1)) -1)
(test (interp (add (num 2) (num -1))) 1)
(test (interp (sub (num -2) (num 1))) -3)
(test (interp (sub (num -2) (add (num 1) (num 0)))) -3)
(test/exn (interp (id 'x)) "free identifier")
(test (interp (with 
        'x
        (num 2) 
        (num 3))) 
    3)
(test (interp (with 
        'x
        (num 2) 
        (id 'x))) 
    2)
;;; 
(test (interp (with 
        'x
        (num 2) 
        (add (num 3) (id 'x))))
    5)
(test (interp (with 
        'x
        (num 2) 
        (with 
            'y
            (num 4)
            (id 'x)
            )))
    2)
; shadowing
(test (interp (with 
        'x
        (num 2) 
        (with 
            'x
            (num 4)
            (id 'x)
            )))
    4)

(test (subst
             (with 
                'x
                (add (id 'x) (num 1)) 
                (id 'x))
             'x
             10 )
      (with 
        'x
        (add (num 10) (num 1)) 
        (id 'x)))

#|
substitute 10 for x in {with {y x} y}
|#
(test (subst (with 'y (id 'x) (id 'y))
             'x 10)
      (with 'y (num 10) (id 'y)))

#|
substitute 10 for x in {with {x y} x}
|#
(test (subst (with 'x (id 'y) (id 'x))
             'x 10)
      (with 'x (id 'y) (id 'x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 5
(test (interp (num 5))
      5)

;; {+ 1 2}
(test (interp (add (num 1) (num 2)))
      3)

;; {- 3 2}
(test (interp (sub (num 3) (num 2)))
      1)

;; {+ 1 {- 3 2}}
(test (interp (add (num 1) (sub (num 3) (num 2))))
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (with 'x (add (num 1) (num 2))
                    (add (id 'x) (id 'x))))
      6)

#|
x
|#
(test/exn (interp (id 'x))
          "free identifier")

#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}}
|#
(test (interp (add (with 'x (add (num 1) (num 2))
                         (add (id 'x) (id 'x)))
                   (with 'x (sub (num 4) (num 3))
                         (add (id 'x) (id 'x)))))
      8)

#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}}
|#
(test (interp (add (with 'x (add (num 1) (num 2))
                         (add (id 'x) (id 'x)))
                   (with 'y (sub (num 4) (num 3))
                         (add (id 'y) (id 'y)))))
      8)

#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (with 'x (add (num 1) (num 2))
                    (with 'x (sub (num 4) (num 3))
                          (add (id 'x) (id 'x)))))
      2)

#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (with 'x (add (num 1) (num 2))
                    (with 'y (sub (num 4) (num 3))
                          (add (id 'x) (id 'x)))))
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
substitute 10 for x in {- x 1}
|#
(test (subst (sub (id 'x) (num 1))
             'x 10)
      (sub (num 10) (num 1)))

#|
substitute 10 for x in {with {y 17} x}
|#
(test (subst (with 'y (num 17) (id 'x))
             'x 10)
      (with 'y (num 17) (num 10)))

#|
substitute 10 for x in {with {y x} y}
|#
(test (subst (with 'y (id 'x) (id 'y))
             'x 10)
      (with 'y (num 10) (id 'y)))
