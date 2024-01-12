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
    [with (name id?) (value WAE?) (body WAE?)]
    [id (x symbol?)]
)

(test (num 2) (num 2))
(test (with (id 'x) (num 2) (num 3)) (with (id 'x) (num 2) (num 3)))

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
        [id (x)
            (if (equal? body name)
                (num value) 
                body)]
    )
)

;;; (with (id 'x)  
;;;     (num 1)
;;;     (with (id 'y)
;;;         (num 2)
;;;         (id 'x)
;;;         )
;;; )

;;;     (with (id 'y)
;;;         (num 2)
;;;         (num 1)

(test (subst
    (add (id 'x) (num 1))
    (id 'x)
    2)
    (add (num 2) (num 1)))
(test (subst 
    (add (id 'x) (num 1))
    (id 'y)
    3)
    (add (id 'x) (num 1)))

(test (subst 
    (with (id 'y)
        (num 2)
        (id 'x)
        )
    (id 'x)
    1
        )
    (with (id 'y)
        (num 2)
        (num 1)
        )
)
(test (subst 
    (with (id 'x)
        (num 2)
        (id 'x)
        )
    (id 'x)  
    1
        )
    (with (id 'x)
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
        (id 'x)
        (num 2) 
        (num 3))) 
    3)
(test (interp (with 
        (id 'x) 
        (num 2) 
        (id 'x))) 
    2)
;;; 
(test (interp (with 
        (id 'x)
        (num 2) 
        (add (num 3) (id 'x))))
    5)
(test (interp (with 
        (id 'x)
        (num 2) 
        (with 
            (id 'y)
            (num 4)
            (id 'x)
            )))
    2)
; shadowing
;;; (test (interp (with 
;;;         (id 'x)
;;;         (num 2) 
;;;         (with 
;;;             (id 'x)
;;;             (num 4)
;;;             (id 'x)
;;;             )))
;;;     4)
;;; 
(test (subst
             (with 
                (id 'x) 
                (add (id 'x) (num 1)) 
                (id 'x))
             (id 'x)
             10 )
      (with 
        (id 'x) 
        (add (num 10) (num 1)) 
        (id 'x)))