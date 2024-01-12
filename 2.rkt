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
    [add (wae1 WAE?) (wae2 WAE?)]
    [sub (wae1 WAE?) (wae2 WAE?)]
    ;;; [with (bounding-id (id bounding-id)) (id-value-ae WAE?) (body-ae WAE?)]
    [with (bounding-id id?) (id-value-ae WAE?) (body-ae WAE?)]
    [id (x symbol?)]
)

(test (num 2) (num 2))
(test (with (id 'x) (num 2) (num 3)) (with (id 'x) (num 2) (num 3)))

(define (interp an-wae)
    (type-case WAE an-wae
        [num (n) n]
        [add (wae1 wae2) (+ (interp wae1) (interp wae2))]
        [sub (wae1 wae2) (- (interp wae1) (interp wae2))]
        [with (bounding-id id-value-wae body-wae) 
            (interp 
                (subst-wae 
                    bounding-id 
                    (interp id-value-wae) 
                    body-wae)
            )]
        [id (x) (error 'interp "free identifier: ~a" x)]
    )
)

(define (subst-wae bounding-id id-value body-wae)  ; (id 'x)  1
    (type-case WAE body-wae
        [num (n) body-wae]
        [add (wae1 wae2) (add 
            (subst-wae bounding-id id-value wae1)
            (subst-wae bounding-id id-value wae2))]
        [sub (wae1 wae2) (sub
            (subst-wae bounding-id id-value wae1)
            (subst-wae bounding-id id-value wae2))]
        [with (bounding-id2  ; (id 'y)
            id-value-wae2  ; (num 2)
            body-wae2)  ; (id 'x)
            (with bounding-id2
                (subst-wae bounding-id id-value id-value-wae2)

                (if (equal? bounding-id bounding-id2)
                    body-wae2; shadowing
                    (subst-wae bounding-id id-value body-wae2))
                )
        ]  ; 
        [id (x)
            (if (equal? body-wae bounding-id)
                (num id-value) 
                body-wae)]
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

(test (subst-wae
    (id 'x)
    2
    (add (id 'x) (num 1)))
    (add (num 2) (num 1)))
(test (subst-wae 
    (id 'y)
    3
    (add (id 'x) (num 1)))
    (add (id 'x) (num 1)))

(test (subst-wae 
    (id 'x)
    1
    (with (id 'y)
        (num 2)
        (id 'x)
        )
        )
    (with (id 'y)
        (num 2)
        (num 1)
        )
)
(test (subst-wae 
    (id 'x)  
    1
    (with (id 'x)
        (num 2)
        (id 'x)
        )
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