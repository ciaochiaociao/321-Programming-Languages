#lang plai
(print-only-errors)

;;; (displayln "Hello, \
;;; World!\"")
;;; (displayln "Hello, 
;;; World!\"")
;;; (displayln (+ 1 2))

;;; (define color 'red)

;;; (displayln color)

;;; (define my-hash (make-hash))
;;; (hash-set! my-hash 'name "Mr. Ranedeer")
;;; (hash-set! my-hash 'version 2.7)

;;; ; Retrieving values
;;; (displayln (hash-ref my-hash 'name))   ; Outputs: Mr. Ranedeer
;;; (displayln (hash-ref my-hash 'version)) ; Outputs: 2.7


;;; ; Using a symbol in a quoted expression
;;; (define quoted-expr '(+ x 1))
;;; (displayln quoted-expr)

;;; (displayln 'xyz)
;;; (displayln 'xyz)

;;; ; Creating a simple pair
;;; (define my-pair (cons 1 2))
;;; ; my-pair is now a pair (1 . 2)

;;; ; Creating a simple list
;;; (define my-list (cons 1 '()))
;;; ; my-list is now the list (1)

;;; ; Adding an element to the front of a list
;;; (define extended-list (cons 2 my-list))
;;; ; extended-list is now the list (2 1)


;;; ; Given the pair (1 . 2)
;;; (car my-pair) ; returns 1

;;; ; Given the list (2 1)
;;; (car extended-list) ; returns 2

;;; (displayln '(1 2 3))
;;; (displayln '(1 . 2))

; Syntax
#| <AE> ::= <num>
        | {+ <AE> <AE>}
        | {- <AE> <AE>}
   <num>  ::=  a number
|#

(define-type AE
    [num (n number?)]
    [add (ae1 AE?) (ae2 AE?)]
    [sub (ae1 AE?) (ae2 AE?)]
)

(define (interp an-ae)
    (type-case AE an-ae
        [num (n) n]
        [add (ae1 ae2) (+ (interp ae1) (interp ae2))]
        [sub (ae1 ae2) (- (interp ae1) (interp ae2))]
    )
)

(test (interp (num 2)) 2)
(test (interp (num -1)) -1)
(test (interp (add (num 2) (num -1))) 1)
(test (interp (sub (num -2) (num 1))) -3)
(test (interp (sub (num -2) (add (num 1) (num 0)))) -3)