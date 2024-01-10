#lang racket
(displayln "Hello, \
World!\"")
(displayln "Hello, 
World!\"")
(displayln (+ 1 2))

(define color 'red)

(displayln color)

(define my-hash (make-hash))
(hash-set! my-hash 'name "Mr. Ranedeer")
(hash-set! my-hash 'version 2.7)

; Retrieving values
(displayln (hash-ref my-hash 'name))   ; Outputs: Mr. Ranedeer
(displayln (hash-ref my-hash 'version)) ; Outputs: 2.7


; Using a symbol in a quoted expression
(define quoted-expr '(+ x 1))
(displayln quoted-expr)

(displayln 'xyz)
(displayln 'xyz)

; Creating a simple pair
(define my-pair (cons 1 2))
; my-pair is now a pair (1 . 2)

; Creating a simple list
(define my-list (cons 1 '()))
; my-list is now the list (1)

; Adding an element to the front of a list
(define extended-list (cons 2 my-list))
; extended-list is now the list (2 1)


; Given the pair (1 . 2)
(car my-pair) ; returns 1

; Given the list (2 1)
(car extended-list) ; returns 2

(displayln '(1 2 3))
(displayln '(1 . 2))