#lang plai

#|
<AE> ::= <num>
       |  {+ <AE> <AE>}
       |  {- <AE> <AE>}
|#

;;; (print-only-errors)

(define-type AE  ; (define-type type-id
  [num (n number?)]     ; (variant-id (field-id contract-expr))
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

(define an-ae (add (num 1) (num 2)))


;; interp : AE? -> number?
(define (interp an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]))

;; 1
(test (interp (num 1))
      1)

;; {2 + 5}
(test (interp (add (num 2) (num 5)))
      7)

;; {2 - 5}
(test (interp (sub (num 2) (num 5)))
      -3)

;; {+ 1 {- 3 2}}
(test (interp (add (num 1) (sub (num 3) (num 2))))
      2)

;; {{1 - 2} + {3 + 4}}
(test (interp (add (sub (num 1) (num 2)) (add (num 3) (num 4))))
      6)
