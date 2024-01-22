#lang plai

(print-only-errors)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [id (name symbol?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> WAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (num s-expr)]
        [(symbol? s-expr)
         (id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid WAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "add")
            (add (parse (second s-expr))
                 (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "sub")
            (sub (parse (second s-expr))
                 (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (with (first (second s-expr))
                  (parse (second (second s-expr)))
                  (parse (third s-expr)))]
           [else (error 'parse "expected WAE, got ~a" s-expr)])]
        [else (error 'parse "expected WAE, got ~a" s-expr)]))

(define (check-pieces s-expr n-pieces who)
  (unless (= n-pieces (length s-expr))
    (error 'parse "expected ~a, got ~a" who s-expr)))

(test (parse `1) (num 1))
(test (parse `y) (id 'y))
(test (parse `{+ 1 2}) (add (num 1) (num 2)))
(test (parse `{- 1 2}) (sub (num 1) (num 2)))
(test (parse `{with {x 3} {+ x 2}}) (with 'x (num 3) (add (id 'x) (num 2))))
(test/exn (parse `{+ 1 2 3}) "expected add")

;; ----------------------------------------------------------------------

;; interp : WAE? DefSub? -> number?
(define (interp a-wae ds)
  (type-case WAE a-wae
    [num (n) n]
    [add (l r) (+ (interp l ds) (interp r ds))]
    [sub (l r) (- (interp l ds) (interp r ds))]
    [id (name)
        (lookup name ds)]
    [with (name named-expr body)
          (define new-ds (aSub name
                               (interp named-expr ds)
                               ds))
          (interp body new-ds)]))

;; lookup : symbol? DefSub? -> number?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier: ~a" name)]
    [aSub (name2 value rest)
          (if (equal? name2 name)
              value
              (lookup name rest))]))


;; ----------------------------------------------------------------------
;; tests from last time, updated

(define initial-def-sub (mtSub))

;; 5 -> 5
(test (interp (parse `5) initial-def-sub)
      5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) initial-def-sub)
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) initial-def-sub)
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) initial-def-sub)
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              initial-def-sub)
      6)
#|
x
|#
(test/exn (interp (parse `x) initial-def-sub)
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
              initial-def-sub)
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
              initial-def-sub)
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              initial-def-sub)
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              initial-def-sub)
      6)
