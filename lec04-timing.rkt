#lang plai

(define (mk-sums n)
  (cond [(zero? n)
         1]
        [else
         (define varn (string->symbol (format "x~a" n)))
         `{+ ,varn ,(mk-sums (- n 1))}]))

(define (mk-withs n body)
  (cond [(zero? n)
         body]
        [else
         (define varn (string->symbol (format "x~a" n)))
         `{with {,varn 1}
                ,(mk-withs (- n 1) body)}]))

(define (mk-exp n)
  (mk-withs n (mk-sums n)))

(test (mk-exp 2)
      `{with {x2 1}
             {with {x1 1}
                   {+ x2 {+ x1 1}}}})

;(require "lec03.rkt")
;(define (run n)
;  (define expr (parse (mk-exp n)))
;  (time (interp expr '())))

;; (require "lec04-f1wae.rkt")
(require "lec04-f1wae-hash.rkt")
 (define (run n)
   (define expr (parse (mk-exp n)))
   (time (interp expr '() (mtSub))))

(run 1000)
(run 2000)
