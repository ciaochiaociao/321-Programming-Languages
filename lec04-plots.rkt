#lang plai

(require plot)

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

(require (prefix-in lec03: "lec03.rkt"))
(define (run-lec03 n)
  (define expr (lec03:parse (mk-exp n)))
  (define-values (_ cpu real gc)
    (time-apply (lambda () (lec03:interp expr '())) '()))
  (vector n real))

(require (prefix-in lec04: "lec04-f1wae.rkt"))
(define (run-lec04 n)
  (define expr (lec04:parse (mk-exp n)))
  (define-values (_ cpu real gc)
    (time-apply (lambda () (lec04:interp expr '() (lec04:mtSub))) '()))
  (vector n real))

(require (prefix-in lec04-hash: "lec04-f1wae-hash.rkt"))
(define (run-lec04-hash n)
  (define expr (lec04-hash:parse (mk-exp n)))
  (define-values (_ cpu real gc)
    (time-apply (lambda () (lec04-hash:interp expr '() (lec04-hash:mtSub))) '()))
  (vector n real))

(define points
  (range 100 1600 100)
  ;(range 100 3200 200)
  )

(time
 (plot (list (lines (map run-lec03 points))
             (lines (map run-lec04 points))
             (lines (map run-lec04-hash points)))))
