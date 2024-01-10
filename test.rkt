#lang plai

#|
<AE> ::= <num>
       |  {+ <AE> <AE>}
       |  {- <AE> <AE>}
|#

;;; (print-only-errors)

(number? 2)
(define-type Shape
  [circle (radius number?)]
  [rectangle (width number?) (height number?)])

(rectangle 10 20)

;; 1. define-type
(define-type Human
    [person (name string?) (age number?)]
    [alien (planet string?) (age number?) (name string?)])

(define x (alien "Mars" 100 "Marvin"))
(alien-name x)
(set-alien-name! x "Marvin the Martian")
(alien-name x)

;; try type-case
;;; (define a-human (person "Marvin" 100))

;;; (define (print-human h)
;;;     (type-case Human h
;;;         (person (name age) ( (println "# I am a person\n") (println (string-append "Name:" name )) (println (string-append "Age:" (number->string age)))))
;;;         (alien (planet age name) ((println "# I am an alien\n") (println (string-append "Name:" name )) (println (string-append "Age:" (number->string age))) (println (string-append "Planet:" planet))))
;;;         )
;;;     )


;;; (print-human a-human)
;;; (print-human x)

;; 2. define-struct
(define-struct Human2 (name age))

(define x2 (make-Human2 "Marvin" 100))
(Human2-age x2)

;; 3. struct
(struct Human3 (name age))
(define x3 (Human3 "Marvin" 100))



(define/contract distance (>=/c 0) 43.52)

(define (square2 x)
    (* x x)
)

(square2 10)

(+ distance 1)

(define/contract (square x)
  ;; The contract specification
  (-> number? number?)
  ;; The function implementation
  (+ x x))

(square 10)

(in-range 1 10)

(for ([i (in-range 1 10)])
    (println i))

(define num-list (for/list ((i (in-range 1 10))) i ))

(println num-list)


;;

(println -5)
(println (- 5))

; DP
(vector 1 2 3 4 5)
(define n 5)
(define fibs (make-vector 5 1))
(vector-set! fibs 0 0)
(vector-set! fibs 0 1)


;;; ;; FILEPATH: /G:/My Drive/0. NU/321/test.rkt
;;; ;; BEGIN: ed8c6549bwf9
;;; (define (fib n)
;;;     (cond
;;;         [(= n 0) 1]
;;;         [(= n 1) 1]
;;;         [else (+ (fib (- n 1)) (fib (- n 2)))]))

;;; ;; END: ed8c6549bwf9

;;; (fib 5)
