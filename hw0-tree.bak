#lang plai

(print-only-errors)

(define-type Tree
    [positive-leaf (val natural?)]
    [negative-leaf (val natural?)]
    [interior-node (left Tree?) (right Tree?)])


; function that returns true if the tree contains the given value
(define (contains? tree check_val)
    (type-case Tree tree
        [positive-leaf (val) (if (= val check_val) #t #f)]
        [negative-leaf (val) (if (= val (- check_val)) #t #f)]
        [interior-node (left right) (or (contains? left check_val) (contains? right check_val))]
        ))


; function that returns the smallest value in the tree
(define (smallest tree)
    (type-case Tree tree
        [positive-leaf (val) val]
        [negative-leaf (val) (- val)]
        [interior-node (left right) (min (smallest left) (smallest right))]
        ))

; function that returns the sum of all values in the tree
(define (sum tree)
    (type-case Tree tree
        [positive-leaf (val) val]
        [negative-leaf (val) (- val)]
        [interior-node (left right) (+ (sum left) (sum right))]
        ))

; balanced if sum of all values in the tree is equal to zero
(define (balanced? tree)
    (= (sum tree) 0))

; deeply balanced if and only if all the interior nodes in the tree are balanced.
(define (deeply-balanced? tree)
    (type-case Tree tree
        [positive-leaf (val) #t]
        [negative-leaf (val) #t]
        [interior-node (left right) (and (deeply-balanced? left) (deeply-balanced? right) (balanced? tree))]  ; depth first
        ))

; a negate function, which takes a Tree as argument, and returns a new Tree of the same shape, but where the value of each leaf is negated.
(define (negate tree)
    (type-case Tree tree
        [positive-leaf (val) (negative-leaf val)]
        [negative-leaf (val) (positive-leaf val)]
        [interior-node (left right) (interior-node (negate left) (negate right))]
        ))

; an add function, which takes a Tree as its first argument and an integer as its second, and returns a new tree where the given integer is added to the value of all the leaves in the tree input tree
(define (add tree add_val)
    (type-case Tree tree
        ; use positive-leaf if the added result is positive, otherwise use negative-leaf
        [positive-leaf (val) 
            (let ([result (+ val add_val)])
                (if (> result 0) 
                    (positive-leaf (abs result)) 
                    (negative-leaf (abs result))))]

        [negative-leaf (val) 
            (let ([result (+ (- val) add_val)])
                (if (< result 0) 
                    (negative-leaf (abs result)) 
                    (positive-leaf (abs result))))]

        [interior-node (left right) (interior-node (add left add_val) (add right add_val))]
        ))

; Implement a positive-thinking function, which takes a Tree as argument and produces a new tree, which removes all negative leaves from the original tree. If the resulting tree would have no nodes, return false.
(define (positive-thinking tree)
    (type-case Tree tree
        [positive-leaf (val) (positive-leaf val)]
        [negative-leaf (val) #f]
        [interior-node (left right)
            (let ([left-tree (positive-thinking left)] [right-tree (positive-thinking right)])
                (cond   [(and (not left-tree) (not right-tree)) #f]
                        [(not left-tree) right-tree]
                        [(not right-tree) left-tree]
                        [else (interior-node left-tree right-tree)]))]
        ))

; Tests for positive-leaf
(test (contains? (positive-leaf 0) 0) #t)
(test (contains? (positive-leaf 5) 5) #t)
(test (contains? (positive-leaf 5) 6) #f)
(test (contains? (positive-leaf 5) -5) #f)

; Tests for negative-leaf
(test (contains? (negative-leaf 0) 0) #t)
(test (contains? (negative-leaf 5) -5) #t)
(test (contains? (negative-leaf 5) 5) #f)
(test (contains? (negative-leaf 5) 6) #f)

; Tests for interior-node
(test (contains? (interior-node (positive-leaf 5) (negative-leaf 6)) 5) #t)
(test (contains? (interior-node (positive-leaf 5) (negative-leaf 6)) -6) #t)
(test (contains? (interior-node (positive-leaf 5) (negative-leaf 6)) 6) #f)
(test (contains? (interior-node (positive-leaf 5) (negative-leaf 6)) -5) #f)

; Tests for nested interior-node
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) 5) #t)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) 6) #t)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) -7) #t)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) 7) #f)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) 8) #f)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) -6) #f)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) -5) #f)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) 4) #f)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (negative-leaf 7))) -4) #f)

; Tests for three-level nested interior-node 
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (interior-node (positive-leaf 7) (negative-leaf 8)))) 5) #t)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (interior-node (positive-leaf 7) (negative-leaf 8)))) 6) #t)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (interior-node (positive-leaf 7) (negative-leaf 8)))) 7) #t)
(test (contains? (interior-node (positive-leaf 5) (interior-node (positive-leaf 6) (interior-node (positive-leaf 7) (negative-leaf 8)))) -8) #t)

; Test for smallest value in single positive-leaf tree
(test (smallest (positive-leaf 5)) 5)

; Test for smallest value in single negative-leaf tree
(test (smallest (negative-leaf 5)) -5)

; Test for smallest value in nested positive-leaf trees
(test (smallest (interior-node (positive-leaf 5) (positive-leaf 10))) 5)

; Test for smallest value in nested negative-leaf trees
(test (smallest (interior-node (negative-leaf 5) (negative-leaf 10))) -10)

; Test for smallest value in nested positive-leaf and negative-leaf trees
(test (smallest (interior-node (positive-leaf 5) (negative-leaf 10))) -10); Tests for sum function
(test (sum (positive-leaf 5)) 5)
(test (sum (negative-leaf 5)) -5)
(test (sum (interior-node (positive-leaf 5) (negative-leaf 10))) -5)
(test (sum (interior-node (positive-leaf 5) (interior-node (positive-leaf 10) (negative-leaf 15)))) 0)
(test (sum (interior-node (interior-node (positive-leaf 5) (negative-leaf 10)) (interior-node (positive-leaf 15) (negative-leaf 20)))) -10)

; Tests for balanced? function
(test (balanced? (positive-leaf 5)) #f)
(test (balanced? (negative-leaf 5)) #f)
(test (balanced? (interior-node (positive-leaf 5) (negative-leaf 10))) #f)
(test (balanced? (interior-node (positive-leaf 5) (interior-node (positive-leaf 10) (negative-leaf 15)))) #t)
(test (balanced? (interior-node (interior-node (positive-leaf 5) (negative-leaf 10)) (interior-node (positive-leaf 15) (negative-leaf 20)))) #f); Tests for deeply-balanced?

; Tests for deeply-balanced?
(test (deeply-balanced? (positive-leaf 5)) #t)
(test (deeply-balanced? (negative-leaf 5)) #t)
(test (deeply-balanced? (interior-node (positive-leaf 5) (negative-leaf 10))) #f)
(test (deeply-balanced? (interior-node (positive-leaf 5) (interior-node (positive-leaf 10) (negative-leaf 15)))) #f)
(test (deeply-balanced? (interior-node (positive-leaf 0) (interior-node (positive-leaf 10) (negative-leaf 10)))) #t)
(test (deeply-balanced? (interior-node (interior-node (positive-leaf 5) (negative-leaf 5)) (interior-node (positive-leaf 15) (negative-leaf 15)))) #t)
(test (deeply-balanced? (interior-node (interior-node (positive-leaf 5) (negative-leaf 5)) (interior-node (positive-leaf 0) (negative-leaf 0)))) #t); Tests for negate function

; Tests for negate function
(test (contains? (negate (positive-leaf 5)) -5) #t)
(test (contains? (negate (negative-leaf 5)) 5) #t)
(test (contains? (negate (interior-node (interior-node (positive-leaf 1) (negative-leaf 2)) (interior-node (negative-leaf 3) (positive-leaf 4)))) -4) #t)
(test (contains? (negate (interior-node (interior-node (positive-leaf 1) (negative-leaf 2)) (interior-node (negative-leaf 3) (positive-leaf 4)))) 3) #t)
(test (contains? (negate (interior-node (interior-node (positive-leaf 1) (negative-leaf 2)) (interior-node (negative-leaf 3) (positive-leaf 4)))) 2) #t)
(test (contains? (negate (interior-node (interior-node (positive-leaf 1) (negative-leaf 2)) (interior-node (negative-leaf 3) (positive-leaf 4)))) -1) #t); Tests for add function

; Tests for add function
(test (contains? (add (positive-leaf 5) 10) 15) #t)
(test (contains? (add (positive-leaf 5) -10) -5) #t)
(test (contains? (add (negative-leaf 5) 10) 5) #t)
(test (contains? (add (negative-leaf 5) -10) -15) #t)
(test (contains? (add (interior-node (positive-leaf 5) (negative-leaf 10)) 10) 15) #t)
(test (contains? (add (interior-node (positive-leaf 5) (negative-leaf 15)) -10) -25) #t)
(test (balanced? (add (interior-node (positive-leaf 5) (negative-leaf 5)) 0)) #t)

; Tests for positive-thinking function
(test (contains? (positive-thinking (positive-leaf 5)) 5) #t)

;; test if all values are negatives, return false
(test (positive-thinking (negative-leaf 5)) #f)
(test (positive-thinking (interior-node (negative-leaf 5) (negative-leaf 10))) #f)

(test (sum (positive-thinking (interior-node (positive-leaf 5) (negative-leaf 10)))) 5)
(test (balanced? (positive-thinking (interior-node (positive-leaf 0) (negative-leaf 5)))) #t)
(test (add (positive-thinking (interior-node (positive-leaf 5) (negative-leaf 10))) 10) (positive-leaf 15))
(test (negate (positive-thinking (interior-node (positive-leaf 5) (negative-leaf 10)))) (negative-leaf 5))
(test (deeply-balanced? (interior-node (positive-leaf 0) (interior-node (positive-leaf 10) (negative-leaf 10)))) #t)
(test (deeply-balanced? (positive-thinking (interior-node (positive-leaf 0) (interior-node (positive-leaf 10) (negative-leaf 10))))) #f)