(module+ test
  (require rackunit)
  
  (define (test-f)
    (check-equal? (f 5) 5)
    (check-equal? (f "hello") "hello")
    (check-equal? (f '(1 2 3)) '(1 2 3)))
  
  (test-f))