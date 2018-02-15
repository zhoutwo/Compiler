(define-syntax (add a b) []
  (+ a b))

(define (addd [a : Integer] [b : Integer]) : Integer
  (add a (add a b)))

(addd 10 22)
