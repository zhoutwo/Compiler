(define-syntax (get42-call-by-name m n) [x]
  (let ([val1 m])
    (let ([_ (set! x val1)])
       (let ([val2 m])
         (set! n val2)))))

(define (mult [x : Integer] [y : Integer]) : Integer
  (if (eq? 0 x)
      0
      (+ y (mult (+ (- 1) x) y))))

(let ([x 7])
  (let ([y 5])
    (let ([_ (get42-call-by-name (mult x 2) y)])
      (+ x y))))
