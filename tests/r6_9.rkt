(define-syntax (swap m n) []
  (let ([b n])
    (swap-actual m)))

(define-syntax (swap-actual a)
  (let ([tmp a])
    (let ([_ (set! a b)])
      (set! b tmp))))

(let ([x 20])
  (let ([y 42])
    (let ([_ (swap x y)])
      x)))
