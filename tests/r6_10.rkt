(define-syntax (swap m n) []
  (let ([b n])
    (swap-actual m)))

(define-syntax (swap-actual a) [b]
  (let ([tmp a])
    (let ([_ (set! a b)])
      (set! b tmp))))

(let ([b 20])
  (let ([y 42])
    (let ([_ (swap b y)])
      b)))
