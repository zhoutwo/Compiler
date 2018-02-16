(define-syntax (swap a) [b]
  (let ([tmp a])
    (let ([_ (set! a b)])
      (set! b tmp))))

(let ([x 20])
  (let ([b 42])
    (let ([_ (swap x)])
      x)))
