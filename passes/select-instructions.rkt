#lang racket
(require "../utilities.rkt")
(provide select-instructions)

(define (make-tag len types cur_pos)
  (let ([offset 7])
    (cond
        [(null? types) (bitwise-ior 1 (arithmetic-shift len 1))]
        [(and (pair? (car types)) (equal? 'Vector (caar types)))
          (bitwise-ior (arithmetic-shift 1 (+ offset cur_pos)) (make-tag len (cdr types) (+ 1 cur_pos)))]
        [else (make-tag len (cdr types) (+ 1 cur_pos))])))

(define select-instructions-helper
  (lambda (e)
    (match e
      [(? symbol?) (values `((var ,e)) 0)]
      [(? integer?) (values `((int ,e)) 0)]
      [(? boolean?) (values (if e
                      `((int 1))
                      `((int 0))) 0)]
      [`(void) (values `((int 0)) 0)]
      [`(assign ,var (global-value ,arg1))
          (define-values (varStmts varStack) (select-instructions-helper var))
          (values `((movq (global-value ,arg1) ,@varStmts)) varStack)]
      [`(assign ,var (vector-ref ,vec, n))
          (define-values (vecStmts vecStack) (select-instructions-helper vec))
          (define-values (varStmts varStack) (select-instructions-helper var))
          (values `((label ,(gensym "vector_ref_starts"))
                    (movq ,@vecStmts (reg r11))
                    (movq (deref r11 ,(* 8 (+ 1 n))) ,@varStmts)
                    (label ,(gensym "vector_ref_ends"))) (max vecStack varStack))]
      [`(assign ,var (vector-set! ,vec ,n ,arg))
          (define-values (vecStmts vecStack) (select-instructions-helper vec))
          (define-values (argStmts argStack) (select-instructions-helper arg))
          (define-values (varStmts varStack) (select-instructions-helper var))
          (values `((label ,(gensym "vector_set_starts"))
                    (movq ,@vecStmts (reg r11))
                    (movq ,@argStmts (deref r11 ,(* 8 (+ 1 n))))
                    (movq (int 0) ,@varStmts)
                    (label ,(gensym "vector_set_ends"))) (max vecStack argStack varStack))]
      [`(assign ,var (collect ,bytes))
          (define-values (bytesStmts bytesStack) (select-instructions-helper bytes))
          (define-values (varStmts varStack) (select-instructions-helper var))
          (values `((label ,(gensym "collect_starts"))
                    (movq (reg r15) (reg rdi))
                    (movq ,@bytesStmts (reg rsi))
                    ,(let ([mac? (equal? 'macosx (system-type 'os))])
                       (if mac?
                           `(callq _collect)
                           `(callq _collect)))
                    (movq (int 0) ,@varStmts)
                    (label ,(gensym "collect_ends"))) (max bytesStack varStack))]
      [`(assign ,var (allocate ,len (Vector ,types ...)))
          (define-values (varStmts varStack) (select-instructions-helper var))
          (values `((label ,(gensym "allocate_starts"))
                    (movq (global-value free_ptr) ,@varStmts)
                    (addq (int ,(* 8 (+ 1 len))) (global-value free_ptr))
                    (movq ,@varStmts (reg r11))
                    (movq (int ,(make-tag len types 0)) (deref r11 0))
                    (label ,(gensym "allocate_ends"))) varStack)]
      [`(assign ,lhs (+ ,op1 ,lhs))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((addq ,@op1Stmts ,@lhsStmts)) (max op1Stack lhsStack))]
      [`(assign ,lhs (+ ,lhs ,op1))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((addq ,@op1Stmts ,@lhsStmts)) (max op1Stack lhsStack))]
      [`(assign ,lhs (+ ,op1 ,op2))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (op2Stmts op2Stack) (select-instructions-helper op2))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((movq ,@op1Stmts ,@lhsStmts)
                      (addq ,@op2Stmts ,@lhsStmts)) (max op1Stack op2Stack lhsStack))]
      [`(assign ,lhs (- ,op1))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((movq ,@op1Stmts ,@lhsStmts)
                      (negq ,@lhsStmts)) (max op1Stack lhsStack))]
      [`(assign ,lhs (read))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((callq _read_int)
                      (movq (reg rax) ,@lhsStmts)) lhsStack)]
      [`(assign ,lhs (eq? ,op1 ,op2))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (op2Stmts op2Stack) (select-instructions-helper op2))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((cmpq ,@op2Stmts ,@op1Stmts)
                      (set e (byte-reg al))
                      (movzbq (byte-reg al) ,@lhsStmts)) (max op1Stack op2Stack lhsStack))]
      [`(assign ,lhs (> ,op1 ,op2))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (op2Stmts op2Stack) (select-instructions-helper op2))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((cmpq ,@op2Stmts ,@op1Stmts)
                      (set g (byte-reg al))
                      (movzbq (byte-reg al) ,@lhsStmts)) (max op1Stack op2Stack lhsStack))]
      [`(assign ,lhs (>= ,op1 ,op2))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (op2Stmts op2Stack) (select-instructions-helper op2))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((cmpq ,@op2Stmts ,@op1Stmts)
                      (set ge (byte-reg al))
                      (movzbq (byte-reg al) ,@lhsStmts)) (max op1Stack op2Stack lhsStack))]
      [`(assign ,lhs (<= ,op1 ,op2))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (op2Stmts op2Stack) (select-instructions-helper op2))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((cmpq ,@op2Stmts ,@op1Stmts)
                      (set le (byte-reg al))
                      (movzbq (byte-reg al) ,@lhsStmts)) (max op1Stack op2Stack lhsStack))]
      [`(assign ,lhs (< ,op1 ,op2))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (op2Stmts op2Stack) (select-instructions-helper op2))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((cmpq ,@op2Stmts ,@op1Stmts)
                      (set l (byte-reg al))
                      (movzbq (byte-reg al) ,@lhsStmts)) (max op1Stack op2Stack lhsStack))]
      [`(assign ,lhs (not ,op1))
            (define-values (op1Stmts op1Stack) (select-instructions-helper op1))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((movq ,@op1Stmts (reg rax))
                      (xorq (int 1) (reg rax))
                      (movq (int 1) (reg rbx))
                      (cmpq (reg rbx) (reg rax))
                      (set e (byte-reg al))
                      (movzbq (byte-reg al) ,@lhsStmts)) (max op1Stack lhsStack))]
      [`(assign ,var (function-ref ,label))
            (define-values (varStmts varStack) (select-instructions-helper var))
            (define varStmt (car varStmts))
            (values `((leaq (function-ref ,label) ,@varStmts)) varStack)]
      [`(assign ,var (app ,op ,rands ...))
            (define-values (varStmts varStack) (select-instructions-helper var))
            (define-values (opStmts opStack) (select-instructions-helper op))
            (define fname var)
            (define fStackNeeded (max (* 16 (- (length rands) 6)) 0))
            (define-values (randStmts randStacks) (map2 select-instructions-helper rands))
            (set! randStmts (apply append randStmts))
            (define moveStmts (let loop ([randStms randStmts]
                                         [regs (vector->list arg-registers)]
                                         [nextStackLoc 0]
                                         [moves '()])
                                         (cond
                                           [(null? randStms) (begin
                                                                (assert (or (eq? fStackNeeded 0) (eq? (- fStackNeeded) (- nextStackLoc 16))) "Inconsistent stack size")
                                                                moves)]
                                           [(null? regs)
                                              (loop (cdr randStms) '() (+ nextStackLoc 16) (cons `(movq ,(car randStms) (deref rsp ,nextStackLoc)) moves))]
                                           [else
                                              (loop (cdr randStms) (cdr regs) nextStackLoc (cons `(movq ,(car randStms) (reg ,(car regs))) moves))])))
            (values `(,@moveStmts
                      (indirect-callq ,@opStmts)
                      (movq (reg rax) ,@varStmts)) (apply max varStack opStack fStackNeeded randStacks))]
      [`(assign ,lhs ,num)
            (define-values (numStmts numStack) (select-instructions-helper num))
            (define-values (lhsStmts lhsStack) (select-instructions-helper lhs))
            (values `((movq ,@numStmts ,@lhsStmts)) (max numStack lhsStack))]
      [`(return ,arg)
            (define-values (argStmts argStack) (select-instructions-helper arg))
            (values `((movq ,@argStmts (reg rax))) argStack)]
      [`(if (eq? ,a ,b) ,thns ,alts)
            (define-values (newA aMaxStack) (select-instructions-helper a))
            (define-values (newB bMaxStack) (select-instructions-helper b))
            (define-values (newThns thnsMaxStacks) (map2 select-instructions-helper thns))
            (define-values (newAlts altsMaxStacks) (map2 select-instructions-helper alts))
            (values `((if (eq? ,@newA ,@newB)
                           ,(apply append newThns)
                           ,(apply append newAlts))) (apply max aMaxStack bMaxStack (apply max thnsMaxStacks) altsMaxStacks))]
      [`(define (,f [,xs : ,ts] ...) : ,rt (,var-types ...) ,stmts)
          (define-values (bodyStmts bodyMaxStacks) (map2 select-instructions-helper stmts))
          (define maxStack (apply max bodyMaxStacks))
          (define farg-types (map (lambda (x t) (cons x t)) xs ts))
          (define var-types-and-x-types `(,@var-types ,@farg-types))
          (define-values (xStmts xStacks) (map2 select-instructions-helper xs))
          (set! xStmts (apply append xStmts))
          (define moveStmts (let loop ([xStmts xStmts]
                                       [regs (vector->list arg-registers)]
                                       [nextStackLoc 16]
                                       [moves '()])
                                       (cond
                                         [(null? xStmts) moves]
                                         [(null? regs)
                                            (loop (cdr xStmts) '() (+ nextStackLoc 16) (cons `(movq (deref rbp ,nextStackLoc) ,(car xStmts)) moves))]
                                         [else
                                            (loop (cdr xStmts) (cdr regs) nextStackLoc (cons `(movq (reg ,(car regs)) ,(car xStmts)) moves))])))
          `(define
              (,f)
              ,(length xs)
              (,@var-types-and-x-types ,maxStack)
              (,@moveStmts ,@(apply append bodyStmts)))])))

(define (select-instructions e)
  (match e
    [`(program ,vars (type ,t) (defines ,defs ...) ,e)
        (define-values (newStms eMaxStacks) (map2 select-instructions-helper e))
        (define defStmts (map select-instructions-helper defs))
        (define maxStacks (apply max eMaxStacks))
        `(program (,vars ,(length vars)) (type ,t) (defines ,@defStmts) ,maxStacks ,(apply append newStms))]))
