#lang racket
(require "../utilities.rkt")
(require "../runtime-config.rkt")
(provide print-x86)

(define print-x86-arg
  (lambda (arg)
    (match arg
           [`(int ,value) (string-append "$" (number->string value))]
           [`(reg ,reg) (string-append "%" (symbol->string reg))]
           [`(byte-reg ,reg) (string-append "%" (symbol->string reg))]
           [`(deref ,reg ,value) (string-append (number->string value) "(%" (symbol->string reg) ")")]
           [`(global-value ,val) (string-append "_" (symbol->string val) "(%rip)")]
           [`(function-ref ,label) (string-append "_" (sanitized-label label) "(%rip)")]
           [else (symbol->string arg)])))

(define print-save-caller-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "saving_caller")) ":\n")
      (let loop ([regs (set->list caller-save)])
        (if (null? regs)
            ""
            (string-append "\tpushq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_saving_caller")) ":\n"))))

(define print-restore-caller-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "restoring_caller")) ":\n")
      (let loop ([regs (reverse (set->list caller-save))])
        (if (null? regs)
            ""
            (string-append "\tpopq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_restoring_caller")) ":\n"))))

(define print-save-general
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "saving_general")) ":\n")
      (let loop ([regs (vector->list general-registers)])
        (if (null? regs)
            ""
            (string-append "\tpushq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_saving_general")) ":\n"))))

(define print-restore-general
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "restoring_general")) ":\n")
      (let loop ([regs (reverse (vector->list general-registers))])
        (if (null? regs)
            ""
            (string-append "\tpopq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_restoring_general")) ":\n"))))

(define print-save-callee-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "saving_callee")) ":\n")
      (let loop ([regs (set->list callee-save)])
        (if (null? regs)
            ""
            (string-append "\tpushq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_saving_callee")) ":\n"))))

(define print-restore-callee-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "restoring_callee")) ":\n")
      (let loop ([regs (reverse (set->list callee-save))])
        (if (null? regs)
            ""
            (string-append "\tpopq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_restoring_callee")) ":\n"))))

(define mac? (equal? 'macosx (system-type 'os)))

(define print-common
  (lambda (stm)
    (match stm
          [`(label ,label)
            (string-append (if mac? "_" "") (sanitized-label label) ":\n")]
          [`(jmp-if ,cc ,label)
            (string-append "\t" "j" (symbol->string cc) " " (if mac? "_" "") (sanitized-label label) "\n")]
          [`(jmp ,label)
            (string-append "\t" "jmp " (if mac? "_" "") (sanitized-label label) "\n")]
          [`(set ,cc ,reg)
            (string-append "\t" "set" (symbol->string cc) " " (print-x86-arg reg) "\n")]
          [`(callq ,func)
            (string-append
              (print-save-caller-save)
              "\tcallq " (print-x86-arg func) "\n"
              (print-restore-caller-save))]
          [`(indirect-callq ,func)
            (string-append "\tcallq *" (print-x86-arg func) "\n")]
          [`(leaq ,arg1 ,arg2)
            (string-append "\tleaq" " " (print-x86-arg arg1) ", " (print-x86-arg arg2) "\n")]
          [`(,op ,arg1 ,arg2)
            (string-append "\t" (symbol->string op) " " (print-x86-arg arg1) ", " (print-x86-arg arg2) "\n")]
          [`(,op ,arg1)
            (string-append "\t" (symbol->string op) " " (print-x86-arg arg1) "\n")]
            )))

(define print-x86
  (lambda (p)
    (match p
      [`(define (,label) (,size ,rspills) ,stms)
          (string-append
            "\t.global " (if mac? "_" "") (sanitized-label label) "\n"
            (if mac? "_" "") (sanitized-label label) ":\n"
            "\tpushq %rbp\n"
            "\tmovq %rsp, %rbp" "\n"
            ;(print-save-caller-save)
            ;(print-save-callee-save)
            (print-save-general)
            (if (< 0 size) (string-append "\tsubq $" (number->string size) ", %rsp" "\n") "")
            "\tmovq $0, (%r15)" "\n"
            "\taddq $" (number->string rspills) ", %r15" "\n"
            (string-append* (map print-common stms))
            "\tsubq $" (number->string rspills) ", %r15" "\n"
            (if (< 0 size) (string-append  "\taddq $" (number->string size) ", %rsp" "\n") "")
            ;(print-restore-callee-save)
            ;(print-restore-caller-save)
            (print-restore-general)
            "\tpopq %rbp\n"
            "\tretq" "\n\n")]
      [`(program (,size ,rspills) (type ,type) (defines ,defs ...) ,stms)
          (string-append
            (string-append*
              (string-append* (map print-x86 defs))
              "\t.global " (if mac? "_" "") "main" "\n"
              (if mac? "_" "") "main:" "\n"
              ;"\tpushq %rbp\n"
              "\tmovq %rsp, %rbp" "\n"
              (print-save-callee-save)
              (if (< 0 size) (string-append "\tsubq $" (number->string size) ", %rsp" "\n") "")
              "\tmovq $" (number->string (rootstack-size)) ", %rdi" "\n"
              "\tmovq $" (number->string (heap-size)) ", %rsi" "\n"
              "\tcallq _initialize" "\n"
              "\tmovq _rootstack_begin(%rip), %r15" "\n"
              "\tmovq $0, (%r15)" "\n"
              "\taddq $" (number->string rspills) ", %r15" "\n"
              (map print-common stms))
            (print-by-type type)
            "\tmovq $0, %rax" "\n"
            "\tsubq $" (number->string rspills) ", %r15" "\n"
            (if (< 0 size) (string-append  "\taddq $" (number->string size) ", %rsp" "\n") "")
            (print-restore-callee-save)
            ;"\tpopq %rbp\n"
            "\tretq" "\n")])))
