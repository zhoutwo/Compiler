.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq $1, %rbx
    movq $2, 0(%rbp)
    callq _read_int
    movq %rax, 16(%rbp)
    cmpq $0, 16(%rbp)
    sete %al
    movzbq %al, %rax
    movq %rax, 16(%rbp)
    movq $1, %rax
    cmpq 16(%rbp), %rax
    je _then11979
    movq 0(%rbp), %rbx
    jmp _end11980
_then11979:
    negq %rbx
_end11980:
    addq $10, %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $32, %rsp
    popq %rbp
    retq

