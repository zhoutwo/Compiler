.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq $1, %rbx
    movq $46, 0(%rbp)
    addq $7, %rbx
    movq $4, 16(%rbp)
    addq %rbx, 16(%rbp)
    addq 0(%rbp), %rbx
    movq 16(%rbp), %rax
    movq %rax, 0(%rbp)
    negq 0(%rbp)
    addq 0(%rbp), %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $32, %rsp
    popq %rbp
    retq

