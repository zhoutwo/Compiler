.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq $10, %rbx
    movq $15, 0(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 0(%rbp)
    movq $2, 16(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 16(%rbp)
    movq 16(%rbp), %rax
    movq %rax, 0(%rbp)
    addq %rbx, 0(%rbp)
    movq 0(%rbp), %rdi
    callq _print_int
    movq $0, %rax
    addq $32, %rsp
    popq %rbp
    retq

