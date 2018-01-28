.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq $10, %rbx
    addq $11, %rbx
    movq $4, 16(%rbp)
    negq 16(%rbp)
    movq $25, 0(%rbp)
    movq 16(%rbp), %rax
    addq %rax, 0(%rbp)
    addq 0(%rbp), %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $32, %rsp
    popq %rbp
    retq

