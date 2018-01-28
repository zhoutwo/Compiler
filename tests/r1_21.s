.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $14, %rbx
    movq %rbx, 0(%rbp)
    addq %rbx, 0(%rbp)
    addq %rbx, 0(%rbp)
    movq 0(%rbp), %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

