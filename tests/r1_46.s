.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rbx
    movq $42, 0(%rbp)
    addq 0(%rbp), %rbx
    movq 0(%rbp), %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

