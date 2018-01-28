.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $7, %rbx
    addq $3, %rbx
    movq %rbx, 0(%rbp)
    negq 0(%rbp)
    movq $52, %rbx
    addq 0(%rbp), %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

