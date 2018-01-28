.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $30, %rbx
    addq $10, %rbx
    addq $2, %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    popq %rbp
    retq

