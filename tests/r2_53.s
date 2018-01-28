.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $1, %rdi
    callq _print_bool
    movq $0, %rax
    popq %rbp
    retq

