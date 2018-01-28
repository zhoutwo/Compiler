.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $0, %rbx
    movq $42, %rdi
    callq _print_int
    movq $0, %rax
    popq %rbp
    retq

