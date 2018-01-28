.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    callq _read_int
    movq %rax, %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    popq %rbp
    retq

