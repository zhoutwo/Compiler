.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $1, %rax
    cmpq $0, %rax
    je _then11826
    movq $42, %rbx
    jmp _end11827
_then11826:
    movq $0, %rbx
_end11827:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    popq %rbp
    retq

