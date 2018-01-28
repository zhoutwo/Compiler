.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $1, %rax
    cmpq $1, %rax
    je _then11823
    movq $0, %rbx
    jmp _end11824
_then11823:
    movq $42, %rbx
_end11824:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    popq %rbp
    retq

