.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $1, %rax
    je _then11867
    movq $0, 0(%rbp)
    jmp _end11868
_then11867:
    movq $0, 0(%rbp)
_end11868:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11869
    movq $42, %rbx
    jmp _end11870
_then11869:
    movq $777, %rbx
_end11870:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

