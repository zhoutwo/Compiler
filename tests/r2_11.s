.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $0, %rbx
    xorq $1, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11775
    movq $1, 0(%rbp)
    jmp _end11776
_then11775:
    movq $0, 0(%rbp)
_end11776:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11777
    movq $42, %rbx
    jmp _end11778
_then11777:
    movq $777, %rbx
_end11778:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

