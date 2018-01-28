.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $0, %rax
    je _then11877
    movq $0, 0(%rbp)
    jmp _end11878
_then11877:
    movq $0, 0(%rbp)
_end11878:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11879
    movq $42, %rbx
    jmp _end11880
_then11879:
    movq $777, %rbx
_end11880:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

