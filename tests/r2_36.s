.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $1, %rax
    je _then11883
    movq $0, 0(%rbp)
    jmp _end11884
_then11883:
    movq $1, 0(%rbp)
_end11884:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11885
    movq $777, %rbx
    jmp _end11886
_then11885:
    movq $42, %rbx
_end11886:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

