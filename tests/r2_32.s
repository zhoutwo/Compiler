.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $1, %rax
    je _then11861
    movq $0, 0(%rbp)
    jmp _end11862
_then11861:
    movq $1, 0(%rbp)
_end11862:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11863
    movq $777, %rbx
    jmp _end11864
_then11863:
    movq $42, %rbx
_end11864:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

