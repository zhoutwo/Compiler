.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $1, %rax
    je _then11808
    movq $0, 0(%rbp)
    jmp _end11809
_then11808:
    movq $1, 0(%rbp)
_end11809:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11810
    movq $777, %rbx
    jmp _end11811
_then11810:
    movq $42, %rbx
_end11811:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

