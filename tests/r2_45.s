.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11944
    movq $0, %rbx
    jmp _end11945
_then11944:
    movq $42, %rbx
_end11945:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

