.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $2, %rax
    cmpq $1, %rax
    setg %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11987
    movq $0, %rbx
    jmp _end11988
_then11987:
    movq $42, %rbx
_end11988:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

