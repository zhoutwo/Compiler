.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    callq _read_int
    movq %rax, %rbx
    callq _read_int
    movq %rax, 0(%rbp)
    cmpq 0(%rbp), %rbx
    sete %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11834
    movq $0, %rbx
    jmp _end11835
_then11834:
    movq $42, %rbx
_end11835:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

