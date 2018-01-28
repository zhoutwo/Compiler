.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    callq _read_int
    movq %rax, %rbx
    cmpq $0, %rbx
    sete %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11923
    callq _read_int
    movq %rax, %rbx
    cmpq $0, %rbx
    sete %al
    movzbq %al, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11925
    movq $444, 0(%rbp)
    jmp _end11926
_then11925:
    movq $40, 0(%rbp)
_end11926:
    movq $2, %rbx
    addq 0(%rbp), %rbx
    jmp _end11924
_then11923:
    movq $777, %rbx
_end11924:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

