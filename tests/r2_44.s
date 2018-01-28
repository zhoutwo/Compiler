.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    callq _read_int
    movq %rax, %rbx
    cmpq $0, %rbx
    sete %al
    movzbq %al, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11938
    movq $0, 0(%rbp)
    jmp _end11939
_then11938:
    callq _read_int
    movq %rax, %rbx
    cmpq $1, %rbx
    sete %al
    movzbq %al, %rbx
    movq %rbx, 0(%rbp)
_end11939:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11940
    movq $42, %rbx
    jmp _end11941
_then11940:
    movq $0, %rbx
_end11941:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

