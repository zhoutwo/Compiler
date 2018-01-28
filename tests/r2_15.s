.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $42, %rbx
    cmpq $42, %rbx
    sete %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11802
    movq $20, %rbx
    jmp _end11803
_then11802:
_end11803:
    cmpq $42, %rbx
    sete %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11804
    movq $777, %rbx
    jmp _end11805
_then11804:
    movq $42, %rbx
_end11805:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

