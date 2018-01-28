.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $0, %rax
    cmpq $0, %rax
    sete %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11770
    movq $777, %rbx
    jmp _end11771
_then11770:
    movq $42, %rbx
_end11771:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

