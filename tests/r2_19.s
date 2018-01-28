.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $0, %rax
    je _then11816
    movq $0, 0(%rbp)
    jmp _end11817
_then11816:
    callq _read_int
    movq %rax, %rbx
    cmpq $42, %rbx
    sete %al
    movzbq %al, %rbx
    movq %rbx, 0(%rbp)
_end11817:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11818
    movq $42, %rbx
    jmp _end11819
_then11818:
    movq $777, %rbx
_end11819:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

