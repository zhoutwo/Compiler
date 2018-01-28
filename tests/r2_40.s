.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $0, %rax
    sete %al
    movzbq %al, %rax
    movq %rax, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11904
    movq $42, %rbx
    jmp _end11905
_then11904:
    movq $777, %rbx
_end11905:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

