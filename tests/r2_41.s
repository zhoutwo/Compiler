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
    je _then11912
    movq $1, %rbx
    addq $1, %rbx
    cmpq $2, %rbx
    sete %al
    movzbq %al, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11914
    movq $444, 0(%rbp)
    jmp _end11915
_then11914:
    movq $40, 0(%rbp)
_end11915:
    movq $2, %rbx
    addq 0(%rbp), %rbx
    jmp _end11913
_then11912:
    movq $777, %rbx
_end11913:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

