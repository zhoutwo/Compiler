.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rax
    cmpq $1, %rax
    je _then11855
    movq $0, 0(%rbp)
    jmp _end11856
_then11855:
    movq $1, 0(%rbp)
_end11856:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11857
    movq $777, %rbx
    jmp _end11858
_then11857:
    movq $42, %rbx
_end11858:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

