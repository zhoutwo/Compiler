.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $0, %rbx
    xorq $1, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11956
    movq $1, 0(%rbp)
    jmp _end11957
_then11956:
    movq $0, 0(%rbp)
_end11957:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11958
    movq $42, %rbx
    jmp _end11959
_then11958:
    movq $777, %rbx
_end11959:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

