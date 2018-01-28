.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rbx
    callq _read_int
    movq %rax, 0(%rbp)
    cmpq 0(%rbp), %rbx
    sete %al
    movzbq %al, %rbx
    movq %rbx, 0(%rbp)
    xorq $1, 0(%rbp)
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11965
    movq $42, %rbx
    jmp _end11966
_then11965:
    movq $777, %rbx
_end11966:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

