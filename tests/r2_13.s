.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11791
    movq $0, 0(%rbp)
    jmp _end11792
_then11791:
    movq %rbx, 0(%rbp)
_end11792:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11793
    movq $0, %rbx
    jmp _end11794
_then11793:
    movq $1, %rbx
_end11794:
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11795
    movq $777, 0(%rbp)
    jmp _end11796
_then11795:
    movq $42, 0(%rbp)
_end11796:
    movq 0(%rbp), %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

