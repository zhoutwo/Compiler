.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $160, %rsp
    callq _read_int
    movq %rax, %rbx
    callq _read_int
    movq %rax, 0(%rbp)
    callq _read_int
    movq %rax, 16(%rbp)
    callq _read_int
    movq %rax, 32(%rbp)
    callq _read_int
    movq %rax, 48(%rbp)
    callq _read_int
    movq %rax, 64(%rbp)
    callq _read_int
    movq %rax, 80(%rbp)
    callq _read_int
    movq %rax, 96(%rbp)
    callq _read_int
    movq %rax, 112(%rbp)
    callq _read_int
    movq %rax, 128(%rbp)
    callq _read_int
    movq %rax, 144(%rbp)
    addq $31, 144(%rbp)
    movq 144(%rbp), %rax
    addq %rax, 128(%rbp)
    movq 128(%rbp), %rax
    addq %rax, 112(%rbp)
    movq 112(%rbp), %rax
    addq %rax, 96(%rbp)
    movq 96(%rbp), %rax
    addq %rax, 80(%rbp)
    movq 80(%rbp), %rax
    addq %rax, 64(%rbp)
    movq 64(%rbp), %rax
    addq %rax, 48(%rbp)
    movq 48(%rbp), %rax
    addq %rax, 32(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 16(%rbp)
    movq 16(%rbp), %rax
    addq %rax, 0(%rbp)
    addq 0(%rbp), %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $160, %rsp
    popq %rbp
    retq

