.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $240, %rsp
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
    callq _read_int
    movq %rax, 160(%rbp)
    callq _read_int
    movq %rax, 176(%rbp)
    callq _read_int
    movq %rax, 192(%rbp)
    callq _read_int
    movq %rax, 208(%rbp)
    callq _read_int
    movq %rax, 224(%rbp)
    negq 0(%rbp)
    addq 0(%rbp), %rbx
    movq 32(%rbp), %rax
    movq %rax, 0(%rbp)
    negq 0(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 16(%rbp)
    addq 16(%rbp), %rbx
    movq 64(%rbp), %rax
    movq %rax, 0(%rbp)
    negq 0(%rbp)
    movq 48(%rbp), %rax
    movq %rax, 16(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 16(%rbp)
    movq 96(%rbp), %rax
    movq %rax, 0(%rbp)
    negq 0(%rbp)
    movq 80(%rbp), %rax
    movq %rax, 32(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 32(%rbp)
    movq 16(%rbp), %rax
    movq %rax, 0(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 0(%rbp)
    addq 0(%rbp), %rbx
    movq 128(%rbp), %rax
    movq %rax, 0(%rbp)
    negq 0(%rbp)
    movq 112(%rbp), %rax
    movq %rax, 16(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 16(%rbp)
    movq 160(%rbp), %rax
    movq %rax, 0(%rbp)
    negq 0(%rbp)
    movq 144(%rbp), %rax
    movq %rax, 32(%rbp)
    movq 0(%rbp), %rax
    addq %rax, 32(%rbp)
    movq 16(%rbp), %rax
    movq %rax, 0(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 0(%rbp)
    movq 192(%rbp), %rax
    movq %rax, 16(%rbp)
    negq 16(%rbp)
    movq 176(%rbp), %rax
    movq %rax, 32(%rbp)
    movq 16(%rbp), %rax
    addq %rax, 32(%rbp)
    movq 224(%rbp), %rax
    movq %rax, 16(%rbp)
    negq 16(%rbp)
    movq 208(%rbp), %rax
    movq %rax, 48(%rbp)
    movq 16(%rbp), %rax
    addq %rax, 48(%rbp)
    movq 32(%rbp), %rax
    movq %rax, 16(%rbp)
    movq 48(%rbp), %rax
    addq %rax, 16(%rbp)
    movq 16(%rbp), %rax
    addq %rax, 0(%rbp)
    addq 0(%rbp), %rbx
    addq $42, %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $240, %rsp
    popq %rbp
    retq

