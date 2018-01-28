.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $320, %rsp
    movq $1, %rbx
    movq $1, 0(%rbp)
    movq $1, 16(%rbp)
    movq $1, 32(%rbp)
    movq $1, 48(%rbp)
    movq $1, 64(%rbp)
    movq $1, 80(%rbp)
    movq $1, 96(%rbp)
    movq $1, 112(%rbp)
    movq $1, 128(%rbp)
    movq $1, 144(%rbp)
    movq $1, 160(%rbp)
    movq $1, 176(%rbp)
    movq $1, 192(%rbp)
    movq $1, 208(%rbp)
    movq $1, 224(%rbp)
    movq $1, 240(%rbp)
    movq $1, 256(%rbp)
    movq $1, 272(%rbp)
    movq $1, 288(%rbp)
    movq $1, 304(%rbp)
    addq $21, 304(%rbp)
    movq 304(%rbp), %rax
    addq %rax, 288(%rbp)
    movq 288(%rbp), %rax
    addq %rax, 272(%rbp)
    movq 272(%rbp), %rax
    addq %rax, 256(%rbp)
    movq 256(%rbp), %rax
    addq %rax, 240(%rbp)
    movq 240(%rbp), %rax
    addq %rax, 224(%rbp)
    movq 224(%rbp), %rax
    addq %rax, 208(%rbp)
    movq 208(%rbp), %rax
    addq %rax, 192(%rbp)
    movq 192(%rbp), %rax
    addq %rax, 176(%rbp)
    movq 176(%rbp), %rax
    addq %rax, 160(%rbp)
    movq 160(%rbp), %rax
    addq %rax, 144(%rbp)
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
    addq $320, %rsp
    popq %rbp
    retq

