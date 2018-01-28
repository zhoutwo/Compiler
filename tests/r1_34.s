.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
    movq $1, 0(%rbp)
    addq $1, 0(%rbp)
    movq $1, %rbx
    addq $1, %rbx
    addq %rbx, 0(%rbp)
    movq $1, %rbx
    addq $1, %rbx
    movq $1, 16(%rbp)
    addq $1, 16(%rbp)
    addq 16(%rbp), %rbx
    addq %rbx, 0(%rbp)
    movq $1, %rbx
    addq $1, %rbx
    movq $1, 16(%rbp)
    addq $1, 16(%rbp)
    addq 16(%rbp), %rbx
    movq $1, 16(%rbp)
    addq $1, 16(%rbp)
    movq $1, 32(%rbp)
    addq $1, 32(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 16(%rbp)
    addq 16(%rbp), %rbx
    addq %rbx, 0(%rbp)
    movq $1, %rbx
    addq $1, %rbx
    movq $1, 16(%rbp)
    addq $1, 16(%rbp)
    addq 16(%rbp), %rbx
    movq $1, 16(%rbp)
    addq $1, 16(%rbp)
    movq $1, 32(%rbp)
    addq $1, 32(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 16(%rbp)
    addq 16(%rbp), %rbx
    movq $1, 16(%rbp)
    addq $1, 16(%rbp)
    movq $1, 32(%rbp)
    addq $1, 32(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 16(%rbp)
    movq $1, 32(%rbp)
    addq $1, 32(%rbp)
    movq $1, 48(%rbp)
    addq $1, 48(%rbp)
    movq 48(%rbp), %rax
    addq %rax, 32(%rbp)
    movq 32(%rbp), %rax
    addq %rax, 16(%rbp)
    addq 16(%rbp), %rbx
    addq %rbx, 0(%rbp)
    movq $10, %rbx
    addq 0(%rbp), %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $64, %rsp
    popq %rbp
    retq

