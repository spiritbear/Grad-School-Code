   .globl _scheme_entry
_scheme_entry:
    pushq %rbx
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rdi, %rbp
    movq %rsi, %rdx
    leaq _scheme_exit(%rip), %r15
    movq %r15, 32(%rbp)
    movq %rdx, %r8
    addq $8, %rdx
    addq $2, %r8
    movq %r8, %r9
    leaq L0(%rip), %r8
    movq %r8, -2(%r9)
    movq %r9, 8(%rbp)
    movq %rdx, %r8
    addq $16, %rdx
    addq $2, %r8
    movq %r8, %r9
    leaq L1(%rip), %r8
    movq %r8, -2(%r9)
    movq %r9, 48(%rbp)
    movq %rdx, %r8
    addq $16, %rdx
    addq $2, %r8
    movq %r8, %r9
    leaq L9(%rip), %r8
    movq %r8, -2(%r9)
    movq %r9, 24(%rbp)
    movq 48(%rbp), %r8
    movq 48(%rbp), %r9
    movq %r8, 6(%r9)
    movq 24(%rbp), %r8
    movq 24(%rbp), %r9
    movq %r8, 6(%r9)
    movq $8, %r9
    movq $16, %r15
    movq $24, %rcx
    movq $22, %rax
    movq %rdx, %r8
    addq $16, %rdx
    addq $1, %r8
    movq %rcx, -1(%r8)
    movq %rax, 7(%r8)
    movq %r8, %rax
    movq %rdx, %r8
    addq $16, %rdx
    addq $1, %r8
    movq %r15, -1(%r8)
    movq %rax, 7(%r8)
    movq %r8, %r15
    movq %rdx, %r8
    addq $16, %rdx
    addq $1, %r8
    movq %r9, -1(%r8)
    movq %r15, 7(%r8)
    movq %r8, 0(%rbp)
    movq %rdx, %r8
    addq $8, %rdx
    addq $2, %r8
    movq %r8, %r9
    leaq L13(%rip), %r8
    movq %r8, -2(%r9)
    movq %r9, %r8
    movq %r8, 16(%rbp)
    addq $40, %rbp
    movq -40(%rbp), %r8
    movq %r8, 0(%rbp)
    movq -32(%rbp), %r9
    movq 8(%rbp), %r8
    leaq L78(%rip), %r15
    jmp L1
L0:
    movq %rdx, %r8
    addq $16, %rdx
    addq $2, %r8
    movq %r8, %rax
    leaq L14(%rip), %r8
    movq %r8, -2(%rax)
    movq %rax, %r8
    movq %r9, 6(%r8)
    movq %r8, %rax
    jmp *%r15
L1:
    movq %r15, 32(%rbp)
    movq %r8, 8(%rbp)
    movq %r9, 16(%rbp)
    cmpq $22, 0(%rbp)
    je L116
    jmp L117
L9:
    movq %r15, 40(%rbp)
    movq %r8, 16(%rbp)
    movq %r9, 24(%rbp)
    cmpq $22, 8(%rbp)
    je L114
    jmp L115
L14:
    movq 6(%r8), %r8
    addq %r9, %r8
    movq %r8, %rax
    jmp *%r15
L13:
    movq %r9, %rcx
    movq 0(%rbp), %r8
    movq -2(%rcx), %rax
    movq %r8, %r9
    movq %rcx, %r8
    jmp *%rax
L116:
    movq $22, %rax
    jmp *32(%rbp)
L117:
    movq 16(%rbp), %r8
    movq -2(%r8), %rax
    movq 0(%rbp), %r8
    movq -1(%r8), %r8
    addq $40, %rbp
    movq %r8, %r9
    movq -24(%rbp), %r8
    leaq L75(%rip), %r15
    jmp *%rax
L75:
    subq $40, %rbp
    movq %rax, 24(%rbp)
    movq 8(%rbp), %r8
    movq 6(%r8), %r15
    movq 0(%rbp), %r8
    movq 7(%r8), %r8
    addq $40, %rbp
    movq %r8, 0(%rbp)
    movq -24(%rbp), %r9
    movq %r15, %r8
    leaq L73(%rip), %r15
    jmp L1
L73:
    subq $40, %rbp
    movq %rax, %r15
    movq %rdx, %r8
    addq $16, %rdx
    addq $1, %r8
    movq %r8, %r9
    movq 24(%rbp), %r8
    movq %r8, -1(%r9)
    movq %r15, 7(%r9)
    movq %r9, %rax
    jmp *32(%rbp)
L114:
    movq $22, %rax
    jmp *40(%rbp)
L115:
    movq 24(%rbp), %r8
    movq -2(%r8), %rax
    movq 0(%rbp), %r8
    movq -1(%r8), %r9
    movq 8(%rbp), %r8
    movq -1(%r8), %r8
    addq $48, %rbp
    movq %r8, 0(%rbp)
    movq -24(%rbp), %r8
    leaq L70(%rip), %r15
    jmp *%rax
L70:
    subq $48, %rbp
    movq %rax, 32(%rbp)
    movq 16(%rbp), %r8
    movq 6(%r8), %r15
    movq 0(%rbp), %r8
    movq 7(%r8), %r9
    movq 8(%rbp), %r8
    movq 7(%r8), %r8
    addq $48, %rbp
    movq %r8, 8(%rbp)
    movq %r9, 0(%rbp)
    movq -24(%rbp), %r9
    movq %r15, %r8
    leaq L67(%rip), %r15
    jmp L9
L67:
    subq $48, %rbp
    movq %rax, %r15
    movq %rdx, %r8
    addq $16, %rdx
    addq $1, %r8
    movq %r8, %r9
    movq 32(%rbp), %r8
    movq %r8, -1(%r9)
    movq %r15, 7(%r9)
    movq %r9, %rax
    jmp *40(%rbp)
L78:
    subq $40, %rbp
    movq %rax, %r9
    movq 0(%rbp), %r8
    movq %r8, 8(%rbp)
    movq %r9, 0(%rbp)
    movq 16(%rbp), %r9
    movq 24(%rbp), %r8
    movq 32(%rbp), %r15
    jmp L9
_scheme_exit:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
