    .globl _scheme_entry
_scheme_entry:
    pushq %rbx
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rdi, %rbp
    leaq _scheme_exit(%rip), %r15
    movq %r15, %rax
    movq $8, %r9
    movq $3, %r8
    movq %rax, %r15
L1:
    movq %r15, 8(%rbp)
    movq %r8, 0(%rbp)
    movq %r9, %r8
    cmpq $0, 0(%rbp)
    jne L17
L16:
    addq $1, %r8
    movq %r8, %rax
    jmp *8(%rbp)
L17:
    cmpq $0, %r8
    jne L15
L14:
    movq 0(%rbp), %r8
    subq $1, %r8
    movq $1, %r9
    movq 8(%rbp), %r15
    jmp L1
L15:
    subq $1, %r8
    addq $16, %rbp
    movq %r8, %r9
    movq -16(%rbp), %r8
    leaq L8(%rip), %r15
    jmp L1
L8:
    subq $16, %rbp
    movq %rax, %r9
    movq 0(%rbp), %r8
    subq $1, %r8
    movq 8(%rbp), %r15
    jmp L1
_scheme_exit:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
