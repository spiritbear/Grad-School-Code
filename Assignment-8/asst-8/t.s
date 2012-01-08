/*
(code
  (set! rcx r15)
  (set! rbx 1)
  (set! rsi 2)
  (set! rdi 3)
  (set! r8 4)
  (set! r9 5)
  (set! r10 6)
  (set! r11 7)
  (set! r12 8)
  (set! r13 9)
  (set! r14 10)
  (set! r15 11)
  (set! #<disp rbp 0> 12)
  (set! #<disp rbp 8> 13)
  (set! #<disp rbp 16> 14)
  (set! #<disp rbp 24> 15)
  (set! #<disp rbp 32> 16)
  (set! #<disp rbp 40> 17)
  (set! #<disp rbp 48> 18)
  (set! #<disp rbp 56> 19)
  (set! #<disp rbp 64> 20)
  (set! #<disp rbp 72> 21)
  (set! #<disp rbp 80> 22)
  (set! #<disp rbp 88> 23)
  (set! #<disp rbp 96> 24)
  (set! rax 25)
  (set! #<disp rbp 104> 26)
  (set! rax (+ rax #<disp rbp 104>))
  (set! #<disp rbp 104> rax)
  (set! rax #<disp rbp 96>)
  (set! rax (+ rax #<disp rbp 104>))
  (set! #<disp rbp 96> rax)
  (set! rax #<disp rbp 88>)
  (set! rax (+ rax #<disp rbp 96>))
  (set! #<disp rbp 88> rax)
  (set! rax #<disp rbp 80>)
  (set! rax (+ rax #<disp rbp 88>))
  (set! #<disp rbp 80> rax)
  (set! rax #<disp rbp 72>)
  (set! rax (+ rax #<disp rbp 80>))
  (set! #<disp rbp 72> rax)
  (set! rax #<disp rbp 64>)
  (set! rax (+ rax #<disp rbp 72>))
  (set! #<disp rbp 64> rax)
  (set! rax #<disp rbp 56>)
  (set! rax (+ rax #<disp rbp 64>))
  (set! #<disp rbp 56> rax)
  (set! rax #<disp rbp 48>)
  (set! rax (+ rax #<disp rbp 56>))
  (set! #<disp rbp 48> rax)
  (set! rax #<disp rbp 40>)
  (set! rax (+ rax #<disp rbp 48>))
  (set! #<disp rbp 40> rax)
  (set! rax #<disp rbp 32>)
  (set! rax (+ rax #<disp rbp 40>))
  (set! #<disp rbp 32> rax)
  (set! rax #<disp rbp 24>)
  (set! rax (+ rax #<disp rbp 32>))
  (set! #<disp rbp 24> rax)
  (set! rax #<disp rbp 16>)
  (set! rax (+ rax #<disp rbp 24>))
  (set! #<disp rbp 16> rax)
  (set! rax #<disp rbp 8>)
  (set! rax (+ rax #<disp rbp 16>))
  (set! #<disp rbp 8> rax)
  (set! rax #<disp rbp 0>)
  (set! rax (+ rax #<disp rbp 8>))
  (set! #<disp rbp 0> rax)
  (set! rax r15)
  (set! rax (+ rax #<disp rbp 0>))
  (set! r15 rax)
  (set! rax r14)
  (set! rax (+ rax r15))
  (set! r14 rax)
  (set! rax r13)
  (set! rax (+ rax r14))
  (set! r13 rax)
  (set! rax r12)
  (set! rax (+ rax r13))
  (set! r12 rax)
  (set! rax r11)
  (set! rax (+ rax r12))
  (set! r11 rax)
  (set! rax r10)
  (set! rax (+ rax r11))
  (set! r10 rax)
  (set! rax r9)
  (set! rax (+ rax r10))
  (set! r9 rax)
  (set! rax r8)
  (set! rax (+ rax r9))
  (set! r8 rax)
  (set! rax rdi)
  (set! rax (+ rax r8))
  (set! rdi rax)
  (set! rax rsi)
  (set! rax (+ rax rdi))
  (set! rbx (+ rbx rax))
  (set! rsi 27)
  (set! rdi 28)
  (set! r8 29)
  (set! r9 30)
  (set! r10 31)
  (set! r11 32)
  (set! r12 33)
  (set! r13 34)
  (set! r14 35)
  (set! r15 36)
  (set! #<disp rbp 0> 37)
  (set! #<disp rbp 8> 38)
  (set! #<disp rbp 16> 39)
  (set! #<disp rbp 24> 40)
  (set! rax #<disp rbp 16>)
  (set! rax (+ rax #<disp rbp 24>))
  (set! #<disp rbp 16> rax)
  (set! rax #<disp rbp 8>)
  (set! rax (+ rax #<disp rbp 16>))
  (set! #<disp rbp 8> rax)
  (set! rax #<disp rbp 0>)
  (set! rax (+ rax #<disp rbp 8>))
  (set! #<disp rbp 0> rax)
  (set! rax r15)
  (set! rax (+ rax #<disp rbp 0>))
  (set! r15 rax)
  (set! rax r14)
  (set! rax (+ rax r15))
  (set! r14 rax)
  (set! rax r13)
  (set! rax (+ rax r14))
  (set! r13 rax)
  (set! rax r12)
  (set! rax (+ rax r13))
  (set! r12 rax)
  (set! rax r11)
  (set! rax (+ rax r12))
  (set! r11 rax)
  (set! rax r10)
  (set! rax (+ rax r11))
  (set! r10 rax)
  (set! rax r9)
  (set! rax (+ rax r10))
  (set! r9 rax)
  (set! rax r8)
  (set! rax (+ rax r9))
  (set! r8 rax)
  (set! rax rdi)
  (set! rax (+ rax r8))
  (set! rdi rax)
  (set! rax rsi)
  (set! rax (+ rax rdi))
  (set! rbx (+ rbx rax))
  (set! rax rbx)
  (jump rcx))
*/


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
    movq %r15, %rcx
    movq $1, %rbx
    movq $2, %rsi
    movq $3, %rdi
    movq $4, %r8
    movq $5, %r9
    movq $6, %r10
    movq $7, %r11
    movq $8, %r12
    movq $9, %r13
    movq $10, %r14
    movq $11, %r15
    movq $12, 0(%rbp)
    movq $13, 8(%rbp)
    movq $14, 16(%rbp)
    movq $15, 24(%rbp)
    movq $16, 32(%rbp)
    movq $17, 40(%rbp)
    movq $18, 48(%rbp)
    movq $19, 56(%rbp)
    movq $20, 64(%rbp)
    movq $21, 72(%rbp)
    movq $22, 80(%rbp)
    movq $23, 88(%rbp)
    movq $24, 96(%rbp)
    movq $25, %rax
    movq $26, 104(%rbp)
    addq 104(%rbp), %rax
    movq %rax, 104(%rbp)
    movq 96(%rbp), %rax
    addq 104(%rbp), %rax
    movq %rax, 96(%rbp)
    movq 88(%rbp), %rax
    addq 96(%rbp), %rax
    movq %rax, 88(%rbp)
    movq 80(%rbp), %rax
    addq 88(%rbp), %rax
    movq %rax, 80(%rbp)
    movq 72(%rbp), %rax
    addq 80(%rbp), %rax
    movq %rax, 72(%rbp)
    movq 64(%rbp), %rax
    addq 72(%rbp), %rax
    movq %rax, 64(%rbp)
    movq 56(%rbp), %rax
    addq 64(%rbp), %rax
    movq %rax, 56(%rbp)
    movq 48(%rbp), %rax
    addq 56(%rbp), %rax
    movq %rax, 48(%rbp)
    movq 40(%rbp), %rax
    addq 48(%rbp), %rax
    movq %rax, 40(%rbp)
    movq 32(%rbp), %rax
    addq 40(%rbp), %rax
    movq %rax, 32(%rbp)
    movq 24(%rbp), %rax
    addq 32(%rbp), %rax
    movq %rax, 24(%rbp)
    movq 16(%rbp), %rax
    addq 24(%rbp), %rax
    movq %rax, 16(%rbp)
    movq 8(%rbp), %rax
    addq 16(%rbp), %rax
    movq %rax, 8(%rbp)
    movq 0(%rbp), %rax
    addq 8(%rbp), %rax
    movq %rax, 0(%rbp)
    movq %r15, %rax
    addq 0(%rbp), %rax
    movq %rax, %r15
    movq %r14, %rax
    addq %r15, %rax
    movq %rax, %r14
    movq %r13, %rax
    addq %r14, %rax
    movq %rax, %r13
    movq %r12, %rax
    addq %r13, %rax
    movq %rax, %r12
    movq %r11, %rax
    addq %r12, %rax
    movq %rax, %r11
    movq %r10, %rax
    addq %r11, %rax
    movq %rax, %r10
    movq %r9, %rax
    addq %r10, %rax
    movq %rax, %r9
    movq %r8, %rax
    addq %r9, %rax
    movq %rax, %r8
    movq %rdi, %rax
    addq %r8, %rax
    movq %rax, %rdi
    movq %rsi, %rax
    addq %rdi, %rax
    addq %rax, %rbx
    movq $27, %rsi
    movq $28, %rdi
    movq $29, %r8
    movq $30, %r9
    movq $31, %r10
    movq $32, %r11
    movq $33, %r12
    movq $34, %r13
    movq $35, %r14
    movq $36, %r15
    movq $37, 0(%rbp)
    movq $38, 8(%rbp)
    movq $39, 16(%rbp)
    movq $40, 24(%rbp)
    movq 16(%rbp), %rax
    addq 24(%rbp), %rax
    movq %rax, 16(%rbp)
    movq 8(%rbp), %rax
    addq 16(%rbp), %rax
    movq %rax, 8(%rbp)
    movq 0(%rbp), %rax
    addq 8(%rbp), %rax
    movq %rax, 0(%rbp)
    movq %r15, %rax
    addq 0(%rbp), %rax
    movq %rax, %r15
    movq %r14, %rax
    addq %r15, %rax
    movq %rax, %r14
    movq %r13, %rax
    addq %r14, %rax
    movq %rax, %r13
    movq %r12, %rax
    addq %r13, %rax
    movq %rax, %r12
    movq %r11, %rax
    addq %r12, %rax
    movq %rax, %r11
    movq %r10, %rax
    addq %r11, %rax
    movq %rax, %r10
    movq %r9, %rax
    addq %r10, %rax
    movq %rax, %r9
    movq %r8, %rax
    addq %r9, %rax
    movq %rax, %r8
    movq %rdi, %rax
    addq %r8, %rax
    movq %rax, %rdi
    movq %rsi, %rax
    addq %rdi, %rax
    addq %rax, %rbx
    movq %rbx, %rax
    jmp *%rcx
_scheme_exit:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
