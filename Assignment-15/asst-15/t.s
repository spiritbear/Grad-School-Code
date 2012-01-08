/*
(code
  (set! rcx r15)
  (set! rax rdx)
  (set! rdx (+ rdx 8))
  (set! rax (+ rax 2))
  (set! rbx rax)
  (set! rax f$1)
  (set! #<disp rbx -2> rax)
  (set! rsi rbx)
  (set! rax rdx)
  (set! rdx (+ rdx 24))
  (set! rax (+ rax 2))
  (set! rbx rax)
  (set! rax g$4)
  (set! #<disp rbx -2> rax)
  (set! #<disp rbx 6> rsi)
  (set! #<disp rbx 14> rbx)
  (set! rax #<disp rbx -2>)
  (set! r9 48)
  (set! r8 rbx)
  (set! r15 rcx)
  (jump rax)
  g$4
  (set! #<disp rbp 8> r15)
  (set! #<disp rbp 0> r8)
  (set! rsi r9)
  (if (= rsi 0) (jump c$49))
  (jump a$50)
  anon$6
  (set! #<disp rbp 8> r15)
  (set! rcx r8)
  (set! rsi r9)
  (set! rax #<disp rbp 0>)
  (set! rax #<disp rcx 6>)
  (set! #<disp rbp 0> rax)
  (set! rax #<disp rsi -2>)
  (set! rbx #<disp rcx 6>)
  (set! rbp (+ rbp 16))
  (set! r8 rsi)
  (set! r9 rbx)
  (set! r15 rp$32)
  (jump rax)
  f$1
  (set! rcx r15)
  (set! rax r8)
  (set! rbx r9)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 2))
  (set! rsi rax)
  (set! rax anon$6)
  (set! #<disp rsi -2> rax)
  (set! rax rsi)
  (set! #<disp rax 6> rbx)
  (jump rcx)
  c$49
  (set! rax 22)
  (jump #<disp rbp 8>)
  a$50
  (set! rax #<disp rbp 0>)
  (set! rbx #<disp rax 6>)
  (set! rcx #<disp rbx -2>)
  (set! rax #<disp rbp 0>)
  (set! rbx #<disp rax 6>)
  (set! rax rsi)
  (set! rax (- rax 8))
  (set! rbp (+ rbp 16))
  (set! r8 rbx)
  (set! r9 rax)
  (set! r15 rp$34)
  (jump rcx)
  rp$34
  (set! rbp (- rbp 16))
  (set! rbx rax)
  (set! rcx #<disp rbx -2>)
  (set! rax #<disp rbp 0>)
  (set! rsi #<disp rax 14>)
  (set! r9 rsi)
  (set! r8 rbx)
  (set! r15 #<disp rbp 8>)
  (jump rcx)
  rp$32
  (set! rbp (- rbp 16))
  (set! rcx rax)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! rbx rax)
  (set! rax #<disp rbp 0>)
  (set! #<disp rbx -1> rax)
  (set! #<disp rbp 0> rax)
  (set! #<disp rbx 7> rcx)
  (set! rax rbx)
  (jump #<disp rbp 8>))
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
    movq %rdx, %rax
    addq $8, %rdx
    addq $2, %rax
    movq %rax, %rbx
    leaq L1(%rip), %rax
    movq %rax, -2(%rbx)
    movq %rbx, %rsi
    movq %rdx, %rax
    addq $24, %rdx
    addq $2, %rax
    movq %rax, %rbx
    leaq L4(%rip), %rax
    movq %rax, -2(%rbx)
    movq %rsi, 6(%rbx)
    movq %rbx, 14(%rbx)
    movq -2(%rbx), %rax
    movq $48, %r9
    movq %rbx, %r8
    movq %rcx, %r15
    jmp *%rax
L4:
    movq %r15, 8(%rbp)
    movq %r8, 0(%rbp)
    movq %r9, %rsi
    cmpq $0, %rsi
    je L49
    jmp L50
L6:
    movq %r15, 8(%rbp)
    movq %r8, %rcx
    movq %r9, %rsi
    movq 0(%rbp), %rax
    movq 6(%rcx), %rax
    movq %rax, 0(%rbp)
    movq -2(%rsi), %rax
    movq 6(%rcx), %rbx
    addq $16, %rbp
    movq %rsi, %r8
    movq %rbx, %r9
    leaq L32(%rip), %r15
    jmp *%rax
L1:
    movq %r15, %rcx
    movq %r8, %rax
    movq %r9, %rbx
    movq %rdx, %rax
    addq $16, %rdx
    addq $2, %rax
    movq %rax, %rsi
    leaq L6(%rip), %rax
    movq %rax, -2(%rsi)
    movq %rsi, %rax
    movq %rbx, 6(%rax)
    jmp *%rcx
L49:
    movq $22, %rax
    jmp *8(%rbp)
L50:
    movq 0(%rbp), %rax
    movq 6(%rax), %rbx
    movq -2(%rbx), %rcx
    movq 0(%rbp), %rax
    movq 6(%rax), %rbx
    movq %rsi, %rax
    subq $8, %rax
    addq $16, %rbp
    movq %rbx, %r8
    movq %rax, %r9
    leaq L34(%rip), %r15
    jmp *%rcx
L34:
    subq $16, %rbp
    movq %rax, %rbx
    movq -2(%rbx), %rcx
    movq 0(%rbp), %rax
    movq 14(%rax), %rsi
    movq %rsi, %r9
    movq %rbx, %r8
    movq 8(%rbp), %r15
    jmp *%rcx
L32:
    subq $16, %rbp
    movq %rax, %rcx
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %rax, %rbx
    movq 0(%rbp), %rax
    movq %rax, -1(%rbx)
    movq %rax, 0(%rbp)
    movq %rcx, 7(%rbx)
    movq %rbx, %rax
    jmp *8(%rbp)
_scheme_exit:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
