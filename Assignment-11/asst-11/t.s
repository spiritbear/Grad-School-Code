/*
(code
  (set! #<disp rbp 0> r15)
  (set! rcx 8)
  (set! rbx 16)
  (set! rsi 24)
  (set! rdi 32)
  (set! r9 40)
  (set! r8 22)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! #<disp rax -1> r9)
  (set! #<disp rax 7> r8)
  (set! r8 rax)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! #<disp rax -1> rdi)
  (set! #<disp rax 7> r8)
  (set! rdi rax)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! #<disp rax -1> rsi)
  (set! #<disp rax 7> rdi)
  (set! rsi rax)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! #<disp rax -1> rbx)
  (set! #<disp rax 7> rsi)
  (set! rbx rax)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! #<disp rax -1> rcx)
  (set! #<disp rax 7> rbx)
  (set! #<disp rbp 8> rax)
  (set! rbp (+ rbp 16))
  (set! r8 #<disp rbp -8>)
  (set! r15 rp$43)
  (jump num-list?$0)
  num-list?$0
  (set! rcx r15)
  (set! rbx r8)
  (if (= rbx 22) (jump c$72))
  (jump a$73)
  map$1
  (set! #<disp rbp 8> r15)
  (set! rcx r8)
  (set! #<disp rbp 0> r9)
  (if (= #<disp rbp 0> 22) (jump c$68))
  (jump a$69)
  square$2
  (set! rcx r15)
  (set! rsi r8)
  (set! rax rsi)
  (set! rax (sra rax 3))
  (set! rbx rax)
  (set! rax rsi)
  (set! rax (* rax rbx))
  (jump rcx)
  c$72
  (set! rax 14)
  (jump rcx)
  a$73
  (set! rax #<disp rbx -1>)
  (set! rax (logand rax 7))
  (if (not (= rax 0)) (jump a$71))
  c$70
  (set! rax #<disp rbx 7>)
  (set! r8 rax)
  (set! r15 rcx)
  (jump num-list?$0)
  a$71
  (set! rax 6)
  (jump rcx)
  c$68
  (set! rax 22)
  (jump #<disp rbp 8>)
  a$69
  (set! rax #<disp rbp 0>)
  (set! rbx #<disp rax -1>)
  (set! rbp (+ rbp 16))
  (set! r8 rbx)
  (set! r15 rp$39)
  (jump rcx)
  rp$39
  (set! rbp (- rbp 16))
  (set! rbx rax)
  (set! rax #<disp rbp 0>)
  (set! rcx #<disp rax 7>)
  (set! rax rdx)
  (set! rdx (+ rdx 16))
  (set! rax (+ rax 1))
  (set! #<disp rax -1> rbx)
  (set! #<disp rax 7> rcx)
  (jump #<disp rbp 8>)
  rp$43
  (set! rbp (- rbp 16))
  (if (not (= rax 6)) (jump a$67))
  c$66
  (set! rax 6)
  (if (= rax 6) (jump j$61))
  (jump c$59)
  a$67
  (set! rax 14)
  (if (= rax 6) (jump j$61))
  c$59
  (set! rbp (+ rbp 16))
  (set! r8 square$2)
  (set! r9 #<disp rbp -8>)
  (set! r15 rp$42)
  (jump map$1)
  j$61
  (set! rax #<disp rbp 8>)
  (jump #<disp rbp 0>)
  rp$42
  (set! rbx rax)
  (set! rcx #<disp rbp -8>)
  (set! #<disp rcx -1> rbx)
  (set! #<disp rbp -8> rcx)
  (set! rbp (- rbp 16))
  (set! rbx rax)
  (set! rax #<disp rbp 8>)
  (set! #<disp rax -1> rbx)
  (set! #<disp rbp 8> rax)
  (jump j$61))
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
    movq %r15, 0(%rbp)
    movq $8, %rcx
    movq $16, %rbx
    movq $24, %rsi
    movq $32, %rdi
    movq $40, %r9
    movq $22, %r8
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %r9, -1(%rax)
    movq %r8, 7(%rax)
    movq %rax, %r8
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %rdi, -1(%rax)
    movq %r8, 7(%rax)
    movq %rax, %rdi
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %rsi, -1(%rax)
    movq %rdi, 7(%rax)
    movq %rax, %rsi
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %rbx, -1(%rax)
    movq %rsi, 7(%rax)
    movq %rax, %rbx
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %rcx, -1(%rax)
    movq %rbx, 7(%rax)
    movq %rax, 8(%rbp)
    addq $16, %rbp
    movq -8(%rbp), %r8
    leaq L43(%rip), %r15
    jmp L0
L0:
    movq %r15, %rcx
    movq %r8, %rbx
    cmpq $22, %rbx
    je L72
    jmp L73
L1:
    movq %r15, 8(%rbp)
    movq %r8, %rcx
    movq %r9, 0(%rbp)
    cmpq $22, 0(%rbp)
    je L68
    jmp L69
L2:
    movq %r15, %rcx
    movq %r8, %rsi
    movq %rsi, %rax
    sarq $3, %rax
    movq %rax, %rbx
    movq %rsi, %rax
    imulq %rbx, %rax
    jmp *%rcx
L72:
    movq $14, %rax
    jmp *%rcx
L73:
    movq -1(%rbx), %rax
    andq $7, %rax
    cmpq $0, %rax
    jne L71
L70:
    movq 7(%rbx), %rax
    movq %rax, %r8
    movq %rcx, %r15
    jmp L0
L71:
    movq $6, %rax
    jmp *%rcx
L68:
    movq $22, %rax
    jmp *8(%rbp)
L69:
    movq 0(%rbp), %rax
    movq -1(%rax), %rbx
    addq $16, %rbp
    movq %rbx, %r8
    leaq L39(%rip), %r15
    jmp *%rcx
L39:
    subq $16, %rbp
    movq %rax, %rbx
    movq 0(%rbp), %rax
    movq 7(%rax), %rcx
    movq %rdx, %rax
    addq $16, %rdx
    addq $1, %rax
    movq %rbx, -1(%rax)
    movq %rcx, 7(%rax)
    jmp *8(%rbp)
L43:
    subq $16, %rbp
    cmpq $6, %rax
    jne L67
L66:
    movq $6, %rax
    cmpq $6, %rax
    je L61
    jmp L59
L67:
    movq $14, %rax
    cmpq $6, %rax
    je L61
L59:
    addq $16, %rbp
    leaq L2(%rip), %r8
    movq -8(%rbp), %r9
    leaq L42(%rip), %r15
    jmp L1
L61:
    movq 8(%rbp), %rax
    jmp *0(%rbp)
L42:
    movq %rax, %rbx
    movq -8(%rbp), %rcx
    movq %rbx, -1(%rcx)
    movq %rcx, -8(%rbp)
    subq $16, %rbp
    movq %rax, %rbx
    movq 8(%rbp), %rax
    movq %rbx, -1(%rax)
    movq %rax, 8(%rbp)
    jmp L61
_scheme_exit:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
