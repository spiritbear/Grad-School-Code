/*
(code
  (set! r10 r15)
  (set! r9 4)
  (set! r8 2)
  (set! r15 r10)
  (jump ack$1)
  ack$1
  (set! #<disp rbp 8> r15)
  (set! #<disp rbp 0> r8)
  (set! r8 r9)
  (if (not (= #<disp rbp 0> 0)) (jump a$17))
  c$16
  (set! r8 (+ r8 1))
  (set! rax r8)
  (jump #<disp rbp 8>)
  a$17
  (if (not (= r8 0)) (jump a$15))
  c$14
  (set! r8 #<disp rbp 0>)
  (set! r8 (- r8 1))
  (set! r9 1)
  (set! r15 #<disp rbp 8>)
  (jump ack$1)
  a$15
  (set! r8 (- r8 1))
  (set! r9 r8)
  (set! rbp (+ rbp 16))
  (set! r8 #<disp rbp -16>)
  (set! r15 rp$8)
  (jump ack$1)
  rp$8
  (set! rbp (- rbp 16))
  (set! r9 rax)
  (set! r8 #<disp rbp 0>)
  (set! r8 (- r8 1))
  (set! r15 #<disp rbp 8>)
  (jump ack$1))
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
    leaq _scheme_exit(%rip), %r15
    movq %r15, %r10
    movq $4, %r9
    movq $2, %r8
    movq %r10, %r15
    jmp L1
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
    movq %r8, %r9
    addq $16, %rbp
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
