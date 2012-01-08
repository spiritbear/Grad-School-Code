(define invalid-tests
  '(3
    (begin rax 5)
    (letrec ()
      (set! rax 5))
    (letrec ()
      (set! rax 5)
      (r15))
    (letrec ()
      (begin
        (set! rax 5.5)
        (r15)))
    (letrec (["hello$55" (lambda () (r15))])
      (begin 
        (set! rax 5)
        (r15)))
    (letrec ([double$1 (lambda ()
                         (begin
                           (set! rax (+ rax rax))
                           (r15)))]
             [double$1 (lambda ()
                         (begin
                           (set! rdx (+ rdx rdx))
                           (set! rax rdx)
                           (r15)))])
      (begin
        (set! rax 10)
        (double$1)))
    (letrec ([double$1 (lambda ()
                         (begin
                           (set! rax (+ rax rax))
                           (sqr$1)))]
             [sqr$1 (lambda ()
                      (begin
                        (set! rax (* rax rax))
                        (r15)))])
      (begin
        (set! rax 2)
        (double$1)))
    (letrec ()
      (begin
        (set! fv0 5)
        (set! fv0 (* fv0 5))
        (set! rax fv0)
        (r15)))
    (letrec ([f$1 (lambda ()
                    (begin
                      (set! rax 5)
                      (r15)))])
      (begin
        (set! fv0 f$1)
        (fv0)))
    (letrec ([foo (lambda ()
                    (begin
                      (set! rax 5)
                      (r15)))])
      (foo))
    (letrec ([f$5 (lambda (rax)
                    (begin
                      (set! rax (+ rax 20))
                      (r15)))])
      (set! rax 10)
      (f$5))
    (letrec ([f$6 (lambda ()
                    (begin
                      (set! rax (* rax 100))
                      (r15)))])
      (f$6 5))
    (letrec ([f$7 (lambda (rdi)
                    (begin
                      (set! fv0 rdi)
                      (set! fv0 (+ fv0 10))
                      (set! rax fv0)
                      (r15)))])
      (f$7 6))
    (letrec ([foo$0 (lambda ()
                      (begin
                        (set! rax 5)
                        (r15)))])
      (bar$1))
    (letrec ([test-double$1 (lambda ()
                              (begin
                                (set! rdi 5)
                                (double$2)
                                (set! rax rdi)
                                (r15)))]
             [double$2 (lambda ()
                         (begin
                           (set! rdi (+ rdi rdi))
                           (r15)))])
      (test-double$1))
    (letrec ()
      (begin
        (set! x 5)
        (r15)))
    (letrec ()
      (begin 
        (set! rax 9223372036854775808)
        (r15)))
    (letrec ()
      (begin 
        (set! rax -9223372036854775809)
        (r15)))
    (letrec ()
      (begin 
        (set! rax (+ rax 2147483648))
        (r15)))
    (letrec ()
      (begin 
        (set! rax (+ rax -2147483649))
        (r15)))
    (letrec ()
      (begin 
        (set! fv0 2147483648)
        (r15)))
    (letrec ()
      (begin 
        (set! fv0 -2147483649)
        (r15)))
    (letrec ()
      (begin
        (set! fv0 12.5)
        (r15)))
    (letrec ()
      (letrec ()
        (begin
          (set! rax 5)
          (r15))))
    (letrec ()
      (begin
        (set! x 5)
        (r15)))
    (letrec ()
      (begin
        (set! rax 5)
        (set! rax (sra rax -1))
        (r15)))
    (letrec
      (begin
        (set! rax 5)
        (r15)))
    (letrec ()
      (begin
        (set! rax (sra rax 64))
        (r15)))
    (letrec ()
      (begin
        (set! rax (/ rax 5))
        (r15)))
    (letrec ()
      (begin
        (set! rdx (+ fv0 rdx))
        (r15)))
    (letrec ()
      (begin
        (set! fv0 1)
        (set! fv1 2)
        (set! fv0 (+ fv0 fv1))))
    (letrec ()
      (begin
        (set! fv0 1)
        (set! fv1 fv0)
        (r15)))
    (letrec ()
      (begin
        (set! fv0 1)
        (set! fv1 rax)))))

(define tests
  '((letrec () 
      (begin 
        (set! rax 5)
        (r15)))
    (letrec () 
      (begin 
        (set! rax 5)
        (set! rax (+ rax 5))
        (r15)))
    (letrec () 
      (begin 
        (set! rax 10)
        (set! rbx rax)
        (set! rax (- rax rbx))
        (r15)))
    (letrec ()
      (begin 
        (set! r11 5) 
        (set! rax r11)
        (r15)))
    (letrec () 
      (begin 
        (set! r11 10)
        (set! rax -10)
        (set! rax (* rax r11))
        (r15)))
    (letrec ()
      (begin 
        (set! r11 10) 
        (set! r11 (* r11 -10)) 
        (set! rax r11)
        (r15)))
    (letrec ()
      (begin 
        (set! rax 5)
        (set! rax (+ rax 10))
        (r15)))
    (letrec ()
      (begin 
        (set! r8 5)
        (set! rax 10)
        (set! rax (+ rax r8))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 7) 
        (set! rax (+ rax 4))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 7)
        (set! rax (- rax 4))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 7) 
        (set! rax (* rax 4))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 5)
        (set! rbx -11)
        (set! rax (+ rax rbx))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 5)
        (set! rbx -11)
        (set! rax (- rax rbx))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 5)
        (set! rbx -11)
        (set! rax (* rax rbx))
        (r15)))

    ;; some tests dealing with overflow
    (letrec ()
      (begin 
        (set! rax -9223372036854775808)
        (set! rax (- rax 5))
        (r15)))
    (letrec ()
      (begin
        (set! rax 9223372036854775807)
        (set! rax (+ rax 5))
        (r15)))
    (letrec ()
      (begin 
        (set! rax 1000000000000000000)
        (set! rax (* rax rax))
        (r15)))
    (letrec ()
      (begin
        (set! rax 1000000000000000000) 
        (set! rbx -1)
        (set! rbx (* rbx rax))
        (set! rax (* rax rbx))
        (r15)))

    ;; Factorial 5 - the long way.
    (letrec ()
      (begin 
        (set! rax 5)            
        (set! rbx 1)
        (set! rbx (* rbx rax))
        (set! rax (- rax 1))
        (set! rbx (* rbx rax))
        (set! rax (- rax 1))
        (set! rbx (* rbx rax))
        (set! rax (- rax 1))
        (set! rbx (* rbx rax))
        (set! rax rbx)
        (r15)))
    ;; Factorial 5 - the long way, nested begins
    (letrec ()
      (begin 
        (set! rax 5)
        (begin 
          (set! rbx 1)
          (begin
            (set! rbx (* rbx rax))
            (begin
              (set! rax (- rax 1))
              (begin
                (set! rbx (* rbx rax))
                (begin
                  (set! rax (- rax 1))
                  (begin
                    (set! rbx (* rbx rax))
                    (begin
                      (set! rax (- rax 1))
                      (begin
                        (set! rbx (* rbx rax))
                        (begin
                          (set! rax rbx)
                          (r15))))))))))))
    (letrec ([double$0 (lambda ()
                         (begin
                           (set! rax (+ rax rax))
                           (r15)))])
      (begin
        (set! rax 10)
        (double$0)))
    (letrec ([double$1 (lambda ()
                         (begin
                           (set! rax fv0)
                           (set! rax (* rax 2))
                           (set! fv0 rax)
                           (r15)))])
      (begin
        (set! fv0 5)
        (double$1)))
    (letrec ([div$0 (lambda ()
                      (begin
                        (set! fv2 (sra fv2 1))
                        (div$1)))]
             [div$1 (lambda ()
                      (begin
                        (set! rax fv2)
                        (fv0)))])
      (begin
        (set! fv0 r15)
        (set! rax div$0)
        (set! fv1 rax)
        (set! fv2 64)
        (fv1)))
    (letrec ([return$1 (lambda ()
                         (begin
                           (set! rax fv0)
                           (fv1)))]
             [setbit3$0 (lambda ()
                          (begin
                            (set! fv0 (logor fv0 8))
                            (return$1)))])
      (begin
        (set! fv0 1)
        (set! fv1 r15)
        (setbit3$0)))
    (letrec ([zero?$0 (lambda ()
                        (begin
                          (set! rdx 0)
                          (set! rdx (- rdx rax))
                          (set! rdx (sra rdx 63))
                          (set! rdx (logand rdx 1))
                          (return$1)))]
             [return$1 (lambda ()
                         (begin
                           (set! rax rdx)
                           (r15)))])
      (begin
        (set! rax 5)
        (zero?$0)))
    (letrec ([sqr-double$0 (lambda ()
                             (begin
                               (set! rdi (* rdi rdi))
                               (set! fv0 rdi)
                               (double$1)))]
             [double$1 (lambda ()
                         (begin
                           (set! rsi fv0)
                           (set! rsi (+ rsi fv0))
                           (return$3)))]
             [return$3 (lambda ()
                         (begin
                           (set! rax rsi)
                           (r15)))])
      (begin
        (set! rdi 5)
        (sqr-double$0)))
    (letrec ([main$1 (lambda ()
                       (begin
                         (set! rax 5)
                         (r15)))])
      (main$1))
    (letrec ([fact$0 (lambda ()
                       (begin
                        ; no if, so use a computed goto
                        ; put address of fact$1 at bfp[0]
                         (set! rcx fact$1)
                         (set! fv0 rcx)
                        ; put address of fact$2 at bfp[8]
                         (set! rcx fact$2)
                         (set! fv1 rcx)
                        ; if x == 0 set rcx to 8, else set rcx to 0
                         (set! rdx 0)
                         (set! rdx (- rdx rax))
                         (set! rdx (sra rdx 63))
                         (set! rdx (logand rdx 8))
                        ; point bfp at stored address of fact$1 or fact$2
                         (set! rbp (+ rbp rdx))
                        ; grab whichever and reset bfp
                         (set! rcx fv0)
                         (set! rbp (- rbp rdx))
                        ; tail call (jump to) fact$1 or fact$2
                         (rcx)))]
             [fact$1 (lambda ()
                       (begin
                        ; get here if rax is zero, so return 1
                         (set! rax 1)
                         (r15)))]
             [fact$2 (lambda ()
                       (begin
                        ; get here if rax is nonzero, so save return
                        ; address and eax, then call fact$0 recursively
                        ; with eax - 1, setting fact$3 as return point
                         (set! fv0 r15)
                         (set! fv1 rax)
                         (set! rax (- rax 1))
                         (set! r15 fact$3)
                        ; bump rbp by 16 (two 64-bit words) so that
                        ; recursive call doesn't wipe out our saved
                        ; eax and return address
                         (set! rbp (+ rbp 16))
                         (fact$0)))]
             [fact$3 (lambda ()
                       (begin
                        ; restore rbp to original value
                         (set! rbp (- rbp 16))
                        ; eax holds value of recursive call, multiply
                        ; by saved value at fv1 and return to saved
                        ; return address at fv0
                         (set! rax (* rax fv1))
                         (fv0)))])
      (begin
        (set! rax 10)
        (fact$0)))))

