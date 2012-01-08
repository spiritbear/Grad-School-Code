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
        (set! rax 5)
        (r15)))
    (letrec ()
      (locate ()
        (begin
          (set! rax 5.5)
          (r15))))
    (letrec ([double$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rax (+ rax rax))
                             (r15))))]
             [double$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rdx (+ rdx rdx))
                             (set! rax rdx)
                             (r15))))])
      (locate ()
        (begin
          (set! rax 10)
          (double$1))))
    (letrec ([double$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rax (+ rax rax))
                             (sqr$1))))]
             [sqr$1 (lambda ()
                      (locate ()
                        (begin
                          (set! rax (* rax rax))
                          (r15))))])
      (locate ()
        (begin
          (set! rax 2)
          (double$1))))
    (letrec ()
      (locate ()
        (begin
          (set! fv0 5)
          (set! fv0 (* fv0 5))
          (set! rax fv0)
          (r15))))
    (letrec ([f$1 (lambda ()
                    (locate ()
                      (begin
                        (set! rax 5)
                        (r15))))])
      (locate ()
        (begin
          (set! fv0 f$1)
          (fv0))))
    (letrec ([foo (lambda ()
                    (locate ()
                      (begin
                        (set! rax 5)
                        (r15))))])
      (locate () (foo)))
    (letrec ([f$5 (lambda (rax)
                    (locate ()
                      (begin
                        (set! rax (+ rax 20))
                        (r15))))])
      (locate ()
        (set! rax 10)
        (f$5)))
    (letrec ([f$6 (lambda ()
                    (locate ()
                      (begin
                        (set! rax (* rax 100))
                        (r15))))])
      (locate () (f$6 5)))
    (letrec ([f$7 (lambda (rdi)
                    (locate ()
                      (begin
                        (set! fv0 rdi)
                        (set! fv0 (+ fv0 10))
                        (set! rax fv0)
                        (r15))))])
      (locate () (f$7 6)))
    (letrec ([foo$0 (lambda ()
                      (locate () (begin (set! rax 5) (r15))))])
      (locate () (bar$1)))
    (letrec ([test-double$1 (lambda ()
                              (locate ()
                                (begin
                                  (set! rdi 5)
                                  (double$2)
                                  (set! rax rdi)
                                  (r15))))]
             [double$2 (lambda ()
                         (locate ()
                           (begin
                             (set! rdi (+ rdi rdi))
                             (r15))))])
      (locate () (test-double$1)))
    (letrec ()
      (locate ()
        (begin
          (set! x 5)
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 9223372036854775808)
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax -9223372036854775809)
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax (+ rax 2147483648))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax (+ rax -2147483649))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
        (set! fv0 2147483648)
        (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! fv0 -2147483649)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! fv0 12.5)
          (r15))))
    (letrec ()
      (letrec ()
        (locate ()
          (begin
            (set! rax 5)
            (r15)))))
    (letrec ()
      (locate ()
        (begin
          (set! x 5)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 5)
          (set! rax (sra rax -1))
          (r15))))
    (letrec
      (locate ()
        (begin
          (set! rax 5)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax (sra rax 64))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax (/ rax 5))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rdx (+ fv0 rdx))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax r15)
          (if (+ rcx 5)
            (rax)
            (r15)))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (if (= rax 0) (nop) (r15))
          (set! rax 5)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (if (begin (set! rax 10) (r15))
              (r15)
              (r15)))))
    (letrec ()
      (locate ()
        (begin
          (if (begin (set! rax 10) (nop))
              (r15)
              (r15)))))
    (letrec ()
      (locate ()
        (begin
          (if (begin (set! rax 10) (set! r11 10) (= rax r11))
              (set! rax 10)
              (r15)))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (if (if (= rax 0) (nop) (= rbx 0))
              (set! rax 10)
              (set! rbx 10))
          (r15))))
    (letrec ([main$0 (lambda ()
                       (locate ()
                         (begin
                           (set! rax r15)
                           (if (+ rcx 5)
                             (rax)
                             (r15)))))])
      (locate () (main$0)))
    (letrec ([main$0 (lambda ()
                       (locate ()
                         (begin
                           (set! rax 0)
                           (if (= rax 0) (nop) (r15))
                           (set! rax 5)
                           (r15))))])
      (locate () (main$0)))
    (letrec ()
      (locate ()
        (begin
          (if (begin (set! rax 10) (r15))
              (r15)
              (r15)))))
    (letrec ([main$0 (lambda ()
                       (locate ()
                         (begin
                           (if (begin (set! rax 10) (nop))
                               (r15)
                               (r15)))))])
      (locate () (main$0)))
    (letrec ([main$0 (lambda ()
                       (locate ()
                         (begin
                           (if (begin 
                                 (set! rax 10) 
                                 (set! r11 10)
                                 (= rax r11))
                               (set! rax 10)
                               (r15)))))])
      (locate () (main$0)))
    (letrec ([main$0 (lambda ()
                       (locate ()
                         (begin
                           (set! rax 0)
                           (if (if (= rax 0) (nop) (= rbx 0))
                               (set! rax 10)
                               (set! rbx 10))
                           (r15))))])
      (locate () (main$0)))
    (letrec ([main$0 (lambda () (locate () (if (= 0 rax) (f$0) (f$1))))]
             [f$0 (lambda () (locate () (begin (set! rax 5) (r15))))]
             [f$1 (lambda () (locate () (begin (set! rax 6) (r15))))])
      (locate () (main$0)))
    (letrec () (locate () (if (= rax 2147483648) (r15) (r15))))
    (letrec () (locate () (if (= fv0 -2147483649) (r15) (r15))))
    (letrec () (locate () (if (= fv0 12.5) (r15) (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 7)
          (if (if (< rax 10) (if (< rax 5) (false) (true)) #f)
              (begin
                (set! rax (* rax rax))
                (r15))
              (r15)))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 10)
          (if (< 0 rax)
              (begin
                (set! rax (+ rax 10))
                (r15))
              (r15)))))
    (letrec ()
      (locate ([x.5 rgx])
        (begin
          (set! x.5 (+ x.5 x.5))
          (r15))))
    (letrec ()
      (locate ([x rax])
        (begin
          (set! x (+ x x))
          (r15))))
    (letrec ()
      (locate ([x.5 rax])
        (begin
          (set! x.5 (+ x.5 x.6))
          (r15))))
    (letrec ()
      (locate ([x.5 fv0])
        (begin
          (set! x.5 6)
          (if (= x.5 x.5)
              (r15)
              (r15)))))))

(define tests
  '((letrec () 
      (locate ()
        (begin 
          (set! rax 5)
          (r15))))
    (letrec () 
      (locate ()
        (begin 
          (set! rax 5)
          (set! rax (+ rax 5))
          (r15))))
    (letrec () 
      (locate ()
        (begin 
          (set! rax 10)
          (set! rbx rax)
          (set! rax (- rax rbx))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! r11 5) 
          (set! rax r11)
          (r15))))
    (letrec () 
      (locate ()
        (begin 
          (set! r11 10)
          (set! rax -10)
          (set! rax (* rax r11))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! r11 10) 
          (set! r11 (* r11 -10)) 
          (set! rax r11)
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 5)
          (set! rax (+ rax 10))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! r8 5)
          (set! rax 10)
          (set! rax (+ rax r8))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 7) 
          (set! rax (+ rax 4))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 7)
          (set! rax (- rax 4))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 7) 
          (set! rax (* rax 4))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 5)
          (set! rbx -11)
          (set! rax (+ rax rbx))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 5)
          (set! rbx -11)
          (set! rax (- rax rbx))
          (r15))))
    (letrec ()
      (locate ()
        (begin 
          (set! rax 5)
          (set! rbx -11)
          (set! rax (* rax rbx))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 5)
          (set! rcx 10)
          (if (< rax rcx)
            (r15)
            (begin
              (set! rax rcx)
              (r15))))))
    (letrec ()
      (locate ()
        (begin
        (set! rax 5)
        (if (< rax 10) (set! rax (* rax 10)) (nop))
        (r15))))
      
    ;; Factorial 5 - the long way.
    (letrec ()
      (locate ()
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
          (r15))))
    ;; Factorial 5 - the long way, nested begins
    (letrec ()
      (locate ()
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
                            (r15)))))))))))))
    (letrec ([double$0 (lambda ()
                         (locate ()
                           (begin
                             (set! rax (+ rax rax))
                             (r15))))])
      (locate ()
        (begin
          (set! rax 10)
          (double$0))))
    (letrec ([double$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rax fv0)
                             (set! rax (* rax 2))
                             (set! fv0 rax)
                             (r15))))])
      (locate ()
        (begin
          (set! fv0 5)
          (double$1))))
    (letrec ()
      (locate ([x.5 rax])
        (begin
          (set! x.5 5)
          (r15))))
    (letrec ()
      (locate ([x.5 rax] [y.6 rbx])
        (begin
          (set! x.5 5)
          (set! y.6 6)
          (set! x.5 (+ x.5 y.6))
          (r15))))
    (letrec ([div$0 (lambda ()
                      (locate ()
                        (begin
                          (set! fv2 (sra fv2 1))
                          (div$1))))]
             [div$1 (lambda ()
                      (locate ()
                        (begin
                          (set! rax fv2)
                          (fv0))))])
      (locate ()
        (begin
          (set! fv0 r15)
          (set! rax div$0)
          (set! fv1 rax)
          (set! fv2 64)
          (fv1))))
    (letrec ([setbit3$0 (lambda ()
                          (locate ()
                            (begin
                              (set! fv0 (logor fv0 8))
                              (return$1))))]
             [return$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rax fv0)
                             (fv1))))])
      (locate ()
        (begin
          (set! fv0 1)
          (set! fv1 r15)
          (setbit3$0))))
    (letrec ([zero?$0 (lambda ()
                        (locate ()
                          (begin
                            (set! rdx 0)
                            (set! rdx (- rdx rax))
                            (set! rdx (sra rdx 63))
                            (set! rdx (logand rdx 1))
                            (return$1))))]
             [return$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rax rdx)
                             (r15))))])
      (locate ()
        (begin
          (set! rax 5)
          (zero?$0))))
    (letrec ([sqr-double$0 (lambda ()
                             (locate ()
                               (begin
                                 (set! rdi (* rdi rdi))
                                 (set! fv0 rdi)
                                 (double$1))))]
             [double$1 (lambda ()
                         (locate ()
                           (begin
                             (set! rsi fv0)
                             (set! rsi (+ rsi fv0))
                             (return$3))))]
             [return$3 (lambda ()
                         (locate ()
                           (begin
                             (set! rax rsi)
                             (r15))))])
      (locate ()
        (begin
          (set! rdi 5)
          (sqr-double$0))))
    (letrec ([main$1 (lambda ()
                       (locate ()
                         (begin
                         (set! rax 5)
                         (r15))))])
      (locate () (main$1)))
    (letrec ([if-test$1 (lambda ()
                          (locate ()
                            (begin
                              (if (begin (set! rax 5) (= rax 5))
                                  (set! rax (+ rax 10))
                                  (set! rax (- rax 10)))
                              (set! rax (* rax 10))
                              (r15))))])
      (locate () (if-test$1)))
    (letrec ([if-test$2 (lambda ()
                          (locate ()
                            (begin
                              (if (begin 
                                    (set! rax 7) 
                                    (if (< rax 1) (false) (< rax 10)))
                                  (set! rax (* rax 2))
                                  (set! rax (+ rax rax)))
                              (r15))))])
      (locate () (if-test$2)))
    (letrec ([if-test$3 (lambda ()
                          (locate ()
                            (begin
                              (set! rax 2)
                              (if (if (= rax 0)
                                      (true)
                                      (if (= rax 1)
                                          (true)
                                          (= rax 2)))
                                   (begin
                                     (set! rax (* rax 5))
                                     (r15))
                                   (begin
                                     (set! rax (- rax 5))
                                     (r15))))))])
      (locate () (if-test$3)))
    (letrec ([if-test$4 (lambda ()
                          (locate ()
                            (begin
                              (set! rax 2)
                              (if (if (= rax 10) (false) (true))
                                  (set! rax (+ rax 10))
                                  (set! rax (- rax 2)))
                              (set! rax (* rax 10))
                              (r15))))])
      (locate () (if-test$4)))
    (letrec ([if-test$5 (lambda ()
                          (locate ([n.1 rdi] [x.2 rax] [y.3 rbx])
                            (begin
                              (set! x.2 1)
                              (set! y.3 1)
                              (if (= n.1 0)
                                  (set! x.2 (+ x.2 y.3))
                                  (set! y.3 (+ y.3 x.2)))
                              (set! x.2 n.1)
                              (if (if (= n.1 y.3) (false) (true))
                                  (set! n.1 (+ n.1 x.2))
                                  (set! n.1 (+ n.1 y.3)))
                              (set! x.2 n.1)
                              (r15))))])
      (locate ([n.1 rdi])
        (begin
          (set! n.1 1)
          (if-test$5))))
    (letrec ([if-test$6 (lambda ()
                          (locate ([n.1 rdi] [x.2 rax] [y.3 rbx])
                            (begin
                              (set! x.2 1)
                              (begin
                                (set! y.3 1)
                                (if (= n.1 0)
                                    (set! x.2 (+ x.2 y.3))
                                    (set! y.3 (+ y.3 x.2)))
                                (set! x.2 n.1))
                              (if (if (= n.1 y.3) (false) (true))
                                  (set! n.1 (+ n.1 x.2))
                                  (set! n.1 (+ n.1 y.3)))
                              (set! x.2 n.1)
                              (r15))))])
      (locate ([n.1 rdi])
        (begin
          (set! n.1 1)
          (if-test$6))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (set! rbx 1)
          (if (if (= rax 0) (= rbx 1) (false))
            (set! rdx 5)
            (begin
              (set! rdx 5)
              (set! rdx (+ rdx rdx))))
          (set! rax rdx)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (set! rbx 0)
          (if (if (= rax 0) (= rbx 1) (false))
            (set! rdx 5)
            (begin
              (set! rdx 5)
              (set! rdx (+ rdx rdx))))
          (set! rax rdx)
          (r15))))
    (letrec () 
      (locate () 
        (begin 
          (set! rax 0)
          (set! rbx 2)
          (if (= rax 0) 
              (if (= rbx 2) 
                  (set! rax rbx)
                  (set! rax 5)) 
              (set! rax 7))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (set! rbx 0)
          (if (if (= rax 1) (true) (= rbx 1))
              (set! rdx 1)
              (set! rdx 0))
          (set! rax rdx)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 1)
          (set! rbx 0)
          (if (if (= rax 1) (true) (= rbx 1))
              (set! rdx 1)
              (set! rdx 0))
          (set! rax rdx)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (set! rbx 1)
          (if (if (= rax 1) (true) (= rbx 1))
              (set! rdx 1)
              (set! rdx 0))
          (set! rax rdx)
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (set! rbx 1)
          (if (if (= rax 1) (= rbx 1) (true))
              (set! rax 1)
              (set! rax 0))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 1)
          (set! rbx 0)
          (if (if (= rax 1) (= rbx 1) (true))
              (set! rax 1)
              (set! rax 0))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 0)
          (set! rbx 0)
          (if (if (= rax 1) (= rbx 1) (true))
              (set! rax 1)
              (set! rax 0))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 1)
          (set! rbx 1)
          (if (if (= rax 1) (= rbx 1) (true))
              (set! rax 1)
              (set! rax 0))
          (r15))))
    (letrec ()
      (locate ()
        (begin
          (set! rax 1)
          (begin
            (set! rbx 2)
            (begin
              (set! rcx 3)
              (begin
                (set! rdx rax)
                (set! rdx (+ rdx rbx))
                (if (= rdx rcx)
                    (begin (set! rdx 5))
                    (begin (set! rdx 10)))
                (set! rax (+ rax rdx)))
              (set! rax (+ rax rcx)))
            (set! rax (+ rax rbx)))
          (set! rax (+ rax rax))
          (r15))))
    (letrec ([fact$0 (lambda ()
                       (locate ()
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
                           (rcx))))]
             [fact$1 (lambda ()
                       (locate ()
                         (begin
                           ; get here if rax is zero, so return 1
                           (set! rax 1)
                           (r15))))]
             [fact$2 (lambda ()
                       (locate ()
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
                           (fact$0))))]
             [fact$3 (lambda ()
                       (locate ()
                         (begin
                           ; restore rbp to original value
                           (set! rbp (- rbp 16))
                           ; eax holds value of recursive call, multiply
                           ; by saved value at fv1 and return to saved
                           ; return address at fv0
                           (set! rax (* rax fv1))
                           (fv0))))])
      (locate ()
        (begin
          (set! rax 10)
          (fact$0))))
    (letrec ([fact$0 (lambda ()
                       (locate ([n.1 rax] [a.2 rbx])
                         (begin
                           ;; no non-tail calls, so setup and accumulator
                           (set! a.2 1)
                           (fact$1))))]
             [fact$1 (lambda ()
                       (locate ([n.1 rax] [a.2 rbx])
                         (if (= n.1 0)
                             (begin
                               (set! n.1 a.2)
                               (r15))
                             (begin
                               (set! a.2 (* a.2 n.1))
                               (set! n.1 (- n.1 1))
                               (fact$1)))))])
      (locate ([n.1 rax])
        (begin
          (set! n.1 10)
          (fact$0))))
    (letrec ([fib$0 (lambda ()
                      (locate ([n.1 rax] [a.2 rbx] [b.3 rcx])
                        (begin
                          (set! a.2 0)
                          (set! b.3 1)
                          (fib$1))))]
             [fib$1 (lambda ()
                      (locate ([n.1 rax] [a.2 rbx] [b.3 rcx] [t.4 fv1]
                               [return.5 rax])
                        (if (= n.1 0)
                            (begin
                              (set! return.5 a.2)
                              (fv0))
                            (begin
                              (set! n.1 (- n.1 1))
                              (set! t.4 a.2)
                              (set! a.2 b.3)
                              (set! b.3 (+ b.3 t.4))
                              (fib$1)))))])
      (locate ([n.1 rax])
        (begin
          (set! fv0 r15)
          (set! n.1 5)
          (fib$0))))
    (letrec ([f$1 (lambda ()
                    (locate ([x.1 r8] [y.2 r9])
                      (if (if (= x.1 1) (true) (> y.2 1000))
                          (begin (set! rax y.2) (r15))
                          (begin
                            (set! y.2 (* y.2 2))
                            (set! rax x.1)
                            (set! rax (logand rax 1))
                            (if (= rax 0) (set! y.2 (+ y.2 1)) (nop))
                            (set! x.1 (sra x.1 1))
                            (f$1)))))])
      (locate ()
        (begin
          (set! r8 3)
          (set! r9 10)
          (f$1))))
    (letrec ([f$1 (lambda ()
                    (locate ([x.1 r8] [y.2 r9])
                      (if (if (= x.1 1) (true) (> y.2 1000))
                          (begin (set! rax y.2) (r15))
                          (begin
                            (set! y.2 (* y.2 2))
                            (set! rax x.1)
                            (set! rax (logand rax 1))
                            (if (= rax 0) (set! y.2 (+ y.2 1)) (nop))
                            (set! x.1 (sra x.1 1))
                            (f$1)))))]
             [g$2 (lambda () (locate () (f$1)))]
             [h$3 (lambda () (locate () (g$2)))]
             [s$4 (lambda () (locate () (t$5)))]
             [t$5 (lambda () (locate () (s$4)))])
      (locate ()
        (begin
          (set! r8 3)
          (set! r9 10)
          (f$1))))
    ))
