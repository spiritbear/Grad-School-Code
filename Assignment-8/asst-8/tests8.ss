(define invalid-tests 
  '(3
    (begin rax 5)
    (letrec () (set! rax 5))
    (letrec () (set! rax 5) (r15))
    (letrec () (begin (set! rax 5) (r15)))
    (letrec ([f$5 (lambda (rax)
                    (locals ()
                      (begin (r15))))])
      (locals () (f$5)))
    (letrec () (letrec () (begin (set! rax 5) (r15 rax))))
    (letrec (locals () (begin (set! rax 5) (r15 rax))))
    (letrec () (locals (x.1) (begin (set! x.1 5.5) x.1)))
    (letrec ([double$1 (lambda ()
                         (locals (x.1)
                           (begin (set! x.1 (+ x.1 x.1)) x.1)))]
             [triple$1 (lambda ()
                         (locals (x.2)
                           (begin (set! x.2 (* x.2 3)) x.2)))])
      (locals () (double$1)))
    (letrec ([foo (lambda ()
                    (locals (a.1) (begin (set! a.1 5) a.1)))])
      (locals () (foo)))
    (letrec ([foo$0 (lambda ()
                      (locals (a.1)
                        (begin (set! a.1 5) a.1)))])
      (locals () (bar$1))) 
    (letrec ([even?$1 (lambda (n.1)
                        (locals ()
                          (if (= 0 n.1)
                              1
                              (odd?$2 (- n.1 1)))))]
             [odd?$2 (lambda (n.1)
                       (locals ()
                         (if (= 0 n.1)
                             0
                             (even?$1 (- n.1 1)))))])
      (locals ()
        (if (odd?$1 17)
            10
            0)))
    (letrec () (locals () (begin (set! x 5) x))) 
    (letrec () (locals () 9223372036854775808))
    (letrec () (locals () -9223372036854775809))
    (letrec () 
      (locals (x.1) (begin (set! x.1 0) (+ x.1 9223372036854775808))))
    (letrec ()
      (locals (x.1) (begin (set! x.1 0) (+ x.1 -9223372036854775809))))
    (letrec () (locals () 12.5))
    (letrec () (locals (x.1) (begin (set! x.1 (sra x.1 -1)) x.1)))
    (letrec () (locals (x.1) (begin (set! x.1 (sra x.1 64)) x.1)))
    (letrec () (locals (x.1 y.2) (begin (set! x.1 (sra x.1 y.2)) x.1)))
    (letrec ([foo$1 (lambda () (locals () 7))])
      (locals (x.1 y.2) (begin (set! x.1 (sra x.1 foo$1)) x.1)))
    (letrec () (locals (x.1) (begin (set! x.1 (/ x.1 5)) x.1)))
    (letrec ()
      (locals (x.1 y.2 z.3)
        (begin (set! x.1 y.2) (if (+ z.3 5) (x.1) (z.3)))))
    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 0)
          (if (= x.1 0) (nop) x.1)
          (set! x.1 5)
          x.1)))
    (letrec ([foo$1 (lambda () (locals () 7))])
      (locals (x.1)
        (begin
          (set! x.1 0)
          (if (= x.1 0) (nop) (> x.1 0))
          (set! x.1 5)
          x.1)))
    (letrec ()
      (locals (x.1)
        (begin (if (begin (set! x.1 10) x.1) x.1 x.1))))
    (letrec ([foo$1 (lambda () (locals () 7))])
      (locals (x.1)
        (begin (if (begin (set! x.1 10) (foo$1)) (foo$1) (foo$1)))))
    (letrec ()
      (locals (x.1)
        (begin (if (begin (set! x.1 10) (nop)) x.1 x.1))))
    (letrec ()
      (locals (x.1 y.2 z.3)
        (begin
          (if (begin (set! x.1 10) (set! y.2 10) (= x.1 y.2))
              (set! z.3 10)
              x.1))))
    (letrec ()
      (locals (x.1 y.2)
        (begin
          (set! x.1 0)
          (if (if (= x.1 0) (nop) (= y.2 0))
              (set! x.1 10)
              (set! y.2 10))
          x.1)))
    (letrec ([main$0 (lambda ()
                       (locals (x.1 y.2 z.3)
                         (begin
                           (set! x.1 y.2)
                           (if (+ z.3 5) x.1 y.2))))])
      (locals () (main$0)))
    (letrec ([main$0 (lambda ()
                       (locals (x.1)
                         (begin
                           (if (begin (set! x.1 10) (nop))
                               x.1
                               x.1))))])
      (locals () (main$0)))
    (letrec ([foo$1 (lambda () (locals () 7))]
             [main$0 (lambda ()
                       (locals (x.1)
                         (begin
                           (if (begin (set! x.1 10) (nop))
                               (foo$1)
                               (foo$1)))))])
      (locals () (main$0)))
    (letrec ([main$0 (lambda ()
                       (locals (x.1 y.2)
                         (begin
                           (if (begin
                                 (set! x.1 10)
                                 (set! y.2 10)
                                 (= x.1 y.2))
                               (set! x.1 10)
                               x.1))))])
      (locals () (main$0)))
    (letrec ([main$0 (lambda ()
                       (locals (x.1)
                         (if (= 0 x.1) (f$0) (f$1))))]
             [f$0 (lambda () (locals () 5))]
             [f$1 (lambda () (locals () 6))])
      (locals () (main$0)))
    (letrec ()
      (locals (x.1) (if (= x.1 9223372036854775808) x.1 x.1)))
    (letrec ()
      (locals (x.1)
        (if (= x.1 -9223372036854775809) x.1 x.1)))
    (letrec ()
      (locals (x.1) (if (= x.1 12.5) x.1 x.1)))
    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 7)
          (if (if (< x.1 10) (if (< x.1 5) (false) (true)) #f)
              (begin (set! x.1 (* x.1 x.1)) x.1)
              x.1))))
    (letrec ()
      (locals ()
        (begin (set! x.5 (+ x.5 x.5)) x.5)))
    (letrec ()
      (locals (x z.5)
        (begin
          (set! x.1 5)
          (set! z.5 6)
          (set! z.5 (+ z.5 x.1))
          z.5)))
    (letrec ()
      (locals (x.1 y.2)
        (begin
          (set! x.1 5)
          (set! y.2 2)
          (set! z.5 (+ z.5 x.1))
          (set! z.5 (+ z.5 x.1))
          z.5)))
    (letrec ([id$1 (lambda (x.1) (locals () x.1))])
      (locals (x.1 y.2)
        (set! x.1 5)
        (id$1 x.1)))
    (letrec ()
      (locals (x.1 y.2 z.1)
        (begin
          (set! x.1 5)
          (set! y.2 rax)
          (set! z.1 (+ x.1 y.2))
          z.1)))
    (letrec ()
      (locals (x.1 y.2)
        (begin
          (set! x.1 (true))
          (if x.1 (set! y.2 5) (set! y.2 100))
          y.2)))
    (letrec ()
      (locals (x.1 y.2 acc.3)
        (begin
          (set! x.1 5)
          (set! y.2 0)
          (set! acc.3 0)
          loop$1
          (set! acc.3 (+ acc.3 x.1))
          (set! x.1 (- x.1 1))
          (if (= y.2 x.1) (nop) (loop$1))
          (set! rax acc.3)
          (r15 rax))))
    (letrec ([double$1 (lambda ()
                         (locals (x.3)
                           (begin
                             (set! x.3 10)
                             (set! x.3 (+ x.3 x.3))
                             x.3)))])
      (locals (x.1 y.2 fv0)
        (begin
          (set! x.1 4)
          (set! y.2 0)
          (set! fv0 (+ x.1 y.2))
          (double$1 r15 rbp fv0))))
    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 rax)
          (+ x.1 5))))
    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 4)
          (set! x.1 (+ x.1 rax))
           x.1)))
    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 4)
          (+ x.1 rax))))
    (letrec ()
      (locals ()
        (alloc)))
    (letrec ()
      (locals ()
        (mref (alloc 16 24) 0)))
    (letrec ()
      (locals ()
        (mref (alloc 16))))
    (letrec ()
      (locals ()
        (mset! (alloc 16) 10)))
    ;; Leah Brown
    (letrec ([f$0 (lambda (a.2) (locals () (+ (mref a.2 0) 13)))])
      (locals (y.1)
        (begin
          (set! y.1 (alloc 16))
          (mset! y.1 8 10)
          (mset! y.1 81 5 6)
          (f$0 y.1))))

    ;; Nilesh Mahajan
    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 (alloc -16)))))

    (letrec ()
      (locals (x.1)
        (begin
          (set! x.1 (alloc 1.6)))))
  ))

(define tests 
  '((letrec () (locals () 7))
    (letrec () (locals () (+ 5 7)))
    (letrec () (locals () (+ 7 (* 5 7))))
    (letrec () (locals () (* (+ 2 4) (+ (+ 6 7) 4))))
    (letrec () 
      (locals () 
        (if (= (+ 7 (* 2 4)) (- 20 (+ (+ 1 1) (+ (+ 1 1) 1))))
            (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 10)))))
            0)))
    (letrec ()
      (locals (a.1)
        (begin
          (set! a.1 10)
          (if (< 7 a.1)
              (nop)
              (set! a.1 (+ a.1 a.1)))
          a.1)))
    (letrec ()
      (locals (a.1)
        (begin
          (set! a.1 5)
          (if (< 5 a.1)
              a.1
              (+ a.1 a.1)))))
    (letrec ()
      (locals (c.1 a.2)
        (begin
          (set! a.2 5)
          (set! c.1 10)
          (if (< a.2 c.1) a.2 c.1))))
     (letrec ()
       (locals (a.1)
         (begin
           (set! a.1 5)
           (if (< a.1 10) (set! a.1 (* a.1 10)) (nop))
           a.1)))
    (letrec ([f$0 (lambda (x.1) (locals () (+ 1 x.1)))])
      (locals (f.2) (f$0 (begin (set! f.2 3) (+ f.2 1)))))
    (letrec ([f$0 (lambda (h.1 v.2) (locals () (* h.1 v.2)))]
             [k$1 (lambda (x.1) (locals () (+ x.1 5)))]
             [g$2 (lambda (x.1) (locals () (+ 1 x.1)))])
      (locals (x.4 g.1)
        (begin
          (set! x.4 15)
          (k$1 (g$2 (begin (set! g.1 3) (f$0 g.1 x.4)))))))
    (letrec ([one$1 (lambda (n.1) 
                      (locals () (if (= 0 n.1) 1 (one$1 (- n.1 1)))))])
       (locals () (one$1 13)))
    (letrec ([f$0 (lambda (p.2) (locals () (- (mref p.2 8) (mref p.2 0))))])
      (locals (x.1)
        (begin
          (set! x.1 (alloc 16))
          (mset! x.1 0 73)
          (mset! x.1 8 35)
          (f$0 x.1))))
    (letrec ([f$0 (lambda (p.2 i.3 i.4)
                    (locals () (- (mref p.2 i.3) (mref p.2 i.4))))])
      (locals (x.1)
        (begin
          (set! x.1 (alloc 16))
          (mset! x.1 0 73)
          (mset! x.1 8 35)
          (+ (f$0 x.1 0 8) -41))))
    (letrec ([f$0 (lambda (p.3)
                    (locals (p.4)
                      (- (mref
                           (mref (mref (mref (mref p.3 0) 0) 8) 0)
                           (mref (mref p.3 8) (mref (mref p.3 0) 32)))
                         (mref
                           (mref p.3 (mref p.3 16))
                           (mref (mref p.3 0) (mref p.3 32))))))])
      (locals (x.1 x.2)
        (begin
          (set! x.1 (alloc 48))
          (set! x.2 (alloc 56))
          (mset! x.1 0 x.2)
          (mset! x.1 8 x.1)
          (mset! x.2 0 x.1)
          (mset! x.2 8 -4421)
          (mset! x.1 16 0)
          (mset! x.1 24 -37131)
          (mset! x.1 32 32)
          (mset! x.1 40 48)
          (mset! x.2 16 -55151)
          (mset! x.2 24 -32000911)
          (mset! x.2 32 40)
          (mset! x.2 40 55)
          (mset! x.2 48 -36)
          (* (f$0 x.1) 2))))
    (letrec ([make-vector$0 (lambda (size.1)
                              (locals (v.2)
                                (begin
                                  (set! v.2 (alloc (+ (* size.1 8) 8)))
                                  (mset! 0 v.2 size.1)
                                  v.2)))]
             [chained-vector-set!$1 (lambda (v.3 off.4 val.5)
                                      (locals ()
                                        (begin
                                          (mset! (* (+ off.4 1) 8) v.3 val.5)
                                          v.3)))]
             [vector-length$4 (lambda (v.8) (locals () (mref v.8 0)))]
             [find-greatest-less-than$2 (lambda (v.6 val.7)
                                          (locals ()
                                            (fglt-help$3 v.6 val.7 (+ v.6 8)
                                              (vector-length$4 v.6))))]
             [fglt-help$3 (lambda (v.9 val.10 curr.11 size.12)
                            (locals ()
                              (if (if (> curr.11 (+ (+ v.9 (* size.12 8)) 8))
                                      (true)
                                      (> (mref curr.11 0) val.10))
                                  (mref curr.11 -8)
                                  (fglt-help$3 v.9 val.10 (+ curr.11 8)
                                               size.12))))])
      (locals (v.13)
        (begin
          (set! v.13 (chained-vector-set!$1
                       (chained-vector-set!$1 
                         (chained-vector-set!$1 
                           (chained-vector-set!$1 
                             (chained-vector-set!$1 
                               (chained-vector-set!$1 
                                 (chained-vector-set!$1 
                                   (chained-vector-set!$1 
                                     (chained-vector-set!$1 
                                       (chained-vector-set!$1 
                                         (make-vector$0 10) 0 0)
                                       1 10)
                                     2 20)
                                   3 30)
                                 4 40)
                               5 50)
                             6 60)
                           7 70)
                         8 80)
                       9 90))
          (find-greatest-less-than$2 v.13 76))))
    ;(letrec ([vector-scale!$0 (lambda (vect.1 scale.2)
    ;                               (locals (size.3)
    ;                                 (begin
    ;                                   (set! size.3 (mref vect.1 0))
    ;                                   (vector-scale!$1 size.3 vect.1 scale.2))))]
    ;            [vector-scale!$1 (lambda (offset.4 vect.5 scale.6)
    ;                               (locals ()
    ;                                 (if (< offset.4 1)
    ;                                     0
    ;                                     (begin
    ;                                       (mset! vect.5 (* offset.4 8)
    ;                                              (* (mref vect.5 (* offset.4 8))
    ;                                                 scale.6))
    ;                                       (vector-scale!$1 (- offset.4 1)
    ;                                                        vect.5 scale.6)))))]
    ;            [vector-sum$2 (lambda (vect.7)
    ;                            (locals ()
    ;                              (vector-sum$3 (mref vect.7 0) vect.7)))]
    ;            [vector-sum$3 (lambda (offset.9 vect.10)
    ;                            (locals ()
    ;                              (if (< offset.9 1)
    ;                                  0
    ;                                  (+ (mref vect.10 (* offset.9 8))
    ;                                     (vector-sum$3 (- offset.9 1)
    ;                                                   vect.10)))))])
    ;     (locals (vect.11)
    ;       (begin
    ;         (set! vect.11 (alloc 48))
    ;         (mset! vect.11 0 5)
    ;         (mset! vect.11 8 123)
    ;         (mset! vect.11 16 10)
    ;         (mset! vect.11 24 7)
    ;         (mset! vect.11 32 12)
    ;         (mset! vect.11 40 57)
    ;         (vector-scale!$0 vect.11 10)
    ;         (vector-sum$2 vect.11))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (locals (ptr.3)
                       (begin
                         (set! ptr.3 (alloc 16))
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (locals () (mref ptr.4 0)))]
             [snd$2 (lambda (ptr.5) (locals () (mref ptr.5 8)))]
             [length$3 (lambda (ptr.6)
                         (locals ()
                           (if (= ptr.6 0)
                               0
                               (+ 1 (length$3 (snd$2 ptr.6))))))])
      (locals ()
        (length$3 (::$0 5 (::$0 10 (::$0 11 (::$0 5 (::$0 15 0))))))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (locals (ptr.3)
                       (begin
                         (set! ptr.3 (alloc 16))
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (locals () (mref ptr.4 0)))]
             [snd$2 (lambda (ptr.5) (locals () (mref ptr.5 8)))]
             [count-leaves$3 (lambda (ptr.6)
                               (locals ()
                                 (if (= ptr.6 0)
                                     1
                                     (+ (count-leaves$3 (fst$1 ptr.6))
                                        (count-leaves$3 (snd$2 ptr.6))))))])
      (locals ()
        (count-leaves$3
          (::$0 
            (::$0
              0
              (::$0 0 0))
            (::$0
              (::$0
                (::$0 (::$0 0 (::$0 0 0)) 0)
                0)
              (::$0 (::$0 (::$0 0 0) (::$0 0 (::$0 0 0)))
                    (::$0 (::$0 0 0) 0)))))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (locals (ptr.3)
                       (begin
                         (set! ptr.3 (alloc 16))
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (locals () (mref ptr.4 0)))]
             [snd$2 (lambda (ptr.5) (locals () (mref ptr.5 8)))]
             [add1$3 (lambda (n.6) (locals () (+ n.6 1)))]
             [map$4 (lambda (f.7 ls.8)
                      (locals ()
                        (if (= ls.8 0)
                            0
                            (::$0 (f.7 (fst$1 ls.8)) 
                                  (map$4 f.7 (snd$2 ls.8))))))]
             [sum$5 (lambda (ls.9)
                      (locals ()
                        (if (= 0 ls.9)
                            0
                            (+ (fst$1 ls.9) (sum$5 (snd$2 ls.9))))))])
      (locals (ls.10)
        (begin
          (set! ls.10 (::$0 5 (::$0 4 (::$0 3 (::$0 2 (::$0 1 0))))))
          (set! ls.10 (::$0 10 (::$0 9 (::$0 8 (::$0 7 (::$0 6 ls.10))))))
          (sum$5 (map$4 add1$3 ls.10)))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (locals (ptr.3)
                       (begin
                         (set! ptr.3 (alloc 16))
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (locals () (mref ptr.4 0)))]
             [snd$2 (lambda (ptr.5) (locals () (mref ptr.5 8)))]
             [list-ref$3 (lambda (ls.11 offset.12)
                           (locals ()
                             (if (= offset.12 0)
                                 (fst$1 ls.11)
                                 (list-ref$3 (snd$2 ls.11) (- offset.12 1)))))]
             [add$6 (lambda (v.13 w.14) (locals () (+ v.13 w.14)))]
             [sub$7 (lambda (v.15 w.16) (locals () (- v.15 w.16)))]
             [mult$8 (lambda (v.17 w.18) (locals () (* v.17 w.18)))]
             [expt$9 (lambda (v.17 w.18) 
                       (locals () 
                         (if (= w.18 0)
                             1
                             (* v.17 (expt$9 v.17 (- w.18 1))))))]
             [selector$4 (lambda (op*.7 sel.19 rand1.20 rand2.21)
                           (locals ()
                             (if (= sel.19 0)
                                 0
                                 (::$0 ((list-ref$3 op*.7 (fst$1 sel.19))
                                        (fst$1 rand1.20) (fst$1 rand2.21))
                                       (selector$4 op*.7 (snd$2 sel.19)
                                                   (snd$2 rand1.20)
                                                   (snd$2 rand2.21))))))]
             [sum$5 (lambda (ls.9)
                      (locals ()
                        (if (= 0 ls.9)
                            0
                            (+ (fst$1 ls.9) (sum$5 (snd$2 ls.9))))))])
      (locals (ls.10)
        (begin
          (sum$5 (selector$4 
                   (::$0 add$6 (::$0 sub$7 (::$0 mult$8 (::$0 expt$9 0))))
                   (::$0 2 (::$0 0 (::$0 1 (::$0 3 (::$0 2 0)))))
                   (::$0 5 (::$0 9 (::$0 10 (::$0 2 (::$0 3 0)))))
                   (::$0 3 (::$0 1 (::$0 3 (::$0 3 (::$0 8 0))))))))))
    (letrec ([thunk-num$0 (lambda (n.1)
                            (locals (th.2)
                              (begin 
                                (set! th.2 (alloc 16))
                                (mset! th.2 0 force-th$1)
                                (mset! th.2 8 n.1)
                                th.2)))]
             [force-th$1 (lambda (cl.3)
                           (locals ()
                             (mref cl.3 8)))]
             [add-ths$2 (lambda (cl1.4 cl2.5 cl3.6 cl4.7)
                          (locals ()
                            (+ (+ ((mref cl1.4 0) cl1.4)
                                  ((mref cl2.5 0) cl2.5))
                               (+ ((mref cl3.6 0) cl3.6)
                                  ((mref cl4.7 0) cl4.7)))))])
      (locals ()
        (add-ths$2 (thunk-num$0 5) (thunk-num$0 17) (thunk-num$0 7)
                   (thunk-num$0 9))))
    (letrec ([make-vector$0 (lambda (size.1)
                              (locals (v.20)
                                (begin
                                  (set! v.20 (alloc (* (+ size.1 1) 8)))
                                  (mset! 0 v.20 size.1)
                                  v.20)))]
             [vector-set!$1 (lambda (vect.2 off.3 val.4)
                              (locals ()
                                (begin
                                  (if (> off.3 (mref vect.2 0))
                                      (nop)
                                      (mset! (* (+ off.3 1) 8) vect.2 val.4))
                                  0)))]
             [vector-equal?$3 (lambda (vect1.8 vect2.9)
                                (locals ()
                                  (if (= (mref 0 vect1.8) (mref 0 vect2.9))
                                      (vector-equal?$4 vect1.8 vect2.9
                                                       (mref 0 vect1.8))
                                      0)))]
             [vector-equal?$4 (lambda (vect1.11 vect2.12 off.10)
                                (locals ()
                                  (if (< off.10 0)
                                      1 
                                      (if (= (mref (* (+ off.10 1) 8) vect1.11)
                                             (mref vect2.12 (* (+ off.10 1) 8)))
                                          (vector-equal?$4 vect1.11 vect2.12
                                                           (- off.10 1))
                                          0))))])
      (locals (v1.13 v2.14)
        (begin
          (set! v1.13 (make-vector$0 5))
          (vector-set!$1 v1.13 0 134)
          (vector-set!$1 v1.13 1 123)
          (vector-set!$1 v1.13 2 503)
          (vector-set!$1 v1.13 3 #x66)
          (vector-set!$1 v1.13 4 #xff)
          (set! v2.14 (make-vector$0 5))
          (vector-set!$1 v2.14 0 134)
          (vector-set!$1 v2.14 1 123)
          (vector-set!$1 v2.14 2 503)
          (vector-set!$1 v2.14 3 #x66)
          (vector-set!$1 v2.14 4 #xff)
          (if (= (vector-equal?$3 v1.13 v2.14) 0)
              100
              -100))))

    ;;; A little OOP
    (letrec ([stack-new$0 (lambda (size.1)
                            (locals (stack.2 store.3 meths.4)
                              (begin
                                (set! store.3 (alloc (* 8 size.1)))
                                (set! meths.4 (alloc (* 3 8)))
                                (set! stack.2 (alloc (* 3 8)))
                                (mset! meths.4 0 stack-push$2)
                                (mset! meths.4 8 stack-pop$3)
                                (mset! meths.4 16 stack-top$4)
                                (mset! stack.2 0 meths.4)
                                (mset! stack.2 8 0)
                                (mset! stack.2 16 store.3)
                                stack.2)))]
             [invoke$1 (lambda (obj.5 meth-idx.6)
                         (locals ()
                           (mref (mref obj.5 0) (* meth-idx.6 8))))]
             [stack-push$2 (lambda (self.7 val.8)
                             (locals ()
                               (begin
                                 (mset! (mref self.7 16) 
                                        (* (mref self.7 8) 8)
                                        val.8)
                                 (mset! self.7 8 (+ (mref self.7 8) 1))
                                 self.7)))]
             [stack-pop$3 (lambda (self.9)
                            (locals ()
                              (begin
                                (mset! self.9 8 (- (mref 8 self.9) 1))
                                (mref (mref self.9 16) 
                                      (* (mref self.9 8) 8)))))]
             [stack-top$4 (lambda (self.9)
                            (locals ()
                              (mref (mref self.9 16) 
                                    (* (- (mref 8 self.9) 1) 8))))])
      (locals (s1.10 s2.11)
        (begin
          (set! s1.10 (stack-new$0 10))
          ((invoke$1 s1.10 0) s1.10 10) ;; push 10
          ((invoke$1 s1.10 0) s1.10 20) ;; push 20
          ((invoke$1 s1.10 0) s1.10 30) ;; push ... well you get the idea
          ((invoke$1 s1.10 0) s1.10 40)
          ((invoke$1 s1.10 0) s1.10 50)
          ((invoke$1 s1.10 0) s1.10 60)
          ((invoke$1 s1.10 0) s1.10 70)
          ((invoke$1 s1.10 0) s1.10 80)
          ((invoke$1 s1.10 0) s1.10 90)
          ((invoke$1 s1.10 0) s1.10 100)
          (set! s2.11 (stack-new$0 5))
          ((invoke$1 s2.11 0) s2.11 ((invoke$1 s1.10 1) s1.10)) ;; push pop
          ((invoke$1 s1.10 1) s1.10) ;; pop
          ((invoke$1 s2.11 0) s2.11 ((invoke$1 s1.10 1) s1.10))
          ((invoke$1 s1.10 1) s1.10) ;; pop
          ((invoke$1 s2.11 0) s2.11 ((invoke$1 s1.10 1) s1.10))
          ((invoke$1 s1.10 1) s1.10) ;; pop
          ((invoke$1 s2.11 0) s2.11 ((invoke$1 s1.10 1) s1.10))
          ((invoke$1 s1.10 1) s1.10) ;; pop
          ((invoke$1 s2.11 0) s2.11 ((invoke$1 s1.10 1) s1.10))
          ((invoke$1 s1.10 1) s1.10) ;; pop
          (+
            (+ 
              (+ ((invoke$1 s2.11 2) s2.11) ((invoke$1 s2.11 1) s2.11))
              (+ ((invoke$1 s2.11 2) s2.11) ((invoke$1 s2.11 1) s2.11)))
            (+
              (+ ((invoke$1 s2.11 2) s2.11) ((invoke$1 s2.11 1) s2.11))
              (+ 
                (+ ((invoke$1 s2.11 2) s2.11) ((invoke$1 s2.11 1) s2.11))
                (+ ((invoke$1 s2.11 2) s2.11) ((invoke$1 s2.11 1) s2.11))))))))

    ;;;;;;
    ;;; Student tests from a8 before 
    ;;;;;;

    ;;; Ben Peters
    ;;; bejpeter
    ;;; uses only one variable,
    ;;; testing m-set!, alloc, and mref,
    ;;; as well as live calls
    ;;; should produce 9
    (letrec ([proc$1 (lambda (a.1)
                       (locals ()
                         (begin
                           (+ a.1 5))))])
      (locals (b.1)
        (begin
          (set! b.1 (alloc 8))
          (mset! b.1 0 proc$1)
          (set! b.1 (mref b.1 0))
          (b.1 4))))

    ;; Francis Fernandez
    (letrec ([make-list$0 (lambda (length.0)
                            (locals ()
                              (alloc (byte-offset$3 length.0))))]
             [fill-list$1 (lambda (content.0 value.1 length.2)
                            (locals ()
                              (fill-list-helper$2 content.0
                                                  value.1
                                                  0
                                                  length.2)))]
             [fill-list-helper$2 (lambda (content.0 value.1 index.2 length.3)
                                   (locals ()
                                     (if (= index.2 length.3)
                                         content.0
                                         (begin
                                           (mset! content.0
                                                  (byte-offset$3 index.2)
                                                  value.1)
                                           (fill-list-helper$2 content.0
                                                               value.1
                                                               (+ index.2 1)
                                                               length.3)))))]
             [byte-offset$3 (lambda (int.0)
                              (locals ()
                                (* int.0 8)))])
      (locals (length.10 value.5)
        (begin
          (set! length.10 10)
          (set! value.5 5)
          (mref (fill-list$1 (make-list$0 length.10)
                             value.5
                             length.10)
                (byte-offset$3 (- length.10 1))))))

    ;; Chabane Maidi
    (letrec ()
      (locals (x.15)
        (begin
          (set! x.15 (alloc 16))
          (if (= 10 11) (nop) (mset! x.15 0 12))
          (set! x.15 x.15)
          (mset! x.15 8 x.15)
          (set! x.15 (mref x.15 8))
          (mref x.15 0))))

    ;; Kewal Karavinkoppa
    (letrec ([test$0 (lambda (m.3)
                       (locals ()
                         (- (mref m.3 40) 10)))])
      (locals (x.1 x.2 x.3 x.4 x.5)
        (begin
          (set! x.1 (alloc 32))
          (mset! x.1 0 x.1)
          (mset! x.1 8 5)
          (mset! x.1 16 48)
          (mset! x.1 24 16)
          (set! x.2 (alloc (mref (mref x.1 0) (mref x.1 24))))
          (mset! x.2 0 -15)
          (mset! x.2 8 -3000)
          (mset! x.2 16 20)
          (mset! x.2 24 1500)
          (mset! x.2 (if (< (mref x.1 8) 3) 40 48)
                 (begin (set! x.4 (alloc 8)) (mset! x.4 0 10) (mref x.4 0)))
          (mset! x.2 (if (> (mref x.1 8) 3) 40 48) 20)
          (mset! (if (= (mref x.1 16) 48) x.2 x.1)
                 (if (< (mref x.1 8) 1) 40 48) (if (> (mref x.2 24) 5) 55 60))
          (* (test$0 x.2) (begin (set! x.5 (alloc 16))
                                 (mset! x.5 0 10) (mset! x.5 8 20)
                                 (mref x.5 0))))))

    ;; Zhou Li
    (letrec ([add$0
               (lambda (x.1 y.2)
                 (locals (z.3)
                   (begin
                     (set! z.3 (alloc 8))
                     (mset! z.3 0 (+ x.1 y.2))
                     z.3)))])
      (locals()
        (mref (add$0 1 2) 0)))

    ;; Thiago Rebello
    (letrec ([test$0 (lambda (y.2)
                       (locals ()
                         (+ (* (mref y.2 0) (mref y.2 8)) (mref y.2 16))))])
      (locals (x.1)
        (begin
          (set! x.1 (alloc 32))
          (mset! x.1 0 4)
          (mset! x.1 8 8)
          (mset! x.1 16 16)
          (test$0 x.1))))

    ;; Melanie Dybvig
    (letrec ([d$1 (lambda ()
                    (locals () (alloc 16)))])
      (locals (b.2 c.3)
        (begin
          (set! b.2 32)
          (set! c.3 (d$1))
          (mset! c.3 8 b.2)
          (mref c.3 8))))

    ;; Shiv Indap
    ;;;This is a test case for Assignment-8
    (letrec ([get$0 (lambda (x.1 ls.2)
                      (locals (size.4 ls.3)
                        (begin
                          (set! size.4 (mref ls.2 0))
                          (if (> x.1 size.4)
                              -1
                              (mref ls.2 (* 8 x.1))))))])
      (locals (ls.1)
        (begin
          (set! ls.1 (alloc 48))
          (mset! ls.1 0 5)
          (mset! ls.1 8 9)
          (mset! ls.1 16 2)
          (mset! ls.1 24 7)
          (mset! ls.1 32 8)
          (mset! ls.1 40 3)
          (get$0 4 ls.1))))

    ;;; Wether x is a member of ls : if true return 1 else return 0
    (letrec ([member$0 (lambda (x.1 ls.2)
                         (locals (size.4 ls.3)
                           (begin
                             (set! size.4 (mref ls.2 0))
                             (if (> x.1 size.4)
                                 0
                                 (if (= x.1 (mref ls.2 16))
                                     1
                                     (begin
                                       (set! ls.3 (alloc (* 8 (- size.4 1))))
                                       (mset! ls.3 0 (- size.4 1))
                                       (mset! ls.3 8 (+ ls.2 16))
                                       (member$0 x.1 ls.3)))))))])
      (locals (ls.1)
        (begin
          (set! ls.1 (alloc 48))
          (mset! ls.1 0 5)
          (mset! ls.1 8 9)
          (mset! ls.1 16 2)
          (mset! ls.1 24 7)
          (mset! ls.1 32 8)
          (mset! ls.1 40 3)
          (member$0 4 ls.1))))

    ;; Yu-Shan Huang
    (letrec ([a$1 (lambda (m.5 x.1 y.2)
                    (locals ()
                      (begin
                        (mset! m.5 0 (+ x.1 y.2))
                        m.5)))])
      (locals (x.3)
        (begin
          (set! x.3 (a$1 (alloc 8) 10 6))
          (mref x.3 0))))

    (letrec ([a$1 (lambda (m.1 a.2)
                    (locals ()
                      (begin
                        (mset! m.1 a.2 (+ (mref m.1 (- a.2 8))
                                          (mref m.1 (- a.2 8))))
                        1)))])
      (locals (m.3)
        (begin
          (set! m.3 (alloc 56))
          (mset! m.3 0 1)
          (mset! m.3 8 1)
          (a$1 m.3 16)
          (a$1 m.3 24)
          (a$1 m.3 32)
          (a$1 m.3 40)
          (a$1 m.3 48)
          (mref m.3 48))))

    ;; Patrick Jensen
    (letrec ([f$0 (lambda (c.3 d.4)
                    (locals (e.5)
                      (- (mref c.3 (mref d.4 8))
                         (begin
                           (set! e.5 (alloc 16))
                           (mset! e.5 0 (mref c.3 0))
                           (mset! e.5 8 (mref d.4 0))
                           (if (> (mref e.5 0) (mref e.5 8))
                               (mref e.5 8)
                               (mref e.5 0))))))])
      (locals (a.1 b.2)
        (begin
          (set! a.1 (alloc 24))
          (set! b.2 (alloc 16))
          (mset! a.1 0 8)
          (mset! a.1 8 (+ (mref a.1 0) (mref a.1 0)))
          (mset! a.1 16 (+ (mref a.1 0) (mref a.1 8)))
          (mset! b.2 0 (mref a.1 16))
          (mset! b.2 8 (- (mref b.2 0) (mref a.1 0)))
          (f$0 a.1 b.2))))

    ;; Emily Lyons
    (letrec ([main$0 (lambda (p.1)
                       (locals ()
                         (+
                           (mref p.1 0)
                           (mref p.1 8))))])
      (locals (x.2)
        (begin
          (set! x.2 (alloc 16))
          (mset! x.2 0 10)
          (mset! x.2 8 20)
          (main$0 x.2))))

    ;; Yin Wang
    (letrec ()
      (locals (a.1 x.2)
        (begin
          (set! x.2 (alloc 16))
          (mset! x.2 8 3)
          (mref (begin (set! a.1 x.2) a.1)
                (begin (set! a.1 8) a.1)))))

    (letrec ()
      (locals (a.1 x.2)
        (begin
          (set! x.2 (alloc 16))
          (mset! x.2 0 1)
          (mset! x.2 8 2)
          (mref (begin (set! a.1 x.2) a.1)
                (begin (mset! a.1 8 0) (mref a.1 8))))))

    (letrec ()
      (locals (a.1 x.2)
        (begin
          (set! x.2 (alloc 16))
          (mset! x.2 8 3)
          (mref (begin (if (< 400000000000 200000000000)
                           (set! a.1 x.2)
                           (set! a.1 x.2))
                       a.1)
                (begin (set! a.1 8) a.1)))))

    ;; Lindsey Kuper
    (letrec ()
      (locals (size.1 addr.2)
        (begin
          (set! size.1 16)
          (set! addr.2 (alloc size.1))
          (mset! addr.2 0 5)
          (mset! addr.2 8 4)
          (+ (mref addr.2 0) (mref addr.2 8)))))
    
    ;; Brennon York
    (letrec ([f$1 (lambda (x.1) (locals () (mref x.1 0)))]
             [f$2 (lambda (x.2) (locals () (mref x.2 8)))])
      (locals (z.3)
        (begin
          (set! z.3 (alloc 32))
          (mset! z.3 0 5)
          (mset! z.3 8 12)
          (+ (f$1 z.3) (f$2 z.3)))))

    ;; Leah Brown
    (letrec ([f$0 (lambda (a.2) (locals () (+ (mref a.2 0) 13)))])
      (locals (y.1)
        (begin
          (set! y.1 (alloc 16))
          (mset! y.1 0 10)
          (f$0 y.1))))

    ;; Nilesh Mahajan
    (letrec ()
      (locals (x.1 x.2)
        (begin
          (set! x.1 (alloc 16))
          (mset! x.1 0 2)
          (mref x.1 0))))

    (letrec ()
      (locals (x.1 x.2)
        (begin
          (set! x.2 (alloc (* 2 8)))
          (mset! x.2 0 16)
          (set! x.1 (alloc (mref x.2 0)))
          (mset! x.1 0 2)
          (mref x.1 0))))

    ;;; Student tests from a7 (I've omitted a couple of tests that are
    ;;; a little too time consuming to run the test through the driver.)
    ;;; (small modifications to avoid warnings)

    ;;zhou li
    ;;Assignment 7 test case
    ;;test using the return value of one function as an input for another 
    ;;function
    (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                    (locals ()
                      (if (= n.2 0)
                          (+ (* a.3 (* x.6 x.6)) (+ (* b.4 x.6) c.5))
                          (+ (f$1 (sra n.2 3)
                                  (+ a.3 (logand n.2 4))
                                  (+ b.4 (logand n.2 2))
                                  (+ c.5 (logand n.2 1))
                                  x.6)
                             1))))]
             [f$2 (lambda (x.1 y.2) (locals () (+ x.1 y.2)))])
      (locals () (f$1 16434824 (f$2 3 4) (+ 1 2) -1 7)))


    ;; Melanie Dybvig
    (letrec ([c$0 (lambda (f.6) (locals () f.6))])
      (locals (a.1 b.2 c.3 d.4 e.5)
        (begin
          (set! c.3 2)
          (set! d.4 (* c.3 (+ c.3 8)))
          (set! e.5 (begin
                      (set! a.1 2) ;; make sure a.1 is set
                      (if (< 3 5)
                          (set! a.1 3)
                          (nop))
                      (set! c.3 (+ d.4 c.3)) ;; added to make d.4 above used
                      (set! d.4 c.3)
                      (set! c.3 (+ d.4 a.1)) ;; added to make a.1 and d.4 used
                      c.3))
          (set! a.1 (c$0 e.5))
          (set! b.2 (c$0 c.3))
          (+ d.4 (+ e.5 (+ c.3 (+ a.1 b.2)))))))

    ;; Francis Fernandez
    (letrec ([add-one$0 (lambda (x.0) (locals () (+ x.0 1)))]
             [sum-add-one-twice$1 (lambda (x.0)
                                    (locals ()
                                      (+ (add-one$0 x.0) (add-one$0 x.0))))])
      (locals () (sum-add-one-twice$1 1)))

    ;; Yu-Shan Huang
    (letrec ([a$1 (lambda (x.1 y.2)
                    (locals ()
                      (+ x.1 y.2)))])
      (locals (x.3)
        (begin
          (a$1 5 6)
          (set! x.3 (a$1 10 6))
          x.3)))

    (letrec ([m$1 (lambda (x.1)
                    (locals ()
                      (* x.1 x.1)))]
             [f$2 (lambda (a.1 a.2 a.3 a.4 a.5 a.6 a.7 a.8)
                    (locals ()
                      (+ (+ (+ a.1 a.2) (+ a.3 a.4))
                         (+ (+ a.5 a.6) (+ a.7 a.8)))))])
      (locals (x.3)
        (begin
          (f$2 1 2 3 4 5 6 7 8)
          (set! x.3 (f$2 (m$1 1) (m$1 2) (m$1 3) (m$1 4) (m$1 5) (m$1 6) (m$1 7) (m$1 8)))
          x.3)))

    ;; Kewal Karavinkoppa
    (letrec ([sub1$0 (lambda (x.1)
                       (locals () (begin (set! x.1 (- x.1 1)) x.1)))]
             [mul5$1 (lambda (x.1)
                       (locals () (begin (set! x.1 (* x.1 5)) x.1)))])
      (locals (x.1 y.2 z.3)
        (begin
          (set! x.1 5)
          (set! y.2 10)
          (if (< (sub1$0 x.1) (sub1$0 y.2))
            (set! z.3 (mul5$1 x.1))
            (set! z.3 (mul5$1 y.2)))
          z.3)))

    ;; Lindsey Kuper
    (letrec ([remainder$1
               (lambda (divisor.1 dividend.3)
                 (locals ()
                   (if (= divisor.1 0)
                       0
                       (if (< divisor.1 0)
                           (+ divisor.1 dividend.3)
                           (remainder$1 (- divisor.1 dividend.3)
                                        dividend.3)))))]
             ;; Euclid's Algorithm
             [gcd$2
               (lambda (a.1 b.2)
                 (locals ()
                   (if (= b.2 0)
                       a.1
                       (gcd$2 b.2 (remainder$1 a.1 b.2)))
                   ))])
      (locals ()
         (gcd$2 16 28)))

    ;; Leah Brown
    (letrec ([f$0 (lambda (n.1) (locals () (* n.1 2)))])
      (locals (a.1 b.2)
        (begin
          (set! a.1 3)
          (set! b.2 5)
          (set! a.1 (f$0 a.1))
          (set! b.2 (f$0 b.2))
          (* a.1 b.2))))

    ;; Madha Kerr
    (letrec ([f$1 (lambda (x.4) (locals () (if (= x.4 0) g$2 h$3)))]
             [g$2 (lambda () (locals () 17))]
             [h$3 (lambda () (locals () -23))])
      (locals () ((f$1 (+ 57 14)))))

    ;; Yin Wang
    (letrec ()
      (locals ()
        (if (if (= (+ 7 (* 2 4)) (- 20 (+ (+ 1 1) (+ (+ 1 1) 1))))
                (> 2 3)
                (< 15 (* 4 4)))
            (+ 1 (+ 2 (+ 3 (+ 4 5))))
            0)))

    ;; Matt Karazin
    (letrec ([findroot$0 (lambda (num.0 pos.1)
                           (locals ()
                             (begin
                               (if (= (* pos.1 pos.1) num.0)
                                   pos.1
                                   (if (> pos.1 10)
                                       pos.1
                                       (findroot$0 num.0 (+ pos.1 1)))))))])
      (locals (temp.3) (begin
                         (set! temp.3 (findroot$0 9 0))
                         (if (> temp.3 10)
                             0
                             temp.3))))

    ;; Nilesh Mahajan
    (letrec ([fun$0 (lambda (x.1)
                      (locals ()
                        (if (= x.1 0)
                            0
                            (begin
                              (set! x.1 (fun$1 (- x.1 1)))
                              x.1))))]
             [fun$1 (lambda (x.2)
                      (locals ()
                        (if (= x.2 0)
                            0
                            (begin
                              (set! x.2 (fun$1 (- x.2 2)))
                              x.2))))])
      (locals () (fun$0 1001)))

    ;; Emily Lyons
    (letrec ([expt$1 (lambda (x.1 n.2)
                       (locals ()
                         (if (= n.2 1) x.1 (* x.1 (expt$1 x.1 (- n.2 1))))))])
      (locals () (expt$1 2 3)))

    ;; Thiago Rebello
    (letrec ([f$0 (lambda (a.1 b.2)
                    (locals (c.3)
                      (begin
                        (set! c.3 b.2) ;; make b.2 used a c.3 not live on entry
                        (if (= a.1 5)
                            (set! b.2 10)
                            (set! c.3 15))
                        (- c.3 (- b.2 5)))))]) ;; added to make b.2 used
      (locals () (f$0 3 0)))

    ;; Brennon York
    (letrec ([f$1 (lambda (x.1 y.2)
                    (locals () (if (> x.1 y.2) (f$1 (- x.1 1) y.2) x.1)))])
      (locals (z.3) (begin (set! z.3 6) (f$1 z.3 2))))

    ;; Chabane Maidi
    (letrec ([conseq$1 (lambda (x.1) (locals () (+ x.1 1)))]
             [alt$11 (lambda (x.11) (locals () (+ x.11 11)))])
      (locals ()
        (begin ((if (< 1 211)
                    conseq$1
                    alt$11) 511)
               1)))

    (letrec ([a$0 (lambda (u.1 v.2 w.3 x.4) 
                    (locals () 
                      (if (= u.1 0) 
                          (b$1 v.2 w.3 x.4)
                          (a$0 (- u.1 1) v.2 w.3 x.4))))]
             [b$1 (lambda (q.1 r.2 x.4)
                    (locals (p.3)
                      (begin
                        (set! p.3 (* q.1 r.2))
                        (e$3 (* q.1 r.2) p.3 x.4))))]
             [c$2 (lambda (x.1) (locals () (* 5 x.1)))]
             [e$3 (lambda (n.1 p.3 x.4)
                    (locals ()
                      (if (= n.1 0) 
                          (c$2 p.3)
                          (o$4 (- n.1 1) p.3 x.4))))]
             [o$4 (lambda (n.1 p.3 x.4) 
                    (locals ()
                      (if (= 0 n.1)
                          (c$2 x.4)
                          (e$3 (- n.1 1) p.3 x.4))))])
      (locals (x.4)
        (begin
          (set! x.4 5)
          (a$0 3 2 1 x.4))))
    (letrec ([f$0 (lambda () (locals () 80))])
      (locals (a.1 b.2)
        (begin
          (set! a.1 (f$0))
          (set! b.2 (f$0))
          (* a.1 b.2))))
    (letrec ([f$0 (lambda () (locals () 80))]
             [g$1 (lambda () (locals () 50))])
      (locals (a.1 b.2)
        (begin
          (set! a.1 (f$0))
          (set! b.2 (g$1))
          (* a.1 b.2))))
    (letrec ([f$0 (lambda (x.1) (locals () (+ x.1 1)))]
             [g$1 (lambda (y.2) (locals () (f$0 (f$0 y.2))))])
      (locals () (+ (f$0 1) (g$1 1))))
    (letrec ([fact$0 (lambda (n.1) 
                       (locals () 
                         (if (= n.1 0) 1 (* n.1 (fact$0 (- n.1 1))))))])
      (locals () (fact$0 10)))
     (letrec ()
       (locals (a.1 b.2)
         (begin
           (set! a.1 5)
           (set! b.2 1)
           (set! b.2 (* b.2 a.1))
           (set! a.1 (- a.1 1))
           (set! b.2 (* b.2 a.1))
           (set! a.1 (- a.1 1))
           (set! b.2 (* b.2 a.1))
           (set! a.1 (- a.1 1))
           (set! b.2 (* b.2 a.1))
           b.2)))
     (letrec ()
       (locals (n.1 a.2)
         (begin
           (set! n.1 5)
           (begin
             (set! a.2 1)
             (begin
               (set! a.2 (* a.2 n.1))
               (begin
                 (set! n.1 (- n.1 1))
                 (begin
                   (set! a.2 (* a.2 n.1))
                   (begin
                     (set! n.1 (- n.1 1))
                     (begin
                       (set! a.2 (* a.2 n.1))
                       (begin
                         (set! n.1 (- n.1 1))
                         (begin
                           (set! a.2 (* a.2 n.1))
                           a.2)))))))))))
     (letrec ([double$0 (lambda (a.1)
                          (locals () (+ a.1 a.1)))])
       (locals () (double$0 10)))
     (letrec ([double$1 (lambda (x.1)
                          (locals ()
                            (* x.1 2)))])
       (locals () (begin (double$1 5))))
     (letrec ()
       (locals (x.5 y.10)
         (begin 
           (set! x.5 (begin (set! y.10 10) (set! x.5 15) (* y.10 x.5)))
           x.5)))

    (letrec ([f$0 (lambda (x.1) (locals () (+ 1 x.1)))]
             [g$1 (lambda (x.1) (locals () (- x.1 1)))]
             [t$2 (lambda (x.1) (locals () (- x.1 1)))]
             [j$3 (lambda (x.1) (locals () (- x.1 1)))]
             [i$4 (lambda (x.1) (locals () (- x.1 1)))]
             [h$5 (lambda (x.1) (locals () (- x.1 1)))])
      (locals (x.1 a.2 b.3 c.4)
        (begin
          (set! x.1 80)
          (set! a.2 (f$0 x.1))
          (set! b.3 (g$1 x.1))
          (set! c.4 (h$5 (i$4 (j$3 (t$2 x.1)))))
          (* a.2 (* b.3 (+ c.4 0))))))
    (letrec ([fact$0 (lambda (n.1)
                       (locals (t.2 t.3)
                         (if (= n.1 0)
                             1
                             (begin
                               (set! t.2 (- n.1 1))
                               (set! t.3 (fact$0 t.2))
                               (* n.1 t.3)))))])
      (locals () (fact$0 10)))
    (letrec ([fib$0 (lambda (n.1)
                      (locals ()
                        (if (if (= 0 n.1) (true) (= 1 n.1))
                            1
                            (+ (fib$0 (- n.1 1)) (fib$0 (- n.1 2))))))])
      (locals () (fib$0 10)))
    (letrec ([even$0 (lambda (n.1)
                       (locals ()
                         (if (= n.1 0)
                             1
                             (odd$1 (- n.1 1)))))]
             [odd$1 (lambda (n.1)
                      (locals ()
                        (if (= n.1 0)
                            0
                            (even$0 (- n.1 1)))))])
      (locals () (even$0 17)))
     (letrec ()
       (locals (x.1 y.2 result.3)
         (begin
           (set! result.3 (+ (if (begin 
                                   (set! x.1 5) 
                                   (set! y.2 10)
                                   (< 11 x.1))
                                 (+ x.1 y.2)
                                 (+ y.2 100))
                             (begin
                               (set! x.1 10)
                               (set! y.2 20)
                               (* x.1 y.2))))
           result.3)))
     (letrec ()
       (locals (x.5) 
         (begin (set! x.5 5) x.5)))
     (letrec ()
       (locals (x.5 y.6)
         (begin
           (set! x.5 5)
           (set! y.6 6)
           (+ x.5 y.6))))
     (letrec ([div$0 (lambda (x.1)
                       (locals ()
                         (begin 
                           (set! x.1 (sra x.1 1)) 
                           (div$1 x.1))))]
              [div$1 (lambda (result.1)
                       (locals () result.1))])
       (locals (label-temp.1)
         (begin
           (set! label-temp.1 div$0)
           (label-temp.1 64))))
     ;; Slow division
     (letrec ([expt$0 (lambda (n.1 m.2)
                        (locals ()
                          (if (= m.2 1)
                              n.1
                              (* n.1 (expt$0 n.1 (- m.2 1))))))]
              [div$1 (lambda (n.1 d.2)
                       (locals ()
                         (div-helper$2 31 (- (* 2 n.1) d.2) 
                                       (* d.2 (expt$0 2 32)) 0)))]
              [div-helper$2 (lambda (i.1 p.2 d.3 q.4)
                              (locals ()
                                (if (> 0 i.1)
                                    q.4
                                    (if (>= p.2 0)
                                        (div-helper$2 (- i.1 1)
                                                      (- (* 2 p.2) d.3)
                                                      d.3
                                                      (logor (expt$0 2 i.1)
                                                             q.4))
                                        (div-helper$2 (- i.1 1)
                                                      (- (* 2 (+ p.2 d.3)) d.3)
                                                      d.3
                                                      q.4)))))])
       (locals () (div$1 153 17)))
     (letrec ([setbit3$0 (lambda (x.1)
                           (locals ()
                             (begin
                               (set! x.1 (logor x.1 8))
                               (return$1 x.1))))]
              [return$1 (lambda (x.1)
                          (locals ()
                            (begin x.1)))])
       (locals ()
         (begin (setbit3$0 1))))
     (letrec ([zero?$0 (lambda (n.1)
                         (locals (x.5)
                           (begin
                             (set! x.5 0)
                             (set! x.5 (- x.5 n.1))
                             (set! x.5 (sra x.5 63))
                             (set! x.5 (logand x.5 1))
                             (return$1 x.5))))]
              [return$1 (lambda (x.5)
                          (locals () x.5))])
       (locals () (zero?$0 5)))
     (letrec ([sqr-double$0 (lambda (z.5)
                              (locals ()
                                (begin
                                  (set! z.5 (* z.5 z.5))
                                  (double$1 z.5))))]
              [double$1 (lambda (w.4)
                          (locals ()
                            (begin
                              (set! w.4 (+ w.4 w.4))
                              (return$3 w.4))))]
              [return$3 (lambda (result.1)
                          (locals () result.1))])
       (locals () (begin (sqr-double$0 3) (sqr-double$0 5))))
     ;; test interaction of already assigned frame-vars and
     ;; register allocator
     (letrec ([square$1 (lambda (x.1)
                          (locals ()
                            (begin (* x.1 x.1))))])
       (locals () (square$1 7)))
    ;;; Make sure effect ordering is preserved
    (letrec ([sum$1 (lambda (x.1 y.2 z.3 w.4)
                      (locals ()
                        (+ x.1 (+ y.2 (+ z.3 w.4)))))])
      (locals (a.1)
        (sum$1 (begin (set! a.1 1) a.1)
               (begin (set! a.1 2) a.1)
               (begin (set! a.1 3) a.1)
               (begin (set! a.1 4) a.1))))
   ; test nontail calls w/several arguments
    (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                    (locals ()
                      (if (= n.2 0)
                          (+ (* a.3 (* x.6 x.6)) (+ (* b.4 x.6) c.5))
                          (+ (f$1 (sra n.2 3)
                                  (+ a.3 (logand n.2 4))
                                  (+ b.4 (logand n.2 2))
                                  (+ c.5 (logand n.2 1))
                                  x.6)
                             1))))])
      (locals () (f$1 16434824 1 0 -1 7)))
    (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                    (locals ()
                      (if (= n.2 0)
                          (+ (* a.3 (* x.6 x.6)) (+ (* b.4 x.6) c.5))
                          (- (f$1 (sra n.2 3)
                                  (+ a.3 (logand n.2 4))
                                  (+ b.4 (logand n.2 2))
                                  (+ c.5 (logand n.2 1))
                                  x.6)
                             (g$0 n.2 a.3 b.4 c.5)))))]
             [g$0 (lambda (n.7 a.8 b.9 c.10)
                    (locals () (+ (- n.7 a.8) (- b.9 c.10))))])
      (locals () (f$1 16434824 1 0 -1 7)))
    (letrec ([square$0 (lambda (n.1) (locals () (* n.1 n.1)))])
      (locals () (square$0 10)))
    (letrec ([fact$0 (lambda (n.1)
                       (locals ()
                         (fact$1 n.1 1)))]
             [fact$1 (lambda (n.1 a.2)
                       (locals ()
                         (if (= n.1 0)
                             a.2
                             (fact$1 (- n.1 1) (* n.1 a.2)))))])
      (locals () (fact$0 10)))
    (letrec ([gcd$0 (lambda (x.1 y.2)
                      (locals ()
                        (if (= y.2 0) 
                            x.1 
                            (gcd$0 (if (> x.1 y.2) (- x.1 y.2) x.1)
                                   (if (> x.1 y.2) y.2 (- y.2 x.1))))))])
      (locals () (gcd$0 1071 1029)))

    (letrec ([sub1$1 (lambda (n.1) (locals () (- n.1 1)))]
             [fib$0 (lambda (n.1)
                      (locals ()
                        (if (= 0 n.1)
                            0
                            (if (= 1 n.1)
                                1
                                (+ (fib$0 (sub1$1 n.1))
                                   (fib$0 (sub1$1 (sub1$1 n.1))))))))])
      (locals () (fib$0 10)))
    (letrec ([ack$0 (lambda (m.1 n.2)
                      (locals (tmp.3)
                        (if (= m.1 0)
                            (+ n.2 1)
                            (if (if (> m.1 0) (= n.2 0) (false))
                                (ack$0 (- m.1 1) 1)
                                (begin
                                  (set! tmp.3 (ack$0 m.1 (- n.2 1)))
                                  (ack$0 (- m.1 1) tmp.3))))))])
      (locals () (ack$0 2 4)))
    (letrec ([ack$0 (lambda (m.1 n.2)
                      (locals ()
                        (if (= m.1 0)
                            (+ n.2 1)
                            (if (if (> m.1 0) (= n.2 0) (false))
                                (ack$0 (- m.1 1) 1)
                                (ack$0 (- m.1 1) (ack$0 m.1 (- n.2 1)))))))])
      (locals () (ack$0 2 4)))
    (letrec ([fib$0 (lambda (n.1) (locals () (fib$1 n.1 0 1)))]
             [fib$1 (lambda (n.1 a.2 b.3)
                      (locals ()
                        (if (= n.1 0)
                            a.2
                            (fib$1 (- n.1 1) b.3 (+ b.3 a.2)))))])
      (locals () (fib$0 5)))
    (letrec ([if-test$1 (lambda ()
                           (locals (x.5)
                             (* (if (begin (set! x.5 5) (= x.5 5))
                                    (+ x.5 10)
                                    (- x.5 10)) 10)))])
       (locals () (if-test$1)))
    (letrec ([if-test$2 (lambda ()
                           (locals (x.5)
                             (begin
                               (set! x.5 (if (begin
                                               (set! x.5 7)
                                               (if (< x.5 1)
                                                   (false)
                                                   (< x.5 10)))
                                           (* x.5 2)
                                           (+ x.5 5)))
                               x.5)))])
       (locals () (if-test$2)))
    (letrec ([if-test$3 (lambda (n.1)
                           (locals ()
                             (begin
                               (if (if (= n.1 0)
                                       (true)
                                       (if (= n.1 1) (true) (= n.1 2)))
                                   (* n.1 5)
                                   (- n.1 5)))))])
       (locals () (if-test$3 2)))
    (letrec ([if-test$4 (lambda (x.5)
                           (locals ()
                             (begin
                               (* (if (if (= x.5 10) (false) (true))
                                      (+ x.5 10)
                                      (- x.5 2))
                                  10))))])
      (locals () (if-test$4 2)))
    (letrec ([if-test$5 (lambda (n.1 x.2 y.3)
                           (locals ()
                             (begin
                               (if (= n.1 0)
                                   (set! x.2 (+ x.2 y.3))
                                   (set! y.3 (+ y.3 x.2)))
                               (set! x.2 (+ x.2 n.1))
                               (if (if (= n.1 y.3) (false) (true))
                                   (+ n.1 x.2)
                                   (+ n.1 y.3)))))])
       (locals () (begin (if-test$5 1 1 1))))
    (letrec ([if-test$6 (lambda (n.1)
                           (locals (x.2 y.3)
                             (begin
                               (set! x.2 1)
                               (begin
                                 (set! y.3 1)
                                 (if (= n.1 0)
                                     (set! x.2 (+ x.2 y.3))
                                     (set! y.3 (+ y.3 x.2)))
                                 (set! x.2 (+ x.2 n.1)))
                               (if (if (= n.1 y.3) (false) (true))
                                   (set! n.1 (+ n.1 x.2))
                                   (set! n.1 (+ n.1 y.3)))
                               (+ x.2 n.1))))])
       (locals ()(if-test$6 1)))
    (letrec ()
       (locals (x.1 y.2 z.3)
         (begin
           (set! x.1 0)
           (set! y.2 1)
           (if (if (= x.1 0) (= y.2 1) (false))
               (set! z.3 5)
               (begin (set! z.3 5) (set! z.3 (+ z.3 z.3))))
           z.3)))
    (letrec ()
       (locals (a.1 b.2 c.3)
         (begin
           (set! a.1 0)
           (set! b.2 0)
           (if (if (= a.1 0) (= b.2 1) (false))
               (set! c.3 5)
               (begin (set! c.3 5) (set! c.3 (+ c.3 c.3))))
           c.3)))
    (letrec ([main$0 (lambda (x.1 y.2)
                       (locals (z.3)
                         (begin
                           (if (if (= x.1 1) (true) (= y.2 1))
                               (set! z.3 1)
                               (set! z.3 0))
                           (* z.3 5))))])
      (locals () (main$0 1 0)))
    (letrec ([main$0 (lambda (a.1 b.2)
                       (locals (c.3)
                         (begin
                           (set! c.3 
                             (if (if (= a.1 1) (true) (= b.2 1))
                                 1
                                 0))
                           (+ c.3 5))))])
      (locals () (main$0 0 1)))
    (letrec ([main$0 (lambda (a.1 b.2)
                       (locals ()
                         (begin
                           (if (if (= a.1 1) (= b.2 1) (true))
                               (set! a.1 1)
                               (set! b.2 0))
                           (set! b.2 (* b.2 10))
                           (set! a.1 (+ a.1 b.2))
                           a.1)))])
       (locals () (main$0 0 1)))
    (letrec ([main$0 (lambda (a.1 b.2)
                       (locals ()
                         (if (if (= a.1 1) (= b.2 1) (true)) 1 0)))])
      (locals () (main$0 1 0)))
    (letrec ([main$0 (lambda (a.1 b.2)
                       (locals ()
                         (if (if (= a.1 1) (= b.2 1) (true)) 1 0)))])
      (locals () (main$0 0 0)))
    (letrec ()
       (locals (a.1 b.2)
         (begin
           (set! a.1 1)
           (set! b.2 1)
           (if (if (= a.1 1) (= b.2 1) (true)) 1 0))))
    (letrec ()
      (locals (n.1 a.2 b.3 c.4)
        (begin
          (set! n.1 1)
          (begin
            (set! a.2 2)
            (begin
              (set! b.3 3)
              (set! n.1 (+ n.1 (if (= (+ n.1 b.3) b.3) 5 10)))
              (set! n.1 (+ n.1 b.3)))
            (set! n.1 (+ n.1 a.2)))
          (+ n.1 n.1))))
    (letrec ()
       (locals (a.1 b.2 c.3 d.4 e.5)
         (begin
           (set! a.1 1)
           (set! b.2 2)
           (set! c.3 3)
           (set! d.4 4)
           (set! e.5 5)
           (+ (+ (+ (+ e.5 d.4) c.3) b.2) a.1))))
    (letrec ()
       (locals (a.1 b.2 c.3 d.4 e.5 f.6)
         (begin
           (set! a.1 1)
           (set! b.2 2)
           (set! c.3 3)
           (set! d.4 4)
           (set! e.5 5)
           (set! f.6 6)
           (set! a.1 
             (if (> (+ a.1 d.4) f.6)
               (* a.1 (+ c.3 f.6))
               (* a.1 (+ b.2 e.5))
               ))
           a.1)))
    (letrec ([dot$0 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                      (locals ()
                        (+ (* a.1 b.5) 
                           (+ (* a.2 b.6) 
                              (+ (* a.3 b.7) (* a.4 b.8))))))])
      (locals () (dot$0 2 4 6 8 1 3 5 7)))
    (letrec ([dot-double-first$1 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                                   (locals ()
                                     (dot$0 (+ a.1 a.1) (+ a.2 a.2)
                                            (+ a.3 a.3) (+ a.4 a.4)
                                            b.5 b.6 b.7 b.8)))]
             [dot$0 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                      (locals ()
                        (+ (* a.1 b.5) 
                           (+ (* a.2 b.6) 
                              (+ (* a.3 b.7) (* a.4 b.8))))))])
      (locals () (dot-double-first$1 2 4 6 8 1 3 5 7)))
    (letrec ([dot-double-first$1 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                                   (locals ()
                                     (begin
                                       (set! a.1 (+ a.1 a.1))
                                       (set! a.2 (+ a.2 a.2))
                                       (set! a.3 (+ a.3 a.3))
                                       (set! a.4 (+ a.4 a.4))
                                       (dot$0 a.1 a.2 a.3 a.4
                                              b.5 b.6 b.7 b.8))))]
             [dot$0 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                      (locals ()
                        (+ (* a.1 b.5) 
                           (+ (* a.2 b.6) 
                              (+ (* a.3 b.7) (* a.4 b.8))))))])
      (locals () (dot-double-first$1 2 4 6 8 1 3 5 7)))
     
    ;; stress the register allocator, but not so much that it needs to spill.
    (letrec ()
      (locals (a.1 b.2 c.3 d.4 e.5 f.6 g.7 h.8 i.9 j.10 k.11 l.12 m.13 n.14 
               o.15 p.16 q.17 r.18 s.19 t.20 u.21 v.22 w.23 x.24 y.25 z.26)
        (begin
          (set! a.1 1)
          (set! b.2 2)
          (set! c.3 3)
          (set! d.4 4)
          (set! e.5 5)
          (set! f.6 6)
          (set! g.7 7)
          (set! h.8 8)
          (set! i.9 9)
          (set! j.10 10)
          (set! k.11 11)
          (set! l.12 12)
          (set! m.13 13)
          (set! a.1 (+ (- (+ a.1 b.2) (+ (- c.3 d.4) e.5)) f.6))
          (set! a.1 (+ (- a.1 g.7) (+ h.8 (- i.9 (+ j.10 k.11)))))
          (set! a.1 (+ a.1 (+ l.12 m.13)))
          (set! n.14 14)
          (set! o.15 15)
          (set! p.16 16)
          (set! q.17 17)
          (set! r.18 18)
          (set! s.19 19)
          (set! t.20 20)
          (set! u.21 21)
          (set! v.22 22)
          (set! w.23 23)
          (set! x.24 24)
          (set! y.25 25)
          (set! a.1 (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ a.1 n.14) o.15) p.16)
                       q.17) r.18) s.19) t.20) u.21) v.22) w.23) x.24) y.25))
          (set! z.26 26)
          (set! b.2 27)
          (set! c.3 28)
          (set! d.4 29)
          (set! e.5 30)
          (set! f.6 31)
          (set! g.7 32)
          (set! h.8 33)
          (set! i.9 34)
          (set! j.10 35)
          (set! k.11 36)
          (set! l.12 37)
          (+ a.1 (+ z.26 (+ b.2 (+ c.3 (+ d.4 (+ e.5 (+  f.6 (+ g.7 (+ h.8
             (+ i.9 (+ j.10 (+ k.11 l.12)))))))))))))))

    ;; another stress test, which should fail unless low-degree
    ;; nodes are chosen properly
     (letrec ()
       (locals (b.2 g.7 c.3 d.4 e.5 a.1 f.6)
         (begin
           (set! a.1 1)
           (set! b.2 2)
           (set! c.3 a.1)
           (set! d.4 4)
           (set! e.5 5)
           (set! f.6 b.2)
           (set! f.6 (+ f.6 c.3))
           (set! f.6 (+ f.6 d.4))
           (set! f.6 (+ f.6 e.5))
           (set! g.7 7)
           (+ f.6 g.7))))
 
   ;; another variant of latter 
    (letrec ()
      (locals (b.2 g.7 c.3 d.4 e.5 a.1 f.6 h.8 i.9 j.10 k.11)
        (begin
          (set! h.8 77)
          (set! i.9 88)
          (set! j.10 99)
          (set! k.11 111)
          (set! a.1 1)
          (set! b.2 2)
          (set! c.3 a.1)
          (set! d.4 4)
          (set! e.5 5)
          (set! f.6 b.2)
          (set! f.6 (+ f.6 c.3))
          (set! f.6 (+ f.6 d.4))
          (set! f.6 (+ f.6 e.5))
          (set! g.7 7)
          (set! f.6 (+ f.6 g.7))
          (set! f.6 (+ f.6 i.9))
          (set! f.6 (+ f.6 j.10))
          (set! f.6 (+ f.6 k.11))
          (+ f.6 h.8)))) 

    ;; The following makes a.1 of high degree and since no variable
    ;; can be spilled causes an error in the register allocator
    (letrec ()
      (locals (a.1 b.2 c.3 d.4 e.5 f.6 g.7 h.8 i.9 j.10 k.11 l.12 m.13 n.14 
               o.15 p.16 q.17 r.18 s.19 t.20 u.21 v.22 w.23 x.24 y.25 z.26)
        (begin
          (set! a.1 1)
          (set! b.2 2)
          (set! c.3 3)
          (set! d.4 4)
          (set! e.5 5)
          (set! f.6 6)
          (set! g.7 7)
          (set! h.8 8)
          (set! i.9 9)
          (set! j.10 10)
          (set! k.11 11)
          (set! l.12 12)
          (set! m.13 13)
          (set! n.14 14)
          (set! o.15 15)
          (set! p.16 16)
          (set! q.17 17)
          (set! r.18 18)
          (set! s.19 19)
          (set! t.20 20)
          (set! u.21 21)
          (set! v.22 22)
          (set! w.23 23)
          (set! x.24 24)
          (set! y.25 25)
          (set! z.26 26)
          (set! a.1 (+ a.1 (+ b.2 (+ c.3 (+ d.4 (+ e.5 (+ f.6 (+ g.7 (+ h.8
                    (+ i.9 (+ j.10 (+ k.11 (+ l.12 (+ m.13 (+ n.14 (+ o.15 
                    (+ p.16 (+ q.17 (+ r.18 (+ s.19 (+ t.20 (+ u.21 (+ v.22 
                    (+ w.23 (+ x.24 (+ y.25 z.26))))))))))))))))))))))))))
          (set! b.2 27)
          (set! c.3 28)
          (set! d.4 29)
          (set! e.5 30)
          (set! f.6 31)
          (set! g.7 32)
          (set! h.8 33)
          (set! i.9 34)
          (set! j.10 35)
          (set! k.11 36)
          (set! l.12 37)
          (set! m.13 38)
          (set! n.14 39)
          (set! o.15 40)
          (set! a.1 (+ a.1 (+ b.2 (+ c.3 (+ d.4 (+ e.5 (+ f.6 (+ g.7 (+ h.8
                    (+ i.9 (+ j.10 (+ k.11 (+ l.12 (+ m.13 
                    (+ n.14 o.15)))))))))))))))
          a.1)))
    ))
                         
                                  

