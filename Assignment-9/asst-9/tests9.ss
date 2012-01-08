(define invalid-tests 
  '(3
    (begin rax 5)
    (letrec () (set! rax 5))
    (letrec () (set! rax 5) (r15))
    (letrec () (begin (set! rax 5) (r15)))
    (letrec ([f$5 (lambda (rax) (begin (r15)))]) (f$5))
    (letrec () (letrec () 7))
    (letrec () (locals () 3))
    (letrec ([f$17 (lambda () (locals () 7))]) (f$17))
    (letrec (locals () (begin (set! rax 5) (r15 rax))))
    (letrec () (begin (set! x.1 5.5) x.1))
    (letrec ([double$1 (lambda ()
                         (let ([x.1 1])
                           x.1))]
             [triple$1 (lambda ()
                         (let ([x.2 (* x.2 3)])
                           x.2))])
      (double$1))
    (letrec ([foo (lambda () (let ([a.1 5]) a.1))]) (foo))
    (letrec ([foo$0 (lambda () (let ([a.1 5]) a.1))]) (bar$1))
    (letrec ([even?$1 (lambda (n.1)
                        (if (= 0 n.1)
                            1
                            (odd?$2 (- n.1 1))))]
             [odd?$2 (lambda (n.1)
                       (if (= 0 n.1)
                           0
                           (even?$1 (- n.1 1))))])
      (if (odd?$1 17)
          10
          0))
    (letrec () x) 
    (letrec () 9223372036854775808)
    (letrec () -9223372036854775809)
    (letrec () 
      (let ([x.1 (set! x.1 0)]) (+ x.1 9223372036854775808)))
    (letrec () (let ([x.1 0]) (+ x.1 -9223372036854775809)))
    (letrec () 12.5)
    (letrec () (let ([x.1 5]) (let ([x.2 (sra x.1 -1)]) x.2)))
    (letrec () (let ([x.1 5]) (let ([x.2 (sra x.1 64)]) x.2)))
    (letrec () (let ([x.1 5] [y.2 6]) (let ([x.2 (sra x.1 y.2)]) x.2)))
    (letrec ([foo$1 (lambda () 7)]) (let ([x.1 5]) (sra x.1 foo$1)))
    (letrec () (let ([x.1 15]) (/ x.1 5)))
    (letrec ()
      (let ([x.1 7] [y.2 8] [z.3 9])
        (if (+ z.3 5) (x.1) (z.3))))
    (letrec ()
      (let ([x.1 (alloc 8)])
        (begin
          (mset! x.1 0 0)
          (if (= (mref x.1 0) 0) (nop) (mref x.1 0))
          (mset! x.1 0 5)
          (mref x.1 0))))
    (letrec ()
      (let ([x.1 (alloc 8)])
        (begin
          (mset! x.1 0 0)
          (if (= (mref x.1 0) 0) (nop) (> x.1 9))
          (mset! x.1 0 5)
          (mref x.1 0))))
    (letrec ()
      (let ([x.1 1])
        (begin (if (let ([x.2 10]) x.2) x.1 x.1))))
    (letrec ([foo$1 (lambda () 7)])
      (begin (if (let ([x.1 10]) (foo$1)) (foo$1) (foo$1))))
    (letrec ()
      (let ([x.1 1])
        (begin (if (let ([x.2 10]) (nop)) x.1 x.1))))
    (letrec ()
      (let ([x.1 (alloc 8)] [x2 (alloc 8)] [x3 (alloc 8)])
        (begin
          (if (begin (mset! x.1 0 10) (mset! y.2 0 10) (= (mref x.1 0) (mref y.2 0)))
              (set! (mref z.3 0) 10)
              (mref x.1 0)))))
    (letrec ()
      (let ([x.1 (alloc 8)] [x2 (alloc 8)] [x3 (alloc 8)])
        (begin
          (if (begin (mset! x.1 0 10) (nop) (= (mref x.1 0) (mref y.2 0)))
              (mset! z.3 0 10)
              (mref x.1 0)))))
    (letrec ([main$0 (lambda ()
                       (let ([y.2 5])
                         (let ([x.1 y.2] [z.3 15])
                           (if (+ z.3 5) x.1 y.2))))])
      (main$0))
    (letrec ([main$0 (lambda ()
                       (let ([x.1 (alloc 8)])
                         (begin
                           (if (begin (mset! x.1 0 10) (nop))
                               (mref x.1 0)
                               (mref x.1 0)))))])
      (main$0))
    (letrec ([foo$1 (lambda () 7)]
             [main$0 (lambda ()
                       (begin
                         (if (let ([x.1 10]) (nop))
                             (foo$1)
                             (foo$1))))])
      (main$0))
    (letrec ([main$0 (lambda ()
                       (let ([x.1 (alloc 8)] [y.2 10])
                         (begin
                           (if (begin
                                 (mset! x.1 0 10)
                                 (= (mref x.1 0) y.2))
                               (mset! x.1 0 10)
                               (mref x.1 0)))))])
      (main$0))
    (letrec ([main$0 (lambda ()
                       (let ([x.1 15])
                         (if (= 0 x.1) (f$0) (f$1))))]
             [f$0 (lambda () 5)]
             [f$1 (lambda () 6)])
      (main$0))
    (letrec ()
      (let ([x.1 17]) (if (= x.1 9223372036854775808) x.1 x.1)))
    (letrec ()
      (let ([x.1 17])
        (if (= x.1 -9223372036854775809) x.1 x.1)))
    (letrec ()
      (let ([x.1 4]) (if (= x.1 12.5) x.1 x.1)))
    (letrec ()
      (let ([x.1 (alloc 8)])
        (begin
          (mset! x.1 0 7)
          (if (if (< (mref x.1 0) 10) (if (< (mref x.1 0) 5) (false) (true)) #f)
              (begin (mset! x.1 0 (* (mref x.1 0) (mref x.1 0))) (mref x.1 0))
              (mref x.1 0)))))
    (letrec () (let ([x 15]) x))
    (letrec ()
      (let ([x.1 (alloc 8)] [y.2 (alloc 8)])
        (begin
          (mset! x.1 0 5)
          (mset! y.2 0 2)
          (mset! z.5 0 (+ (mref z.5 0) (mref x.1 0)))
          (mset! z.5 0 (+ (mref z.5 0) (mref x.1 0)))
          z.5)))
    (letrec ()
      (let ([x.1 (alloc 8)] [y.1 (alloc 8)])
        (begin
          (mset! x.1 0 (true))
          (if x.1 (mset! y.2 0 5) (mset! y.2 0 100))
          y.2)))
    (letrec ()
      (let ([x.1 (alloc 8)] [y.2 0] [acc.3 (alloc 8)])
        (begin
          (mset! x.1 0 5)
          (mset! acc.3 0 0)
          loop$1
          (mset! acc.3 0 (+ (mref acc.3 0) (mref x.1 0)))
          (mset! x.1 0 (- (mref x.1 0) 1))
          (if (= y.2 (mref x.1 0)) (nop) (loop$1))
          (mref acc.3 0))))
    (letrec ([double$1 (lambda (a.3 b.4) 3)])
      (let ([x.2 5] [fv0 17])
        (double$1 x.2 fv0)))
    (letrec ([double$1 (lambda () 3)])
      (let ([x.2 5] [fv0 17])
        (double$1 x.2 fv0)))
    (letrec ([double$1 (lambda (a.3 b.4) 3)])
      (let ([x.2 5] [fv0 17])
        (double$1 x.2 17)))
    (letrec ()
      (let ([x.1 rax])
        (+ x.1 5)))
    (letrec ()
      (let ([x.1 4])
        (+ x.1 rax)))
    (letrec ([f$1 (lambda (x.3) x.3)]
             [g$2 (lambda (y.3) y.3)])
      (f$1 (g$2 17)))
    (letrec ([f$1 (lambda (x.3) x.3)]
             [g$2 (lambda (y.4) y.4)])
      (let ([z.4 17]) (f$1 (g$2 z.4))))
    (letrec ([f$1 (lambda (x.3) (let ([x.4 x.3]) x.4))]
             [g$2 (lambda (y.4) y.4)])
      (let ([z.5 17]) (f$1 (g$2 z.5))))
    (letrec ([f$1 (lambda (x.3) x.3)]
             [g$2 (lambda (y.4) y.4)])
      (let ([z.5 (alloc 8)]) (mset! z.5 0 15) (f$1 (g$2 (mref z.5 0)))))
    (letrec ([f$1 (lambda (x.3) x.3)]
             [g$2 (lambda (y.4) y.4)])
      (begin
        (let ([z.5 (alloc 8)]) (mset! z.5 0 15) (f$1 (g$2 (mref z.5 0))))
        7))
    (letrec ([f$1 (lambda (x.3) x.3)]
             [g$2 (lambda (y.4) y.4)])
      (if (let ([z.5 (alloc 8)])
            (mset! z.5 0 15)
            (= (mref z.5 0) 7))
          17
          45))
    (letrec ()
      (+ (let ([z.5 (alloc 8)])
           (mset! z.5 0 15)
           (+ (mref z.5 0) 7))
          17))
  ))

(define tests 
  '((letrec () 7)
    (letrec () (+ 5 7))
    (letrec () (+ 7 (* 5 7)))
    (letrec () (* (+ 2 4) (+ (+ 6 7) 4)))
    (letrec () 
      (if (= (+ 7 (* 2 4)) (- 20 (+ (+ 1 1) (+ (+ 1 1) 1))))
          (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 10)))))
          0))
    (letrec ()
      (let ([a.1 10])
        (let ([b.2 (if (< 7 a.1) a.1 (+ a.1 a.1))])
          b.2)))
    (letrec ()
      (let ([a.1 5])
        (let ([b.2 (if (< 7 a.1) a.1 (+ a.1 a.1))])
          b.2)))
    (letrec ()
      (let ([c.1 10] [a.2 5])
        (if (< a.2 c.1) a.2 c.1)))
    (letrec ()
      (let ([a.1 5])
        (let ([b.2 (if (< a.1 10) (+ a.1 a.1) a.1)])
          b.2)))
    (letrec ([f$0 (lambda (x.1) (+ 1 x.1))])
      (f$0 (let ([f.2 3]) (+ f.2 1))))
    (letrec ([f$0 (lambda (h.1 v.2) (* h.1 v.2))]
             [k$1 (lambda (x.3) (+ x.3 5))]
             [g$2 (lambda (x.4) (+ 1 x.4))])
      (let ([x.5 15])
        (k$1 (g$2 (let ([g.6 3]) (f$0 g.6 x.5))))))
    (letrec ([one$1 (lambda (n.1) 
                      (if (= 0 n.1) 1 (one$1 (- n.1 1))))])
       (one$1 13))
    (letrec ([f$0 (lambda (p.2) (- (mref p.2 8) (mref p.2 0)))])
      (let ([x.1 (alloc 16)])
        (begin
          (mset! x.1 0 73)
          (mset! x.1 8 35)
          (f$0 x.1))))
    (letrec ([f$0 (lambda (p.2 i.3 i.4) (- (mref p.2 i.3) (mref p.2 i.4)))])
      (let ([x.1 (alloc 16)])
        (begin
          (mset! x.1 0 73)
          (mset! x.1 8 35)
          (+ (f$0 x.1 0 8) -41))))
    (letrec ([f$0 (lambda (p.3)
                    (- (mref
                         (mref (mref (mref (mref p.3 0) 0) 8) 0)
                         (mref (mref p.3 8) (mref (mref p.3 0) 32)))
                       (mref
                         (mref p.3 (mref p.3 16))
                         (mref (mref p.3 0) (mref p.3 32)))))])
      (let ([x.1 (alloc 48)] [x.2 (alloc 56)])
        (begin
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
                              (let ([v.2 (alloc (+ (* size.1 8) 8))])
                                (begin
                                  (mset! 0 v.2 size.1)
                                  v.2)))]
             [chained-vector-set!$1 (lambda (v.3 off.4 val.5)
                                      (begin
                                        (mset! (* (+ off.4 1) 8) v.3 val.5)
                                        v.3))]
             [vector-length$4 (lambda (v.8) (mref v.8 0))]
             [find-greatest-less-than$2 (lambda (v.6 val.7)
                                          (fglt-help$3 v.6 val.7 (+ v.6 8)
                                            (vector-length$4 v.6)))]
             [fglt-help$3 (lambda (v.9 val.10 curr.11 size.12)
                            (if (if (> curr.11 (+ (+ v.9 (* size.12 8)) 8))
                                    (true)
                                    (> (mref curr.11 0) val.10))
                                (mref curr.11 -8)
                                (fglt-help$3 v.9 val.10 (+ curr.11 8)
                                             size.12)))])
      (let ([v.13 (chained-vector-set!$1
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
                    9 90)])
        (find-greatest-less-than$2 v.13 76)))
    (letrec ([vector-scale!$0 (lambda (vect.1 scale.2)
                                (let ([size.3 (mref vect.1 0)])
                                  (vector-scale!$1 size.3 vect.1 scale.2)))]
             [vector-scale!$1 (lambda (offset.4 vect.5 scale.6)
                                (if (< offset.4 1)
                                    0
                                    (begin
                                      (mset! vect.5 (* offset.4 8)
                                             (* (mref vect.5 (* offset.4 8))
                                                scale.6))
                                      (vector-scale!$1 (- offset.4 1)
                                                       vect.5 scale.6))))]
             [vector-sum$2 (lambda (vect.7)
                             (vector-sum$3 (mref vect.7 0) vect.7))]
             [vector-sum$3 (lambda (offset.9 vect.10)
                             (if (< offset.9 1)
                                 0
                                 (+ (mref vect.10 (* offset.9 8))
                                    (vector-sum$3 (- offset.9 1)
                                                  vect.10))))])
      (let ([vect.11 (alloc 48)])
        (begin
          (mset! vect.11 0 5)
          (mset! vect.11 8 123)
          (mset! vect.11 16 10)
          (mset! vect.11 24 7)
          (mset! vect.11 32 12)
          (mset! vect.11 40 57)
          (vector-scale!$0 vect.11 10)
          (vector-sum$2 vect.11))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (let ([ptr.3 (alloc 16)])
                       (begin
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
             [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
             [length$3 (lambda (ptr.6)
                         (if (= ptr.6 0)
                             0
                             (+ 1 (length$3 (snd$2 ptr.6)))))])
      (length$3 (::$0 5 (::$0 10 (::$0 11 (::$0 5 (::$0 15 0)))))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (let ([ptr.3 (alloc 16)])
                       (begin
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
             [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
             [count-leaves$3 (lambda (ptr.6)
                               (if (= ptr.6 0)
                                   1
                                   (+ (count-leaves$3 (fst$1 ptr.6))
                                      (count-leaves$3 (snd$2 ptr.6)))))])
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
                  (::$0 (::$0 0 0) 0))))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (let ([ptr.3 (alloc 16)])
                       (begin
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
             [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
             [add1$3 (lambda (n.6) (+ n.6 1))]
             [map$4 (lambda (f.7 ls.8)
                      (if (= ls.8 0)
                          0
                          (::$0 (f.7 (fst$1 ls.8)) 
                                (map$4 f.7 (snd$2 ls.8)))))]
             [sum$5 (lambda (ls.9)
                      (if (= 0 ls.9)
                          0
                          (+ (fst$1 ls.9) (sum$5 (snd$2 ls.9)))))])
      (let ([ls.10 (::$0 5 (::$0 4 (::$0 3 (::$0 2 (::$0 1 0)))))])
        (let ([ls.11 (::$0 10 (::$0 9 (::$0 8 (::$0 7 (::$0 6 ls.10)))))])
          (sum$5 (map$4 add1$3 ls.11)))))
    (letrec ([::$0 (lambda (fst.1 snd.2)
                     (let ([ptr.3 (alloc 16)])
                       (begin
                         (mset! ptr.3 0 fst.1)
                         (mset! ptr.3 8 snd.2)
                         ptr.3)))]
             [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
             [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
             [list-ref$3 (lambda (ls.11 offset.12)
                           (if (= offset.12 0)
                               (fst$1 ls.11)
                               (list-ref$3 (snd$2 ls.11) (- offset.12 1))))]
             [add$6 (lambda (v.13 w.14) (+ v.13 w.14))]
             [sub$7 (lambda (v.15 w.16) (- v.15 w.16))]
             [mult$8 (lambda (v.17 w.18) (* v.17 w.18))]
             [expt$9 (lambda (v.217 w.218) 
                       (if (= w.218 0)
                           1
                           (* v.217 (expt$9 v.217 (- w.218 1)))))]
             [selector$4 (lambda (op*.7 sel.19 rand1.20 rand2.21)
                           (if (= sel.19 0)
                               0
                               (::$0 ((list-ref$3 op*.7 (fst$1 sel.19))
                                      (fst$1 rand1.20) (fst$1 rand2.21))
                                     (selector$4 op*.7 (snd$2 sel.19)
                                                 (snd$2 rand1.20)
                                                 (snd$2 rand2.21)))))]
             [sum$5 (lambda (ls.9)
                      (if (= 0 ls.9)
                          0
                          (+ (fst$1 ls.9) (sum$5 (snd$2 ls.9)))))])
      (sum$5 (selector$4 
               (::$0 add$6 (::$0 sub$7 (::$0 mult$8 (::$0 expt$9 0))))
               (::$0 2 (::$0 0 (::$0 1 (::$0 3 (::$0 2 0)))))
               (::$0 5 (::$0 9 (::$0 10 (::$0 2 (::$0 3 0)))))
               (::$0 3 (::$0 1 (::$0 3 (::$0 3 (::$0 8 0))))))))
    (letrec ([thunk-num$0 (lambda (n.1)
                            (let ([th.2 (alloc 16)])
                              (begin 
                                (mset! th.2 0 force-th$1)
                                (mset! th.2 8 n.1)
                                th.2)))]
             [force-th$1 (lambda (cl.3)
                           (mref cl.3 8))]
             [add-ths$2 (lambda (cl1.4 cl2.5 cl3.6 cl4.7)
                          (+ (+ ((mref cl1.4 0) cl1.4)
                                ((mref cl2.5 0) cl2.5))
                             (+ ((mref cl3.6 0) cl3.6)
                                ((mref cl4.7 0) cl4.7))))])
      (add-ths$2 (thunk-num$0 5) (thunk-num$0 17) (thunk-num$0 7)
                 (thunk-num$0 9)))
    (letrec ([make-vector$0 (lambda (size.1)
                              (let ([v.20 (alloc (* (+ size.1 1) 8))])
                                (begin
                                  (mset! 0 v.20 size.1)
                                  v.20)))]
             [vector-set!$1 (lambda (vect.2 off.3 val.4)
                              (begin
                                (if (> off.3 (mref vect.2 0))
                                    (nop)
                                    (mset! (* (+ off.3 1) 8) vect.2 val.4))
                                0))]
             [vector-equal?$3 (lambda (vect1.8 vect2.9)
                                (if (= (mref 0 vect1.8) (mref 0 vect2.9))
                                    (vector-equal?$4 vect1.8 vect2.9
                                                     (mref 0 vect1.8))
                                    0))]
             [vector-equal?$4 (lambda (vect1.11 vect2.12 off.10)
                                (if (< off.10 0)
                                    1 
                                    (if (= (mref (* (+ off.10 1) 8) vect1.11)
                                           (mref vect2.12 (* (+ off.10 1) 8)))
                                        (vector-equal?$4 vect1.11 vect2.12
                                                         (- off.10 1))
                                        0)))])
      (let ([v1.13 (make-vector$0 5)])
        (begin
          (vector-set!$1 v1.13 0 134)
          (vector-set!$1 v1.13 1 123)
          (vector-set!$1 v1.13 2 503)
          (vector-set!$1 v1.13 3 #x66)
          (vector-set!$1 v1.13 4 #xff)
          (let ([v2.14 (make-vector$0 5)])
            (begin
              (vector-set!$1 v2.14 0 134)
              (vector-set!$1 v2.14 1 123)
              (vector-set!$1 v2.14 2 503)
              (vector-set!$1 v2.14 3 #x66)
              (vector-set!$1 v2.14 4 #xff)
              (if (= (vector-equal?$3 v1.13 v2.14) 0)
                  100
                  -100))))))

    ;;; A little OOP
    (letrec ([stack-new$0 (lambda (size.1)
                            (let ([store.3 (alloc (* 8 size.1))]
                                  [meths.4 (alloc (* 3 8))]
                                  [stack.2 (alloc (* 3 8))])
                              (begin
                                (mset! meths.4 0 stack-push$2)
                                (mset! meths.4 8 stack-pop$3)
                                (mset! meths.4 16 stack-top$4)
                                (mset! stack.2 0 meths.4)
                                (mset! stack.2 8 0)
                                (mset! stack.2 16 store.3)
                                stack.2)))]
             [invoke$1 (lambda (obj.5 meth-idx.6)
                         (mref (mref obj.5 0) (* meth-idx.6 8)))]
             [stack-push$2 (lambda (self.7 val.8)
                             (begin
                               (mset! (mref self.7 16) 
                                      (* (mref self.7 8) 8)
                                      val.8)
                               (mset! self.7 8 (+ (mref self.7 8) 1))
                               self.7))]
             [stack-pop$3 (lambda (self.9)
                            (begin
                              (mset! self.9 8 (- (mref 8 self.9) 1))
                              (mref (mref self.9 16) 
                                    (* (mref self.9 8) 8))))]
             [stack-top$4 (lambda (self.209)
                            (mref (mref self.209 16) 
                                  (* (- (mref 8 self.209) 1) 8)))])
      (let ([s1.10 (stack-new$0 10)])
        (begin
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
          (let ([s2.11 (stack-new$0 5)])
            (begin
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
                    (+ ((invoke$1 s2.11 2) s2.11) ((invoke$1 s2.11 1) s2.11))))))))))
    (letrec ([a$0 (lambda (u.1 v.2 w.3 x.4) 
                    (if (= u.1 0) 
                        (b$1 v.2 w.3 x.4)
                        (a$0 (- u.1 1) v.2 w.3 x.4)))]
             [b$1 (lambda (q.5 r.6 x.7)
                    (let ([p.8 (* q.5 r.6)])
                      (e$3 (* q.5 r.6) p.8 x.7)))]
             [c$2 (lambda (x.9) (* 5 x.9))]
             [e$3 (lambda (n.10 p.11 x.12)
                    (if (= n.10 0) 
                        (c$2 p.11)
                        (o$4 (- n.10 1) p.11 x.12)))]
             [o$4 (lambda (n.13 p.14 x.15) 
                    (if (= 0 n.13)
                        (c$2 x.15)
                        (e$3 (- n.13 1) p.14 x.15)))])
      (let ([x.16 5])
        (a$0 3 2 1 x.16)))
    (letrec ([f$0 (lambda () 80)])
      (let ([a.1 (f$0)] [b.2 (f$0)])
        (* a.1 b.2)))
    (letrec ([f$0 (lambda () 80)]
             [g$1 (lambda () 50)])
      (let ([a.1 (f$0)] [b.2 (g$1)])
        (* a.1 b.2)))
    (letrec ([f$0 (lambda (x.1) (+ x.1 1))]
             [g$1 (lambda (y.2) (f$0 (f$0 y.2)))])
      (+ (f$0 1) (g$1 1)))
    (letrec ([fact$0 (lambda (n.1) 
                       (if (= n.1 0) 1 (* n.1 (fact$0 (- n.1 1)))))])
      (fact$0 10))
    (letrec ()
      (let ([a.1 5] [b.2 1])
        (let ([b.3 (* b.2 a.1)] [a.4 (- a.1 1)])
          (let ([b.5 (* b.3 a.4)] [a.6 (- a.4 1)])
            (let ([b.7 (* b.5 a.6)] [a.8 (- a.6 1)])
              (let ([b.9 (* b.7 a.8)] [a.10 (- a.8 1)])
                (let ([b.11 (* b.9 a.10)])
                  b.11)))))))
    (letrec ()
      (let ([n.1 5])
        (let ([a.2 1])
          (let ([a.3 (* a.2 n.1)])
            (let ([n.4 (- n.1 1)])
              (let ([a.5 (* a.3 n.4)])
                (let ([n.6 (- n.4 1)])
                  (let ([a.7 (* a.5 n.6)])
                    (let ([n.8 (- n.6 1)])
                      (let ([a.9 (* a.7 n.8)])
                        a.9))))))))))
    (letrec ([double$0 (lambda (a.1) (+ a.1 a.1))])
      (double$0 10))
    (letrec ([double$1 (lambda (x.1) (* x.1 2))])
      (begin (double$1 5)))
    (letrec ()
      (let ([x.5 (let ([y.10 10]) (let ([x.15 15]) (* y.10 x.15)))])
        x.5))
    (letrec ([f$0 (lambda (x.1) (+ 1 x.1))]
             [g$1 (lambda (x.2) (- x.2 1))]
             [t$2 (lambda (x.3) (- x.3 1))]
             [j$3 (lambda (x.4) (- x.4 1))]
             [i$4 (lambda (x.5) (- x.5 1))]
             [h$5 (lambda (x.6) (- x.6 1))])
      (let ([x.7 80])
        (let ([a.8 (f$0 x.7)]
              [b.9 (g$1 x.7)]
              [c.10 (h$5 (i$4 (j$3 (t$2 x.7))))])
          (* a.8 (* b.9 (+ c.10 0))))))
    (letrec ([fact$0 (lambda (n.1)
                       (if (= n.1 0)
                           1
                           (let ([t.2 (- n.1 1)])
                             (let ([t.3 (fact$0 t.2)])
                               (* n.1 t.3)))))])
      (fact$0 10))
    (letrec ([fib$0 (lambda (n.1)
                      (if (if (= 0 n.1) (true) (= 1 n.1))
                          1
                          (+ (fib$0 (- n.1 1)) (fib$0 (- n.1 2)))))])
      (fib$0 10))
    (letrec ([even$0 (lambda (n.1)
                       (if (= n.1 0)
                           1
                           (odd$1 (- n.1 1))))]
             [odd$1 (lambda (n.2)
                      (if (= n.2 0)
                          0
                          (even$0 (- n.2 1))))])
      (even$0 17))
    (letrec ()
      (let ([result.3
             (let ([y.2 10])
               (+ (let ([x.1 5]) (if (< 11 x.1) (+ x.1 y.2) (+ y.2 100)))
                  (let ([x.5 10] [y.4 20]) (* x.5 y.4))))])
        result.3))
    (letrec () (let ([x.5 5]) x.5))
    (letrec () (let ([x.5 5] [y.6 6]) (+ x.5 y.6)))
    (letrec () (let ([x.5 5]) (let ([y.6 6]) (+ x.5 y.6))))
    (letrec ([div$0 (lambda (x.2)
                      (let ([x.3 (sra x.2 1)])
                        (div$1 x.3)))]
             [div$1 (lambda (result.4) result.4)])
      (let ([label-temp.5 div$0])
        (label-temp.5 64)))
    ;; Slow division
    (letrec ([expt$0 (lambda (n.3 m.4)
                       (if (= m.4 1)
                           n.3
                           (* n.3 (expt$0 n.3 (- m.4 1)))))]
             [div$1 (lambda (n.5 d.6)
                      (div-helper$2 31 (- (* 2 n.5) d.6) 
                                    (* d.6 (expt$0 2 32)) 0))]
             [div-helper$2 (lambda (i.7 p.8 d.9 q.10)
                             (if (> 0 i.7)
                                 q.10
                                 (if (>= p.8 0)
                                     (div-helper$2 (- i.7 1)
                                                   (- (* 2 p.8) d.9)
                                                   d.9
                                                   (logor (expt$0 2 i.7)
                                                          q.10))
                                     (div-helper$2 (- i.7 1)
                                                   (- (* 2 (+ p.8 d.9)) d.9)
                                                   d.9
                                                   q.10))))])
      (div$1 153 17))
    (letrec ([setbit3$0 (lambda (x.2)
                          (let ([x.3 (logor x.2 8)])
                            (return$1 x.3)))]
             [return$1 (lambda (x.4) x.4)])
      (begin (setbit3$0 1)))
    (letrec ([zero?$0 (lambda (n.2)
                        (let ([x.5 0])
                          (let ([x.6 (- x.5 n.2)])
                            (let ([x.7 (sra x.6 63)])
                              (let ([x.8 (logand x.7 1)])
                                (return$1 x.8))))))]
             [return$1 (lambda (x.4) x.4)])
      (zero?$0 5))
    (letrec ([sqr-double$0 (lambda (z.5)
                             (let ([z.6 (* z.5 z.5)])
                               (double$1 z.6)))]
             [double$1 (lambda (w.4)
                         (let ([w.7 (+ w.4 w.4)])
                           (return$3 w.7)))]
             [return$3 (lambda (result.8) result.8)])
      (begin (sqr-double$0 3) (sqr-double$0 5)))
    ;; test interaction of already assigned frame-vars and
    ;; register allocator
    (letrec ([square$1 (lambda (x.1) (begin (* x.1 x.1)))])
      (square$1 7))
    ;;; Make sure effect ordering is preserved
    (letrec ([sum$1 (lambda (x.1 y.2 z.3 w.4)
                      (+ x.1 (+ y.2 (+ z.3 w.4))))])
      (let ([a.6 (alloc 8)])
        (sum$1 (begin (mset! a.6 0 1) (mref a.6 0))
               (begin (mset! a.6 0 2) (mref a.6 0))
               (begin (mset! a.6 0 3) (mref a.6 0))
               (begin (mset! a.6 0 4) (mref a.6 0)))))
    (letrec ([sum$1 (lambda (x.1 y.2 z.3 w.4)
                      (+ x.1 (+ y.2 (+ z.3 w.4))))])
      (let ([a.6 (alloc 8)])
        (let ([b.7 (begin (mset! a.6 0 1) (mref a.6 0))]
              [c.8 (begin (mset! a.6 0 2) (mref a.6 0))]
              [d.9 (begin (mset! a.6 0 3) (mref a.6 0))]
              [e.10 (begin (mset! a.6 0 4) (mref a.6 0))])
          (sum$1 b.7 c.8 d.9 e.10))))
   ; test nontail calls w/several arguments
    (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                    (if (= n.2 0)
                        (+ (* a.3 (* x.6 x.6)) (+ (* b.4 x.6) c.5))
                        (+ (f$1 (sra n.2 3)
                                (+ a.3 (logand n.2 4))
                                (+ b.4 (logand n.2 2))
                                (+ c.5 (logand n.2 1))
                                x.6)
                           1)))])
      (f$1 16434824 1 0 -1 7))
    (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                    (if (= n.2 0)
                        (+ (* a.3 (* x.6 x.6)) (+ (* b.4 x.6) c.5))
                        (- (f$1 (sra n.2 3)
                                (+ a.3 (logand n.2 4))
                                (+ b.4 (logand n.2 2))
                                (+ c.5 (logand n.2 1))
                                x.6)
                           (g$0 n.2 a.3 b.4 c.5))))]
             [g$0 (lambda (n.7 a.8 b.9 c.10)
                    (+ (- n.7 a.8) (- b.9 c.10)))])
      (f$1 16434824 1 0 -1 7))
    (letrec ([square$0 (lambda (n.1) (* n.1 n.1))])
      (square$0 10))
    (letrec ([fact$0 (lambda (n.2) (fact$1 n.2 1))]
             [fact$1 (lambda (n.3 a.4)
                       (if (= n.3 0)
                           a.4
                           (fact$1 (- n.3 1) (* n.3 a.4))))])
      (fact$0 10))
    (letrec ([gcd$0 (lambda (x.1 y.2)
                      (if (= y.2 0) 
                          x.1 
                          (gcd$0 (if (> x.1 y.2) (- x.1 y.2) x.1)
                                 (if (> x.1 y.2) y.2 (- y.2 x.1)))))])
      (gcd$0 1071 1029))
    (letrec ([sub1$1 (lambda (n.2) (- n.2 1))]
             [fib$0 (lambda (n.3)
                      (if (= 0 n.3)
                          0
                          (if (= 1 n.3)
                              1
                              (+ (fib$0 (sub1$1 n.3))
                                 (fib$0 (sub1$1 (sub1$1 n.3)))))))])
      (fib$0 10))
    (letrec ([ack$0 (lambda (m.1 n.2)
                      (if (= m.1 0)
                          (+ n.2 1)
                          (if (if (> m.1 0) (= n.2 0) (false))
                              (ack$0 (- m.1 1) 1)
                              (let ([tmp.3 (ack$0 m.1 (- n.2 1))])
                                (ack$0 (- m.1 1) tmp.3)))))])
      (ack$0 2 4))
    (letrec ([ack$0 (lambda (m.1 n.2)
                      (if (= m.1 0)
                          (+ n.2 1)
                          (if (if (> m.1 0) (= n.2 0) (false))
                              (ack$0 (- m.1 1) 1)
                              (ack$0 (- m.1 1) (ack$0 m.1 (- n.2 1))))))])
      (ack$0 2 4))
    (letrec ([fib$0 (lambda (n.2) (fib$1 n.2 0 1))]
             [fib$1 (lambda (n.3 a.4 b.5)
                      (if (= n.3 0)
                          a.4
                          (fib$1 (- n.3 1) b.5 (+ b.5 a.4))))])
      (fib$0 5))
    (letrec ([if-test$1 (lambda ()
                          (let ([x.5 5])
                            (* (if (= x.5 5)
                                   (+ x.5 10)
                                   (- x.5 10)) 10)))])
       (if-test$1))
    (letrec ([if-test$1 (lambda ()
                          (let ([x.5 (alloc 16)])
                            (* (if (begin (mset! x.5 8 5) (= (mref x.5 8) 5))
                                   (+ (mref x.5 8) 10)
                                   (- (mref x.5 8) 10)) 10)))])
       (if-test$1))
    (letrec ([if-test$2 (lambda ()
                          (let ([x.5 (alloc 8)])
                            (begin
                              (mset! x.5 0
                                (if (begin
                                      (mset! x.5 0 7)
                                      (if (< (mref x.5 0) 1)
                                          (false)
                                          (< (mref x.5 0) 10)))
                                    (* (mref x.5 0) 2)
                                    (+ (mref x.5 0) 5)))
                              (mref x.5 0))))])
      (if-test$2))
    (letrec ([if-test$3 (lambda (n.1)
                          (begin
                            (if (if (= n.1 0)
                                    (true)
                                    (if (= n.1 1) (true) (= n.1 2)))
                                (* n.1 5)
                                (- n.1 5))))])
       (if-test$3 2))
    (letrec ([if-test$4 (lambda (x.5)
                          (begin
                            (* (if (if (= x.5 10) (false) (true))
                                   (+ x.5 10)
                                   (- x.5 2))
                               10)))])
      (if-test$4 2))
    (letrec ([if-test$5 (lambda (n.1 x.2 y.3)
                          (begin
                            (if (= n.1 0)
                                (mset! x.2 0 (+ (mref x.2 0) (mref y.3 0)))
                                (mset! y.3 0 (+ (mref y.3 0) (mref x.2 0))))
                            (mset! x.2 0 (+ (mref x.2 0) n.1))
                            (if (if (= n.1 (mref y.3 0)) (false) (true))
                                (+ n.1 (mref x.2 0))
                                (+ n.1 (mref y.3 0)))))])
       (let ([q.6 (alloc 8)] [p.7 (alloc 8)])
         (begin
           (mset! q.6 0 1)
           (mset! p.7 0 2)
           (if-test$5 3 q.6 p.7))))
    (letrec ([if-test$6 (lambda (n.0)
                           (let ([n.1 (alloc 8)]
                                 [x.2 (alloc 8)]
                                 [y.3 (alloc 8)])
                             (begin
                               (mset! n.1 0 n.0)
                               (mset! x.2 0 1)
                               (begin
                                 (mset! y.3 0 1)
                                 (if (= (mref n.1 0) 0)
                                     (mset! (mref x.2 0) 0 (+ (mref x.2 0) (mref y.3 0)))
                                     (mset! y.3 0 (+ (mref y.3 0) (mref x.2 0))))
                                 (mset! x.2 0 (+ (mref x.2 0) (mref n.1 0))))
                               (if (if (= (mref n.1 0) (mref y.3 0)) (false) (true))
                                   (mset! n.1 0 (+ (mref n.1 0) (mref x.2 0)))
                                   (mset! n.1 0 (+ (mref n.1 0) (mref y.3 0))))
                               (+ (mref x.2 0) (mref n.1 0)))))])
       (if-test$6 1))
    (letrec ()
       (let ([x.1 0] [y.2 1] [z.3 (alloc 8)])
         (begin
           (if (if (= x.1 0) (= y.2 1) (false))
               (mset! z.3 0 5)
               (begin (mset! z.3 0 5) (mset! z.3 0 (+ (mref z.3 0) (mref z.3 0)))))
           (mref z.3 0))))
    (letrec ([main$0 (lambda (x.1 y.2)
                       (let ([z.3 (if (if (= x.1 1) (true) (= y.2 1))
                                      1
                                      0)])
                         (* z.3 5)))])
      (main$0 1 0))
    (letrec ([main$0 (lambda (a.3 b.4)
                       (let ([a.1 (alloc 8)] [b.2 (alloc 8)])
                         (begin
                           (mset! a.1 0 a.3)
                           (mset! b.2 0 b.4)
                           (if (if (= (mref a.1 0) 1) (= (mref b.2 0) 1) (true))
                               (mset! a.1 0 1)
                               (mset! b.2 0 0))
                           (mset! b.2 0 (* (mref b.2 0) 10))
                           (mset! a.1 0 (+ (mref a.1 0) (mref b.2 0)))
                           (mref a.1 0))))])
      (main$0 0 1))
    (letrec ([main$0 (lambda (a.1 b.2)
                       (if (if (= a.1 1) (= b.2 1) (true)) 1 0))])
      (main$0 1 0))
    (letrec ([main$0 (lambda (a.1 b.2)
                       (if (if (= a.1 1) (= b.2 1) (true)) 1 0))])
      (main$0 0 0))
    (letrec ()
       (let ([a.1 1] [b.2 1])
         (if (if (= a.1 1) (= b.2 1) (true)) 1 0)))
    (letrec ()
      (let ([n.1 (let ([p.7 (alloc 8)]) (begin (mset! p.7 0 1) p.7))])
        (begin
          (let ([a.2 2])
            (begin
              (let ([b.3 3])
                (begin
                  (mset! n.1 0 (+ (mref n.1 0) (if (= (+ (mref n.1 0) b.3) b.3) 5 10)))
                  (mset! n.1 0 (+ (mref n.1 0) b.3))))
              (mset! n.1 0 (+ (mref n.1 0) a.2))))
          (+ (mref n.1 0) (mref n.1 0)))))
    (letrec ()
      (let ([a.1 1] [b.2 2] [c.3 3] [d.4 4] [e.5 5])
        (+ (+ (+ (+ e.5 d.4) c.3) b.2) a.1)))
    (letrec ()
      (let ([a.1 1] [b.2 2] [c.3 3] [d.4 4] [e.5 5] [f.6 6])
        (let ([a.7 (if (> (+ a.1 d.4) f.6)
                       (* a.1 (+ c.3 f.6))
                       (* a.1 (+ b.2 e.5)))])
          a.7)))
    (letrec ([dot$0 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                      (+ (* a.1 b.5) 
                         (+ (* a.2 b.6) 
                            (+ (* a.3 b.7) (* a.4 b.8)))))])
      (dot$0 2 4 6 8 1 3 5 7))
    (letrec ([dot-double-first$51 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                                    (dot$50 (+ a.1 a.1) (+ a.2 a.2)
                                            (+ a.3 a.3) (+ a.4 a.4)
                                            b.5 b.6 b.7 b.8))]
             [dot$50 (lambda (a.11 a.12 a.13 a.14 b.15 b.16 b.17 b.18)
                       (+ (* a.11 b.15) 
                          (+ (* a.12 b.16) 
                             (+ (* a.13 b.17) (* a.14 b.18)))))])
      (dot-double-first$51 2 4 6 8 1 3 5 7))
    (letrec ([dot-double-first$51 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                                    (let ([a.21 (+ a.1 a.1)]
                                          [a.22 (+ a.2 a.2)]
                                          [a.23 (+ a.3 a.3)]
                                          [a.24 (+ a.4 a.4)])
                                      (dot$50 a.21 a.22 a.23 a.24
                                             b.5 b.6 b.7 b.8)))]
             [dot$50 (lambda (a.11 a.12 a.13 a.14 b.15 b.16 b.17 b.18)
                       (+ (* a.11 b.15) 
                          (+ (* a.12 b.16) 
                             (+ (* a.13 b.17) (* a.14 b.18)))))])
      (dot-double-first$51 2 4 6 8 1 3 5 7))
    ;; stress the register allocator, but not so much that it needs to spill.
    (letrec ()
      (let ([a.1 1]
            [b.2 2]
            [c.3 3]
            [d.4 4]
            [e.5 5]
            [f.6 6]
            [g.7 7]
            [h.8 8]
            [i.9 9]
            [j.10 10]
            [k.11 11]
            [l.12 12]
            [m.13 13])
        (let ([a.51 (+ (- (+ a.1 b.2) (+ (- c.3 d.4) e.5)) f.6)])
          (let ([a.52 (+ (- a.51 g.7) (+ h.8 (- i.9 (+ j.10 k.11))))])
            (let ([a.53 (+ a.52 (+ l.12 m.13))])
              (let ([n.14 14]
                    [o.15 15]
                    [p.16 16]
                    [q.17 17]
                    [r.18 18]
                    [s.19 19]
                    [t.20 20]
                    [u.21 21]
                    [v.22 22]
                    [w.23 23]
                    [x.24 24]
                    [y.25 25])
                (let ([a.54 (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ (+ a.53
                                                                n.14)
                                                             o.15)
                                                          p.16)
                                                       q.17)
                                                    r.18)
                                                 s.19)
                                              t.20)
                                           u.21)
                                        v.22)
                                     w.23)
                                  x.24)
                               y.25)])
                  (let ([z.26 26]
                        [b.82 27]
                        [c.83 28]
                        [d.84 29]
                        [e.85 30]
                        [f.86 31]
                        [g.87 32]
                        [h.88 33]
                        [i.89 34]
                        [j.810 35]
                        [k.811 36]
                        [l.812 37])
                    (+ a.54
                       (+ z.26
                          (+ b.82
                             (+ c.83
                                (+ d.84
                                   (+ e.85
                                      (+ f.86
                                         (+ g.87
                                            (+ h.88
                                               (+ i.89
                                                  (+ j.810
                                                     (+ k.811
                                                        l.812))))))))))))))))))))
    ;; another stress test, which should fail unless low-degree
    ;; nodes are chosen properly
     (letrec ()
       (let ([a.1 1] [b.2 2])
         (let ([c.3 a.1] [d.4 4] [e.5 5] [f.6 b.2])
           (let ([f.16 (+ f.6 c.3)])
             (let ([f.26 (+ f.16 d.4)])
               (let ([f.36 (+ f.26 e.5)] [g.7 7])
                 (+ f.36 g.7)))))))
 
   ;; another variant of latter 
    (letrec ()
      (let ([h.8 77] [i.9 88] [j.10 99] [k.11 111] [a.1 1] [b.2 2])
        (let ([c.3 a.1] [d.4 4] [e.5 5] [f.6 b.2])
          (let ([f.16 (+ f.6 c.3)])
            (let ([f.26 (+ f.16 d.4)])
              (let ([f.36 (+ f.26 e.5)] [g.7 7])
                (let ([f.46 (+ f.36 g.7)])
                  (let ([f.56 (+ f.46 i.9)])
                    (let ([f.66 (+ f.56 j.10)])
                      (let ([f.76 (+ f.66 k.11)])
                        (+ f.76 h.8)))))))))))
    (letrec ()
      (let ([a.1 1]
            [b.2 2]
            [c.3 3]
            [d.4 4]
            [e.5 5]
            [f.6 6]
            [g.7 7]
            [h.8 8]
            [i.9 9]
            [j.10 10]
            [k.11 11]
            [l.12 12]
            [m.13 13]
            [n.14 14]
            [o.15 15]
            [p.16 16]
            [q.17 17]
            [r.18 18]
            [s.19 19]
            [t.20 20]
            [u.21 21]
            [v.22 22]
            [w.23 23]
            [x.24 24]
            [y.25 25]
            [z.26 26])
        (let ([a.101 (+ a.1 (+ b.2 (+ c.3 (+ d.4 (+ e.5 (+ f.6 (+ g.7 (+ h.8
                     (+ i.9 (+ j.10 (+ k.11 (+ l.12 (+ m.13 (+ n.14 (+ o.15 
                     (+ p.16 (+ q.17 (+ r.18 (+ s.19 (+ t.20 (+ u.21 (+ v.22 
                     (+ w.23 (+ x.24 (+ y.25 z.26)))))))))))))))))))))))))]
              [b.202 27]
              [c.203 28]
              [d.204 29]
              [e.205 30]
              [f.206 31]
              [g.207 32]
              [h.208 33]
              [i.209 34]
              [j.2010 35]
              [k.2011 36]
              [l.2012 37]
              [m.2013 38]
              [n.2014 39]
              [o.2015 40])
          (let ([a.102 (+ a.101 (+ b.202 (+ c.203 (+ d.204 (+ e.205 (+ f.206 (+ g.207 (+ h.208
                       (+ i.209 (+ j.2010 (+ k.2011 (+ l.2012 (+ m.2013 
                       (+ n.2014 o.2015))))))))))))))])
            (+ a.102 a.1)))))
    ))
