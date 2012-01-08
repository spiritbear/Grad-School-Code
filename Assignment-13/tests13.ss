(define invalid-tests
  `(7
    #(1 2 3 4)
    (begin (nop) '3)
    (begin (void) 3)
    (if (true) '3 '4)
    (if (false) '3 '4)
    (letrec ([p$1 (lambda () '5)])
      (let ([p.1 (make-procedure p$1 0)])
        (p.1)))
    (letrec () (locals () '7))
    (letrec () (let ([x.1 '7]) (+ x.1 y.2)))
    (letrec () 
      (let ([x.1 '5] [box.1 (cons '1 '2)])
        (begin
          (set-cdr! box.1 (let ([y.2 '7]) y.2))
          (+ x.1 y.2))))
    (letrec () (let ([x.1 5]) x.1))
    (letrec () (let ([x.1 '5]) (begin (set! x.1 '7) x.1)))
    (letrec () (let ([x.1 #(1 2 3)]) x.1))
    (letrec () (let ([x.1 '#(1 2 3)]) x.1))
    (letrec () (let ([x.1 (cons '1 '2)]) (set-cdr! x.1 5)))
    (letrec () (let ([x.1 (make-vector '5)]) (vector-set! x.1 0 10)))
    (letrec () 7)
    (letrec () '5.5)
    (letrec () '#\a)
    (letrec () '(a b c))
    (letrec () (let ([x '5]) x))
    (letrec () (if (if (= '0 '8) #t (= '1 '4)) '8 '6))
    (letrec ([vector-3?.0 (lambda (v.1)
                            (if (= (vector-length v.1) 3) '#t '#f))])
      (let ([v.2 (make-vector '3)])
        (begin
          (vector-set! v.2 '0 '5)
          (vector-set! v.2 '1 '10)
          (vecotr-set! v.2 '2 '20)
          (if (vector-3?.0 v.2)
              (+ (+ (vector-ref v.2 '0) (vector-ref v.2 '1))
                 (vector-ref v.2 '2))
              '#f))))
    (letrec ([new-point.0 (lambda (x.1 y.2)
                            (let ([v.3 (make-vector '2)])
                              (begin
                                (vector-set! v.3 '0 x.1)
                                (vector-set! v.3 '1 x.1)
                                v.3)))]
             [new-3d-point.0 (lambda (x.4 y.5 z.6)
                               (let ([v.7 (make-vector '3)])
                                 (begin
                                   (vector-set! v.7 '0 x.4)
                                   (vector-set! v.7 '1 y.5)
                                   (vector-set! v.7 '2 z.6)
                                   v.7)))])
      (let ([pt1.8 (new-point.0 '1 '2)] [pt2.9 (new-3d-point.0 '1 '2 '3)])
        (+ (vector-ref pt1.8 '0) (vector-ref pt2.9 '0))))
    (letrec ([foo$0 (lambda (x.1)
                      (let ([x.1 x.1])
                        (+ x.1 x.1)))])
      (foo$0 '5))
    (letrec ()
      (let ([x.1 '(1 2 3)])
        x.1))
    (letrec ([foo.1 (lambda (x.2) (set! foo.1 (lambda () x.2)))])
      (foo.1 '5)
      (foo.1))
    (letrec () 'a)
    (letrec ([bar (lambda () (let ([a.1 5]) a.1))]) (bar))
    (letrec ([foo$0 (lambda (x.1 y.2) (+ x.1 y.2))]) (bar$1))
    (letrec () '1152921504606846976)
    (letrec () '-1152921504606846977)
    (letrec () (sra '5 '6))
    (letrec () (logand '5 '2))
    (letrec () (logor '7 '8))
    (letrec () x.1)
    (letrec () (let ([x.1 (alloc '8)])
                 (begin
                   (mset! x.1 '0 '10)
                   (mref x.1 '0))))
    (letrec ([f.1 (lambda (x.3) (let ([x.4 x.3]) x.4))]
             [g.2 (lambda (y.4) y.4)])
      (let ([z.5 '17]) (f.1 (g.2 z.5))))
    (letrec ([f.1 (lambda (x.3) x.3)]
             [g.2 (lambda (y.4) y.4)])
      (let ([z.4 '17]) (f.1 (g.2 z.4))))
    (letrec ([f.1 (lambda (x.3) x.3)]
             [g.2 (lambda (y.3) y.3)])
      (f.1 (g.2 '17)))
    (letrec ()
      (let ([x.1 '15])
        (+ x.1 rax)))
    (letrec () 
      (let ([v.1 (make-vector '10)])
        (let ([v.2 (make-vector (vector-set! v.1 '15))])
          v.2)))
    (letrec ()
      (let ([x.1 (cons '1 '2)] [y.2 (cons '3 '4)])
        (if (pair? x.1 y.2) (+ (car x.1) (car y.2)) x.1)))
    ;; wrong number of args -- nullary with argument
    (letrec () (void '1))
    ;; wrong number of args -- 0 for 1
    (letrec () (car))
    (letrec () (cdr))
    (letrec () (make-vector))
    (letrec () (vector-length))
    (letrec () (boolean?))
    (letrec () (fixnum?))
    (letrec () (null?))
    (letrec () (pair?))
    (letrec () (vector?))
    ;; wrong number of args -- 2 for 1
    (letrec () (let ([x.1 (cons '1 '2)]) (car x.1 (cons '3 '4))))
    (letrec () (let ([x.1 (cons '1 '2)] [y.2 (cons '3 '4)])
                 (cdr x.1 y.2)))
    (letrec () (make-vector '5 '6))
    (letrec () (vector-length (make-vector '7) '1))
    (letrec () (boolean? '#t '#f))
    (letrec () (fixnum? '7 '8))
    (letrec () (null? '() '()))
    (letrec () (pair? (cons '1 '2) (cons '3 '4)))
    (letrec () (vector? (make-vector '1) (make-vector '2)))
    ;; wrong number of args -- 1 for 2
    (letrec () (* '1))
    (letrec () (+ '2))
    (letrec () (- '3))
    (letrec () (cons '4))
    (letrec () (vector-ref (make-vector '5)))
    (letrec () (< '6))
    (letrec () (<= '7))
    (letrec () (= '8))
    (letrec () (>= '9))
    (letrec () (> '10))
    (letrec () (eq? '11))
    (letrec () (let ([x.1 (cons (void) (void))])
                 (begin
                   (set-car! x.1)
                   x.1)))
    (letrec () (let ([x.1 (cons (void) (void))])
                 (begin
                   (set-car! x.1)
                   x.1)))
    ;; wrong number of args -- 3 for 2
    (letrec () (* '1 '2 '3))
    (letrec () (+ '2 '3 '4))
    (letrec () (- '3 '5 '6))
    (letrec () (cons '4 '5 '6))
    (letrec () (vector-ref (make-vector '5) '0 '10))
    (letrec () (< '6 '7 '8))
    (letrec () (<= '7 '8 '9))
    (letrec () (= '8 '9 '10))
    (letrec () (>= '9 '10 '11))
    (letrec () (> '10 '11 '12))
    (letrec () (eq? '11 '12 '13))
    (letrec () (let ([x.1 (cons (void) (void))])
                 (begin
                   (set-car! x.1 '0 '1)
                   x.1)))
    (letrec () (let ([x.1 (cons (void) (void))])
                 (begin
                   (set-car! x.1 '2 '3)
                   x.1)))
    ;; wrong number of args -- 2 for 3
    (letrec () (let ([x.1 (make-vector '2)])
                 (begin
                   (vector-set! x.1 '0)
                   x.1)))
    ;; wrong number of args -- 4 for 3
    (letrec () (let ([x.1 (make-vector '2)])
                 (begin
                   (vector-set! x.1 '0 '3 '1)
                   x.1)))
    (let ([f.1 (lambda (ls.2) 
                 (if (null? ls.2)
                     '0
                     (+ (car ls.2) (f.1 (cdr ls.2)))))])
      (f.1 (cons '1 (cons '2 (cons '3 '())))))
    ))

(define tests 
  '('7
    '()
    '#f
    (let ([t.4146 (cons '1 (cons '2 (cons '3 (cons '4 '()))))])
      t.4146)
    (let ([t.4144 (let ([tmp.4145 (make-vector '5)])
                    (begin
                      (vector-set! tmp.4145 '0 '5)
                      (vector-set! tmp.4145 '1 '4)
                      (vector-set! tmp.4145 '2 '3)
                      (vector-set! tmp.4145 '3 '2)
                      (vector-set! tmp.4145 '4 '1)
                      tmp.4145))])
      t.4144)
    (let ([t.4142 (let ([tmp.4143 (make-vector '2)])
                    (begin
                      (vector-set! tmp.4143 '0 (cons '1 (cons '2 '())))
                      (vector-set! tmp.4143 '1 (cons '3 (cons '4 '())))
                      tmp.4143))])
      t.4142)
    (let ([t.4139 (cons
                    (let ([tmp.4141 (make-vector '2)])
                      (begin
                        (vector-set! tmp.4141 '0 '1)
                        (vector-set! tmp.4141 '1 '2)
                        tmp.4141))
                    (cons
                      (let ([tmp.4140 (make-vector '2)])
                        (begin
                          (vector-set! tmp.4140 '0 '3)
                          (vector-set! tmp.4140 '1 '4)
                          tmp.4140))
                      '()))])
      t.4139)
    (let ([t.4136 (cons
                    (let ([tmp.4138 (make-vector '3)])
                      (begin
                        (vector-set! tmp.4138 '0 '#t)
                        (vector-set! tmp.4138 '1 '#f)
                        (vector-set! tmp.4138 '2 '1)
                        tmp.4138))
                    (cons
                      (let ([tmp.4137 (make-vector '3)])
                        (begin
                          (vector-set! tmp.4137 '0 '#f)
                          (vector-set! tmp.4137 '1 '#t)
                          (vector-set! tmp.4137 '2 '2)
                          tmp.4137))
                      '()))])
      t.4136)
    (let ([t.4135 '10]) (if t.4135 t.4135 '#f))
    (if '#t (if '45 '7 '#f) '#f)
    (+ '4 '5)
    (- '1 '4)
    (* '7 '9)
    (cons '1 '())
    (let ([t.4134 (cons '1 (cons '2 '()))]) (car t.4134))
    (let ([t.4133 (cons '1 (cons '2 '()))]) (cdr t.4133))
    (if '#t '1 '2)
    (let ([t.4132 (cons '1 (cons '2 '()))]) (pair? t.4132))
    (pair? '())
    (let ([t.4130 (let ([tmp.4131 (make-vector '2)])
                    (begin
                      (vector-set! tmp.4131 '0 '1)
                      (vector-set! tmp.4131 '1 '2)
                      tmp.4131))])
      (vector? t.4130))
    (let ([t.4129 (cons '1 (cons '2 '()))]) (vector? t.4129))
    (boolean? '#f)
    (boolean? '7)
    (null? '())
    (let ([t.4128 (cons '1 (cons '2 '()))]) (null? t.4128))
    (fixnum? '1234)
    (fixnum? '())
    (procedure? (lambda (x.4127) x.4127))
    (procedure? '7)
    (<= '1 '8)
    (<= '8 '1)
    (<= '1 '1)
    (< '8 '1)
    (< '1 '8)
    (= '1 '1)
    (= '1 '0)
    (>= '8 '1)
    (>= '1 '8)
    (>= '1 '1)
    (> '8 '1)
    (> '1 '8)
    (if '#f '#f '#t)
    (if '10 '#f '#t)
    ;; value primitives in effect context
    (let ([x.4126 '5]) (begin (* '3 x.4126) x.4126))
    (let ([x.4125 '5]) (begin (+ '3 x.4125) x.4125))
    (let ([x.4124 '5]) (begin (- '3 x.4124) x.4124))
    (let ([x.4123 (cons '1 '5)]) (begin (car x.4123) x.4123))
    (let ([x.4122 (cons '1 '5)]) (begin (cdr x.4122) x.4122))
    (let ([x.4121 '1] [y.4120 '5])
      (begin (cons x.4121 y.4120) x.4121))
    (begin (make-vector '5) '7)
    (let ([v.4119 (make-vector '2)])
      (begin (vector-length v.4119) '7))
    (let ([v.4118 (make-vector '2)])
      (begin (vector-ref v.4118 '0) '7))
    (begin (void) '5)
    ;; value primitives in pred
    (if (+ '3 '5) '7 '8)
    (if (if (* '3 '5) '#f '#t) '7 '8)
    (if (- '3 '5) '7 '8)
    (if (cons '3 '5) '7 '8)
    (if (car (cons '#t '#f)) '7 '8)
    (if (cdr (cons '#t '#f)) '7 '8)
    (if (make-vector '10) '7 '8)
    (let ([v.4117 (make-vector '10)])
      (if (vector-length v.4117) '7 '8))
    (let ([v.4116 (make-vector '10)])
      (begin
        (vector-set! v.4116 '0 '#t)
        (if (vector-ref v.4116 '0) '7 '8)))
    (if (void) '7 '8)
    ;; pred prims in value
    (< '7 '8)
    (let () (<= '7 '8))
    (= '7 '8)
    (letrec () (>= '7 '8))
    (> '7 '8)
    (let () (boolean? '#f))
    (if '#t '#f '#t)
    (let ([x.4115 (cons '1 '())] [y.4114 (cons '1 '())])
      (eq? x.4115 y.4114))
    (fixnum? '7)
    (null? '())
    (letrec () (pair? (cons '1 '())))
    (vector? (make-vector '1))
    (let ([t.4110 '5])
      (if t.4110
        t.4110
        (let ([t.4111 '7])
          (if t.4111
            t.4111
            (let ([t.4112 '#f])
              (if t.4112
                t.4112
                (let ([t.4113 '10]) (if t.4113 t.4113 '11))))))))
    (if '#t (if '#t (if '10 '100 '#f) '#f) '#f)
    ;; pred prims in effect
    (letrec () (begin (< '7 '8) '7))
    (begin (<= '7 '8) '7)
    (letrec () (begin (= '7 '8) '7))
    (begin (>= '7 '8) '7)
    (letrec () (begin (> '7 '8) '8))
    (letrec () (begin (boolean? '#f) '9))
    (letrec ()
      (let ([x.4109 (cons '1 '())] [y.4108 (cons '1 '())])
        (begin (eq? x.4109 y.4108) '10)))
    (letrec () (begin (fixnum? '7) '10))
    (let () (begin (null? '()) '15))
    (letrec () (begin (pair? (cons '1 '())) '20))
    (let () (begin (vector? (make-vector '1)) '10))
    ;; effect prims in value
    (letrec () (set-car! (cons '1 '2) '10))
    (let () (set-cdr! (cons '1 '2) '14))
    (vector-set! (make-vector '4) '0 '10)
    ;; effect prims in pred
    (if (set-car! (cons '1 '2) '10) '7 '8)
    (letrec () (if (set-cdr! (cons '1 '2) '14) '9 '10))
    (letrec ()
      (if (vector-set! (make-vector '4) '0 '10) '11 '12))
    (let ([t.4107 (cons '1 (cons '2 '()))])
      (let ([x.4106 t.4107]) (eq? x.4106 x.4106)))
    (let ([t.4105 (cons '1 (cons '2 '()))]
          [t.4104 (cons '1 (cons '2 '()))])
      (let ([x.4103 t.4105] [y.4102 t.4104]) (eq? x.4103 y.4102)))
    (+ (let ([x.4101 '7] [y.4100 '2])
         (if (if (= x.4101 '7) (< y.4100 '0) (<= '0 y.4100))
           '77
           '88))
       '99)
    (if (= (+ '7 (* '2 '4))
           (- '20 (+ (+ '1 '1) (+ (+ '1 '1) '1))))
      (+ '1 (+ '1 (+ '1 (+ '1 (+ '1 '10)))))
      '0)
    (let ([v.4099 (make-vector '3)])
      (begin
        (vector-set! v.4099 '0 '1)
        (vector-set! v.4099 '1 '2)
        (vector-set! v.4099 '2 '3)
        v.4099))
    (cons
      (let ([f.4090 (lambda (h.4089 v.4088) (* h.4089 v.4088))])
        (let ([k.4092 (lambda (x.4091) (+ x.4091 '5))])
          (let ([x.4098 (void)])
            (let ([x.4093 (cons x.4098 (void))])
              (begin
                (let ([x.4097 '15]) (set-car! x.4093 x.4097))
                (letrec ([g.4094 (lambda (x.4095) (+ '1 x.4095))])
                  (k.4092
                    (g.4094
                      (let ([g.4096 '3]) (f.4090 g.4096 (car x.4093)))))))))))
      '())
    (let ([n.4082 '4])
      (let ([v.4083 (make-vector n.4082)])
        (letrec ([iota-fill!.4084 (lambda (v.4087 i.4086 n.4085)
                                    (if (< i.4086 n.4085)
                                      (begin
                                        (vector-set! v.4087 i.4086 i.4086)
                                        (iota-fill!.4084
                                          v.4087
                                          (+ i.4086 '1)
                                          n.4085))
                                      (void)))])
          (begin (iota-fill!.4084 v.4083 '0 n.4082) v.4083))))
    (let ([x.4077 (cons '1 '())])
      (let ([x.4078 (cons '2 x.4077)])
        (let ([x.4079 (cons '3 x.4078)])
          (let ([x.4080 (cons '4 x.4079)])
            (let ([x.4081 (cons '5 x.4080)]) x.4081)))))
    (let ([n.4068 '5])
      (let ([a.4069 '1])
        (let ([a.4070 (* a.4069 n.4068)])
          (let ([n.4071 (- n.4068 '1)])
            (let ([a.4072 (* a.4070 n.4071)])
              (let ([n.4073 (- n.4071 '1)])
                (let ([a.4074 (* a.4072 n.4073)])
                  (let ([n.4075 (- n.4073 '1)])
                    (let ([a.4076 (* a.4074 n.4075)]) a.4076)))))))))
    (let ([n.4066 '17] [s.4065 '18] [t.4064 '19])
      (let ([st.4067 (make-vector '5)])
        (begin
          (vector-set! st.4067 '0 n.4066)
          (vector-set! st.4067 '1 s.4065)
          (vector-set! st.4067 '2 t.4064)
          (if (if (vector? st.4067) '#f '#t)
            '10000
            (vector-length st.4067)))))
    (let ([t.4063 (cons '7 '10)])
      (letrec ([list4.4056 (lambda (a.4060 b.4059 c.4058 d.4057)
                             (cons
                               a.4060
                               (cons
                                 b.4059
                                 (cons c.4058 (cons d.4057 '())))))])
        (let ([pair.4062 t.4063] [vect.4061 (make-vector '3)])
          (list4.4056
            (set-car! pair.4062 '7)
            (set-cdr! pair.4062 '10)
            (vector-set! vect.4061 '0 '16)
            '()))))
    (let ([f.4055 (void)] [x.4054 (void)] [y.4053 (void)])
      (let ([f.4048 (cons f.4055 (void))]
            [x.4047 (cons x.4054 (void))]
            [y.4046 (cons y.4053 (void))])
        (begin
          (let ([f.4052 (lambda (p.4049)
                          (- (vector-ref
                               (vector-ref
                                 (vector-ref
                                   (vector-ref (vector-ref p.4049 '0) '0)
                                   '1)
                                 '0)
                               (vector-ref
                                 (vector-ref p.4049 '1)
                                 (vector-ref (vector-ref p.4049 '0) '4)))
                             (vector-ref
                               (vector-ref p.4049 (vector-ref p.4049 '2))
                               (vector-ref
                                 (vector-ref p.4049 '0)
                                 (vector-ref p.4049 '4)))))]
                [x.4051 (make-vector '6)]
                [y.4050 (make-vector '7)])
            (begin
              (set-car! f.4048 f.4052)
              (set-car! x.4047 x.4051)
              (set-car! y.4046 y.4050)))
          (vector-set! (car x.4047) '0 (car y.4046))
          (vector-set! (car x.4047) '1 (car x.4047))
          (vector-set! (car y.4046) '0 (car x.4047))
          (vector-set! (car y.4046) '1 '-4421)
          (vector-set! (car x.4047) '2 '0)
          (vector-set! (car x.4047) '3 '-37131)
          (vector-set! (car x.4047) '4 '4)
          (vector-set! (car x.4047) '5 '6)
          (vector-set! (car y.4046) '2 '-55151)
          (vector-set! (car y.4046) '3 '-32000911)
          (vector-set! (car y.4046) '4 '5)
          (vector-set! (car y.4046) '5 '55)
          (vector-set! (car y.4046) '6 '-36)
          (* ((car f.4048) (car x.4047)) '2))))

    (let ([t.13 (let ([tmp.14 (make-vector '5)])
                  (begin
                    (vector-set! tmp.14 '0 '123)
                    (vector-set! tmp.14 '1 '10)
                    (vector-set! tmp.14 '2 '7)
                    (vector-set! tmp.14 '3 '12)
                    (vector-set! tmp.14 '4 '57)
                    tmp.14))])
      (let ([vect.1 t.13])
        (begin
          (letrec ([vector-scale!.2 (lambda (vect.4 scale.3)
                                      (let ([size.5 (vector-length vect.4)])
                                        (letrec ([f.6 (lambda (idx.7)
                                                        (if (>= idx.7 '1)
                                                          (let ([idx.8 (- idx.7
                                                                          '1)])
                                                            (begin
                                                              (vector-set!
                                                                vect.4
                                                                idx.8
                                                                (* (vector-ref
                                                                     vect.4
                                                                     idx.8)
                                                                   scale.3))
                                                              (f.6 idx.8)))
                                                          (void)))])
                                          (f.6 size.5))))])
            (vector-scale!.2 vect.1 '10))
          (letrec ([vector-sum.9 (lambda (vect.10)
                                   (letrec ([f.11 (lambda (idx.12)
                                                    (if (< idx.12 '1)
                                                      '0
                                                      (+ (vector-ref
                                                           vect.10
                                                           (- idx.12 '1))
                                                         (f.11
                                                           (- idx.12
                                                              '1)))))])
                                     (f.11 (vector-length vect.10))))])
            (vector-sum.9 vect.1)))))


    (letrec ([a.4029 (lambda (u.4044 v.4043 w.4042 x.4041)
                       (if (= u.4044 '0)
                         (b.4028 v.4043 w.4042 x.4041)
                         (a.4029 (- u.4044 '1) v.4043 w.4042 x.4041)))]
             [b.4028 (lambda (q.4039 r.4038 x.4037)
                       (let ([p.4040 (* q.4039 r.4038)])
                         (e.4026 (* q.4039 r.4038) p.4040 x.4037)))]
             [c.4027 (lambda (x.4036) (* '5 x.4036))]
             [e.4026 (lambda (n.4035 p.4034 x.4033)
                       (if (= n.4035 '0)
                         (c.4027 p.4034)
                         (o.4025 (- n.4035 '1) p.4034 x.4033)))]
             [o.4025 (lambda (n.4032 p.4031 x.4030)
                       (if (= '0 n.4032)
                         (c.4027 x.4030)
                         (e.4026 (- n.4032 '1) p.4031 x.4030)))])
      (let ([x.4045 '5]) (a.4029 '3 '2 '1 x.4045)))
    (let ([t.4024 (cons
                    '5
                    (cons '10 (cons '11 (cons '5 (cons '15 '())))))])
      ((letrec ([length.4022 (lambda (ptr.4023)
                               (if (null? ptr.4023)
                                 '0
                                 (+ '1 (length.4022 (cdr ptr.4023)))))])
         length.4022)
       t.4024))
    (letrec ([count-leaves.4020 (lambda (p.4021)
                                  (if (pair? p.4021)
                                    (+ (count-leaves.4020 (car p.4021))
                                       (count-leaves.4020 (cdr p.4021)))
                                    '1))])
      (count-leaves.4020
        (cons
          (cons '0 (cons '0 '0))
          (cons
            (cons (cons (cons '0 (cons '0 '0)) '0) '0)
            (cons
              (cons (cons '0 '0) (cons '0 (cons '0 '0)))
              (cons (cons '0 '0) '0))))))
    (let ([t.4019 (cons
                    '5
                    (cons '4 (cons '3 (cons '2 (cons '1 '())))))])
      (letrec ([add1.4012 (lambda (n.4016) (+ n.4016 '1))]
               [map.4011 (lambda (f.4015 ls.4014)
                           (if (null? ls.4014)
                             '()
                             (cons
                               (f.4015 (car ls.4014))
                               (map.4011 f.4015 (cdr ls.4014)))))]
               [sum.4010 (lambda (ls.4013)
                           (if (null? ls.4013)
                             '0
                             (+ (car ls.4013) (sum.4010 (cdr ls.4013)))))])
        (let ([ls.4017 t.4019])
          (let ([ls.4018 (cons
                           '10
                           (cons '9 (cons '8 (cons '7 (cons '6 ls.4017)))))])
            (sum.4010 (map.4011 add1.4012 ls.4018))))))
    (let ([t.4009 (cons
                    '2
                    (cons '0 (cons '1 (cons '3 (cons '2 '())))))]
          [t.4008 (cons
                    '5
                    (cons '9 (cons '10 (cons '2 (cons '3 '())))))]
          [t.4007 (cons
                    '3
                    (cons '1 (cons '3 (cons '3 (cons '8 '())))))])
      (letrec ([list-ref.3991 (lambda (ls.4006 offset.4005)
                                (if (= offset.4005 '0)
                                  (car ls.4006)
                                  (list-ref.3991
                                    (cdr ls.4006)
                                    (- offset.4005 '1))))]
               [add.3990 (lambda (v.4004 w.4003) (+ v.4004 w.4003))]
               [sub.3989 (lambda (v.4002 w.4001) (- v.4002 w.4001))]
               [mult.3988 (lambda (v.4000 w.3999) (* v.4000 w.3999))]
               [expt.3987 (lambda (v.3998 w.3997)
                            (if (= w.3997 '0)
                              '1
                              (* v.3998 (expt.3987 v.3998 (- w.3997 '1)))))]
               [selector.3986 (lambda (op*.3996 sel.3995 rand1.3994
                                                rand2.3993)
                                (if (null? sel.3995)
                                  '0
                                  (cons
                                    ((list-ref.3991 op*.3996 (car sel.3995))
                                     (car rand1.3994)
                                     (car rand2.3993))
                                    (selector.3986
                                      op*.3996
                                      (cdr sel.3995)
                                      (cdr rand1.3994)
                                      (cdr rand2.3993)))))]
               [sum.3985 (lambda (ls.3992)
                           (if (pair? ls.3992)
                             (+ (car ls.3992) (sum.3985 (cdr ls.3992)))
                             '0))])
        (sum.3985
          (selector.3986
            (cons
              add.3990
              (cons sub.3989 (cons mult.3988 (cons expt.3987 '()))))
            t.4009
            t.4008
            t.4007))))
    (letrec ([thunk-num.3978 (lambda (n.3984)
                               (lambda () n.3984))]
             [force.3977 (lambda (th.3983) (th.3983))]
             [add-ths.3976 (lambda (th1.3982 th2.3981 th3.3980 th4.3979)
                             (+ (+ (force.3977 th1.3982)
                                   (force.3977 th2.3981))
                                (+ (force.3977 th3.3980)
                                   (force.3977 th4.3979))))])
      (add-ths.3976
        (thunk-num.3978 '5)
        (thunk-num.3978 '17)
        (thunk-num.3978 '7)
        (thunk-num.3978 '9)))
    (let ([x.3975 (void)] [f.3974 (void)])
      (let ([x.3971 (cons x.3975 (void))]
            [f.3970 (cons f.3974 (void))])
        (begin
          (let ([x.3973 '7] [f.3972 (lambda () (car x.3971))])
            (begin (set-car! x.3971 x.3973) (set-car! f.3970 f.3972)))
          ((car f.3970)))))
    ((lambda (y.3967)
       ((lambda (f.3969) (f.3969 (f.3969 y.3967)))
        (lambda (y.3968) y.3968)))
     '4)
    (let ([double.3966 (lambda (a.3965) (+ a.3965 a.3965))])
      (double.3966 '10))
    (let ([t.3960 '#t] [f.3959 '#f])
      (letrec ([even.3962 (lambda (x.3964)
                            (if (= x.3964 '0)
                              t.3960
                              (odd.3961 (- x.3964 '1))))]
               [odd.3961 (lambda (x.3963)
                           (if (= x.3963 '0)
                             f.3959
                             (even.3962 (- x.3963 '1))))])
        (odd.3961 '13)))
    (let ([t.3958 (cons '3 (cons '1 (cons '3 '())))])
      (letrec ([remq.3955 (lambda (x.3957 ls.3956)
                            (if (null? ls.3956)
                              '()
                              (if (eq? (car ls.3956) x.3957)
                                (remq.3955 x.3957 (cdr ls.3956))
                                (cons
                                  (car ls.3956)
                                  (remq.3955 x.3957 (cdr ls.3956))))))])
        (remq.3955 '3 t.3958)))
    (letrec ([make-param.3947 (lambda (val.3948)
                                (let ([x.3954 val.3948])
                                  (let ([x.3949 (cons x.3954 (void))])
                                    (letrec ([param.3950 (lambda (set.3952
                                                                   val.3951)
                                                           (if set.3952
                                                             (set-car!
                                                               x.3949
                                                               val.3951)
                                                             (car x.3949)))])
                                      param.3950))))])
      (let ([p.3953 (make-param.3947 '10)])
        (begin (p.3953 '#t '15) (p.3953 '#f '#f))))
    (let ([x.3946 '0])
      (let ([x.3943 (cons x.3946 (void))])
        (letrec ([inc.3945 (lambda ()
                             (set-car! x.3943 (+ (car x.3943) '1)))]
                 [dec.3944 (lambda ()
                             (set-car! x.3943 (- (car x.3943) '1)))])
          (begin
            (inc.3945)
            (dec.3944)
            (dec.3944)
            (inc.3945)
            (inc.3945)
            (inc.3945)
            (dec.3944)
            (inc.3945)
            (car x.3943)))))
    (letrec ([gcd.3940 (lambda (x.3942 y.3941)
                         (if (= y.3941 '0)
                           x.3942
                           (gcd.3940
                             (if (> x.3942 y.3941) (- x.3942 y.3941) x.3942)
                             (if (> x.3942 y.3941)
                               y.3941
                               (- y.3941 x.3942)))))])
      (gcd.3940 '1071 '1029))
    (letrec ([sub1.3937 (lambda (n.3939) (- n.3939 '1))]
             [fib.3936 (lambda (n.3938)
                         (if (= '0 n.3938)
                           '0
                           (if (= '1 n.3938)
                             '1
                             (+ (fib.3936 (sub1.3937 n.3938))
                                (fib.3936
                                  (sub1.3937 (sub1.3937 n.3938)))))))])
      (fib.3936 '10))
    (letrec ([ack.3933 (lambda (m.3935 n.3934)
                         (if (= m.3935 '0)
                           (+ n.3934 '1)
                           (if (if (> m.3935 '0) (= n.3934 '0) '#f)
                             (ack.3933 (- m.3935 '1) '1)
                             (ack.3933
                               (- m.3935 '1)
                               (ack.3933 m.3935 (- n.3934 '1))))))])
      (ack.3933 '2 '4))
    (letrec ([fib.3927 (lambda (n.3928)
                         (letrec ([fib.3929 (lambda (n.3932 a.3931 b.3930)
                                              (if (= n.3932 '0)
                                                a.3931
                                                (fib.3929
                                                  (- n.3932 '1)
                                                  b.3930
                                                  (+ b.3930 a.3931))))])
                           (fib.3929 n.3928 '0 '1)))])
      (fib.3927 '5))
    ((((((lambda (x.3922)
           (lambda (y.3923)
             (lambda (z.3924)
               (lambda (w.3925)
                 (lambda (u.3926)
                   (+ x.3922 (+ y.3923 (+ z.3924 (+ w.3925 u.3926)))))))))
         '5)
        '6)
       '7)
      '8)
     '9)
    (let ([t.3914 '#t] [f.3913 '#f])
      (let ([bools.3917 (cons t.3914 f.3913)]
            [id.3916 (lambda (x.3915)
                       (if (if x.3915 '#f '#t) f.3913 t.3914))])
        (letrec ([even.3919 (lambda (x.3921)
                              (if (= x.3921 '0)
                                (id.3916 (car bools.3917))
                                (odd.3918 (- x.3921 '1))))]
                 [odd.3918 (lambda (y.3920)
                             (if (= y.3920 '0)
                               (id.3916 (cdr bools.3917))
                               (even.3919 (- y.3920 '1))))])
          (odd.3918 '5))))
    (let ([x.3911 '7] [y.3910 '4])
      (let ([t.3912 (if (fixnum? x.3911)
                      (if (= x.3911 '4)
                        (if (fixnum? y.3910) (= y.3910 '7) '#f)
                        '#f)
                      '#f)])
        (if t.3912
          t.3912
          (if (fixnum? x.3911)
            (if (= x.3911 '7)
              (if (fixnum? y.3910) (= y.3910 '4) '#f)
              '#f)
            '#f))))
    (let ([y.3909 '()] [z.3893 '10])
      (let ([y.3894 (cons y.3909 (void))])
        (let ([test-ls.3908 (cons '5 (car y.3894))])
          (let ([test-ls.3895 (cons test-ls.3908 (void))])
            (begin
              (set-car!
                y.3894
                (lambda (f.3896)
                  ((lambda (g.3899)
                     (f.3896 (lambda (x.3900) ((g.3899 g.3899) x.3900))))
                   (lambda (g.3897)
                     (f.3896 (lambda (x.3898) ((g.3897 g.3897) x.3898)))))))
              (set-car! test-ls.3895 (cons z.3893 (car test-ls.3895)))
              (let ([length.3907 (void)])
                (let ([length.3901 (cons length.3907 (void))])
                  (begin
                    (let ([length.3906 (lambda (ls.3902)
                                         (if (null? ls.3902)
                                           '0
                                           (+ '1
                                              ((car length.3901)
                                               (cdr ls.3902)))))])
                      (set-car! length.3901 length.3906))
                    (let ([len.3903 ((car length.3901) (car test-ls.3895))])
                      (eq? (begin
                             (set-car!
                               length.3901
                               ((car y.3894)
                                (lambda (len.3904)
                                  (lambda (ls.3905)
                                    (if (null? ls.3905)
                                      '0
                                      (+ '1 (len.3904 (cdr ls.3905))))))))
                             ((car length.3901) (car test-ls.3895)))
                           len.3903))))))))))
    (letrec ([if-test.3887 (lambda (n.3890 x.3889 y.3888)
                             (begin
                               (if (= n.3890 '0)
                                 (vector-set!
                                   x.3889
                                   '0
                                   (+ (vector-ref x.3889 '0)
                                      (vector-ref y.3888 '0)))
                                 (vector-set!
                                   y.3888
                                   '0
                                   (+ (vector-ref y.3888 '0)
                                      (vector-ref x.3889 '0))))
                               (vector-set!
                                 x.3889
                                 '0
                                 (+ (vector-ref x.3889 '0) n.3890))
                               (if (if (= n.3890 (vector-ref y.3888 '0))
                                     '#f
                                     '#t)
                                 (+ n.3890 (vector-ref x.3889 '0))
                                 (+ n.3890 (vector-ref y.3888 '0)))))])
      (let ([q.3892 (make-vector '1)] [p.3891 (make-vector '1)])
        (begin
          (vector-set! q.3892 '0 '1)
          (vector-set! p.3891 '0 '2)
          (if-test.3887 '3 q.3892 p.3891))))
    (letrec ([if-test.3882 (lambda (n.3883)
                             (let ([m.3886 (make-vector '1)]
                                   [x.3885 (make-vector '1)]
                                   [y.3884 (make-vector '1)])
                               (begin
                                 (vector-set! m.3886 '0 n.3883)
                                 (vector-set! x.3885 '0 '1)
                                 (vector-set! y.3884 '0 '1)
                                 (if (eq? (vector-ref m.3886 '0) '0)
                                   (vector-set!
                                     (vector-ref x.3885 '0)
                                     '0
                                     (+ (vector-ref x.3885 '0)
                                        (vector-ref y.3884 '0)))
                                   (vector-set!
                                     y.3884
                                     '0
                                     (+ (vector-ref y.3884 '0)
                                        (vector-ref x.3885 '0))))
                                 (vector-set!
                                   x.3885
                                   '0
                                   (+ (vector-ref x.3885 '0)
                                      (vector-ref m.3886 '0)))
                                 (if (if (eq? (vector-ref m.3886 '0)
                                              (vector-ref y.3884 '0))
                                       '#f
                                       '#t)
                                   (vector-set!
                                     m.3886
                                     '0
                                     (+ (vector-ref m.3886 '0)
                                        (vector-ref x.3885 '0)))
                                   (vector-set!
                                     m.3886
                                     '0
                                     (+ (vector-ref m.3886 '0)
                                        (vector-ref y.3884 '0))))
                                 (+ (vector-ref x.3885 '0)
                                    (vector-ref m.3886 '0)))))])
      (if-test.3882 '1))
    (letrec ([f.3871 (lambda (x.3877) (+ '1 x.3877))]
             [g.3870 (lambda (x.3876) (- x.3876 '1))]
             [t.3869 (lambda (x.3875) (- x.3875 '1))]
             [j.3868 (lambda (x.3874) (- x.3874 '1))]
             [i.3867 (lambda (x.3873) (- x.3873 '1))]
             [h.3866 (lambda (x.3872) (- x.3872 '1))])
      (let ([x.3878 '80])
        (let ([a.3881 (f.3871 x.3878)]
              [b.3880 (g.3870 x.3878)]
              [c.3879 (h.3866 (i.3867 (j.3868 (t.3869 x.3878))))])
          (* a.3881 (* b.3880 (+ c.3879 '0))))))
    (let ([f.3853 (lambda (x.3851) (+ '1 x.3851))]
          [g.3852 (lambda (x.3850) (- x.3850 '1))])
      (let ([x.3854 '80])
        (let ([a.3865 (f.3853 x.3854)]
              [b.3864 (g.3852 x.3854)]
              [c.3863 (letrec ([h.3855 (lambda (x.3856) (- x.3856 '1))])
                        (h.3855
                          (letrec ([i.3857 (lambda (x.3858) (- x.3858 '1))])
                            (i.3857
                              (letrec ([t.3860 (lambda (x.3862)
                                                 (- x.3862 '1))]
                                       [j.3859 (lambda (x.3861)
                                                 (- x.3861 '1))])
                                (j.3859 (t.3860 x.3854)))))))])
          (* a.3865 (* b.3864 (+ c.3863 '0))))))
    (letrec ([fact.3846 (lambda (n.3847)
                          (if (= n.3847 '0)
                            '1
                            (let ([t.3848 (- n.3847 '1)])
                              (let ([t.3849 (fact.3846 t.3848)])
                                (* n.3847 t.3849)))))])
      (fact.3846 '10))
    (letrec ([fib.3839 (lambda (n.3841 k.3840)
                         (if (let ([t.3844 (= n.3841 '0)])
                               (if t.3844 t.3844 (= n.3841 '1)))
                           (k.3840 '1)
                           (fib.3839
                             (- n.3841 '1)
                             (lambda (w.3842)
                               (fib.3839
                                 (- n.3841 '2)
                                 (lambda (v.3843)
                                   (k.3840 (+ w.3842 v.3843))))))))])
      (fib.3839 '10 (lambda (x.3845) x.3845)))
    (letrec ()
      (let ([n.3836 (let ([p.3835 (make-vector '1)])
                      (begin (vector-set! p.3835 '0 '1) p.3835))])
        (begin
          (let ([a.3837 '2])
            (begin
              (let ([b.3838 '3])
                (begin
                  (vector-set!
                    n.3836
                    '0
                    (+ (vector-ref n.3836 '0)
                       (if (= (+ (vector-ref n.3836 '0) b.3838) b.3838)
                         '5
                         '10)))
                  (vector-set! n.3836 '0 (+ (vector-ref n.3836 '0) b.3838))))
              (vector-set! n.3836 '0 (+ (vector-ref n.3836 '0) a.3837))))
          (+ (vector-ref n.3836 '0) (vector-ref n.3836 '0)))))
    (let ([t.3834 (cons '1 (cons '2 '()))]
          [t.3832 (let ([tmp.3833 (make-vector '2)])
                    (begin
                      (vector-set! tmp.3833 '0 '3)
                      (vector-set! tmp.3833 '1 '4)
                      tmp.3833))]
          [t.3830 (let ([tmp.3831 (make-vector '2)])
                    (begin
                      (vector-set! tmp.3831 '0 '1)
                      (vector-set! tmp.3831 '1 '2)
                      tmp.3831))]
          [t.3828 (let ([tmp.3829 (make-vector '3)])
                    (begin
                      (vector-set! tmp.3829 '0 '3)
                      (vector-set! tmp.3829 '1 '4)
                      (vector-set! tmp.3829 '2 '5)
                      tmp.3829))]
          [t.3826 (let ([tmp.3827 (make-vector '4)])
                    (begin
                      (vector-set! tmp.3827 '0 '4)
                      (vector-set! tmp.3827 '1 '5)
                      (vector-set! tmp.3827 '2 '6)
                      (vector-set! tmp.3827 '3 '7)
                      tmp.3827))]
          [t.3824 (let ([tmp.3825 (make-vector '4)])
                    (begin
                      (vector-set! tmp.3825 '0 '2)
                      (vector-set! tmp.3825 '1 '9)
                      (vector-set! tmp.3825 '2 '8)
                      (vector-set! tmp.3825 '3 '1)
                      tmp.3825))])
      (let ([dot-product.3823 (lambda (v1.3819 v2.3818)
                                (if (if (vector? v1.3819)
                                      (if (vector? v2.3818)
                                        (= (vector-length v1.3819)
                                           (vector-length v2.3818))
                                        '#f)
                                      '#f)
                                  (letrec ([f.3820 (lambda (i.3821)
                                                     (if (= i.3821 '0)
                                                       '1
                                                       (let ([i.3822 (- i.3821
                                                                        '1)])
                                                         (+ (* (vector-ref
                                                                 v1.3819
                                                                 i.3822)
                                                               (vector-ref
                                                                 v2.3818
                                                                 i.3822))
                                                            (f.3820
                                                              i.3822)))))])
                                    (f.3820 (vector-length v1.3819)))
                                  '#f))])
        (cons
          (dot-product.3823 t.3834 t.3832)
          (cons
            (dot-product.3823 t.3830 t.3828)
            (cons (dot-product.3823 t.3826 t.3824) '())))))
    (let ([t.3817 (cons
                    '1
                    (cons '2 (cons '3 (cons '4 (cons '5 '())))))]
          [t.3816 (cons
                    '5
                    (cons '4 (cons '3 (cons '2 (cons '1 '())))))])
      (letrec ([num-list?.3809 (lambda (ls.3813)
                                 (if (null? ls.3813)
                                   '#t
                                   (if (fixnum? (car ls.3813))
                                     (num-list?.3809 (cdr ls.3813))
                                     '#f)))]
               [length.3808 (lambda (ls.3812)
                              (if (null? ls.3812)
                                '0
                                (+ (length.3808 (cdr ls.3812)) '1)))]
               [dot-prod.3807 (lambda (ls1.3811 ls2.3810)
                                (if (if (null? ls1.3811) (null? ls2.3810) '#f)
                                  '0
                                  (+ (* (car ls1.3811) (car ls2.3810))
                                     (dot-prod.3807
                                       (cdr ls1.3811)
                                       (cdr ls2.3810)))))])
        (let ([ls1.3815 t.3817] [ls2.3814 t.3816])
          (if (if (if (eq? (num-list?.3809 ls1.3815) '#f) '#f '#t)
                (if (if (eq? (num-list?.3809 ls2.3814) '#f) '#f '#t)
                  (= (length.3808 ls1.3815) (length.3808 ls2.3814))
                  '#f)
                '#f)
            (dot-prod.3807 ls1.3815 ls2.3814)
            '#f))))
    (let ([t.3806 (cons
                    (cons '1 (cons '4 (cons '9 (cons '16 (cons '25 '())))))
                    (cons '2 (cons '3 (cons '4 (cons '5 '())))))])
      (letrec ([num-list?.3799 (lambda (ls.3803)
                                 (let ([t.3804 (null? ls.3803)])
                                   (if t.3804
                                     t.3804
                                     (if (fixnum? (car ls.3803))
                                       (num-list?.3799 (cdr ls.3803))
                                       '#f))))]
               [map.3798 (lambda (f.3802 ls.3801)
                           (if (null? ls.3801)
                             '()
                             (cons
                               (f.3802 (car ls.3801))
                               (map.3798 f.3802 (cdr ls.3801)))))]
               [square.3797 (lambda (n.3800) (* n.3800 n.3800))])
        (let ([ls.3805 t.3806])
          (begin
            (if (num-list?.3799 ls.3805)
              (set-car! ls.3805 (map.3798 square.3797 ls.3805))
              (void))
            ls.3805))))
    (let ([t.3796 (cons
                    '1
                    (cons '2 (cons '3 (cons '4 (cons '5 '())))))])
      (letrec ([num-list?.3792 (lambda (ls.3794)
                                 (if (null? ls.3794)
                                   '#t
                                   (if (fixnum? (car ls.3794))
                                     (num-list?.3792 (cdr ls.3794))
                                     '#f)))]
               [list-product.3791 (lambda (ls.3793)
                                    (if (null? ls.3793)
                                      '1
                                      (* (car ls.3793)
                                         (list-product.3791
                                           (cdr ls.3793)))))])
        (let ([ls.3795 t.3796])
          (if (num-list?.3792 ls.3795)
            (list-product.3791 ls.3795)
            '#f))))
    (letrec ([f.3778 (lambda (x.3790 y.3789)
                       (if x.3790
                         (h.3776 (+ x.3790 y.3789))
                         (g.3777 (+ x.3790 '1) (+ y.3789 '1))))]
             [g.3777 (lambda (u.3781 v.3780)
                       (let ([a.3783 (+ u.3781 v.3780)]
                             [b.3782 (* u.3781 v.3780)])
                         (letrec ([e.3784 (lambda (d.3785)
                                            (let ([p.3786 (cons
                                                            a.3783
                                                            b.3782)])
                                              (letrec ([q.3787 (lambda (m.3788)
                                                                 (if (< m.3788
                                                                        u.3781)
                                                                   (f.3778
                                                                     m.3788
                                                                     d.3785)
                                                                   (h.3776
                                                                     (car p.3786))))])
                                                (q.3787
                                                  (f.3778 a.3783 b.3782)))))])
                           (e.3784 u.3781))))]
             [h.3776 (lambda (w.3779) w.3779)])
      (f.3778 '4 '5))
    (let ([y.3775 '()] [z.3759 '10])
      (let ([y.3760 (cons y.3775 (void))])
        (let ([test-ls.3774 (cons '5 (car y.3760))])
          (let ([test-ls.3761 (cons test-ls.3774 (void))])
            (begin
              (set-car!
                y.3760
                (lambda (f.3762)
                  ((lambda (g.3765)
                     (f.3762 (lambda (x.3766) ((g.3765 g.3765) x.3766))))
                   (lambda (g.3763)
                     (f.3762 (lambda (x.3764) ((g.3763 g.3763) x.3764)))))))
              (set-car! test-ls.3761 (cons z.3759 (car test-ls.3761)))
              (let ([length.3773 (void)])
                (let ([length.3767 (cons length.3773 (void))])
                  (begin
                    (let ([length.3772 (lambda (ls.3768)
                                         (if (null? ls.3768)
                                           '0
                                           (+ '1
                                              ((car length.3767)
                                               (cdr ls.3768)))))])
                      (set-car! length.3767 length.3772))
                    (let ([len.3769 ((car length.3767) (car test-ls.3761))])
                      (eq? (begin
                             (set-car!
                               length.3767
                               ((car y.3760)
                                (lambda (len.3770)
                                  (lambda (ls.3771)
                                    (if (null? ls.3771)
                                      '0
                                      (+ '1 (len.3770 (cdr ls.3771))))))))
                             ((car length.3767) (car test-ls.3761)))
                           len.3769))))))))))
    (letrec ([curry-list.3752 (lambda (x.3755)
                                (lambda (y.3756)
                                  (lambda (z.3757)
                                    (lambda (w.3758)
                                      (cons
                                        x.3755
                                        (cons
                                          y.3756
                                          (cons
                                            z.3757
                                            (cons w.3758 '()))))))))]
             [append.3751 (lambda (ls1.3754 ls2.3753)
                            (if (null? ls1.3754)
                              ls2.3753
                              (cons
                                (car ls1.3754)
                                (append.3751 (cdr ls1.3754) ls2.3753))))])
      (append.3751
        ((((curry-list.3752 '1) '2) '3) '4)
        ((((curry-list.3752 '5) '6) '7) '8)))
    (letrec ([quotient.3727 (lambda (x.3729 y.3728)
                              (if (< x.3729 '0)
                                (- '0 (quotient.3727 (- '0 x.3729) y.3728))
                                (if (< y.3728 '0)
                                  (- '0
                                     (quotient.3727 x.3729 (- '0 y.3728)))
                                  (letrec ([f.3730 (lambda (x.3732 a.3731)
                                                     (if (< x.3732 y.3728)
                                                       a.3731
                                                       (f.3730
                                                         (- x.3732
                                                            y.3728)
                                                         (+ a.3731
                                                            '1))))])
                                    (f.3730 x.3729 '0)))))])
      (let ([sub-interval.3733 '1])
        (letrec ([sub-and-continue.3735 (lambda (n.3741 acc.3740
                                                        k.3739)
                                          (k.3739
                                            (- n.3741 sub-interval.3733)
                                            (* n.3741 acc.3740)))]
                 [strange-fact.3734 (lambda (n.3737 acc.3736)
                                      (if (= n.3737 '0)
                                        (lambda (proc.3738)
                                          (proc.3738 acc.3736))
                                        (sub-and-continue.3735
                                          n.3737
                                          acc.3736
                                          strange-fact.3734)))])
          (let ([x.3745 '20]
                [fact.3744 (let ([seed.3742 '1])
                             (lambda (n.3743)
                               (strange-fact.3734 n.3743 seed.3742)))])
            (let ([x.3746 (cons x.3745 (if '#f '#f (void)))])
              (letrec ([answer-user.3747 (lambda (ans.3748)
                                           (quotient.3727
                                             ans.3748
                                             (car x.3746)))])
                (let ([give-fact5-answer.3750 (fact.3744 '5)]
                      [give-fact6-answer.3749 (fact.3744 '6)])
                  (begin
                    (set-car!
                      x.3746
                      (give-fact5-answer.3750 answer-user.3747))
                    (set-car!
                      x.3746
                      (give-fact6-answer.3749 answer-user.3747))
                    (car x.3746)))))))))
    (letrec ([fib.3722 (lambda (x.3726)
                         (let ([x.3723 (cons x.3726 (void))])
                           (let ([decrx.3725 (lambda ()
                                               (lambda (i.3724)
                                                 (set-car!
                                                   x.3723
                                                   (- (car x.3723)
                                                      i.3724))))])
                             (if (< (car x.3723) '2)
                               '1
                               (+ (begin
                                    ((decrx.3725) '1)
                                    (fib.3722 (car x.3723)))
                                  (begin
                                    ((decrx.3725) '1)
                                    (fib.3722 (car x.3723))))))))])
      (fib.3722 '10))
    ; test use of keywords/primitives as variables
    (let ([quote.3720 (lambda (x.3715) x.3715)]
          [let.3719 (lambda (x.3714 y.3713) (- y.3713 x.3714))]
          [if.3718 (lambda (x.3712 y.3711 z.3710)
                     (cons x.3712 z.3710))]
          [cons.3717 (lambda (x.3709 y.3708) (cons y.3708 x.3709))]
          [|+.3721| '16])
      (let ([|+.3716| (cons |+.3721| (void))])
        (begin
          (set-car! |+.3716| (* '16 '2))
          (cons.3717
            (let.3719 ((quote.3720 (lambda () '0))) (car |+.3716|))
            (if.3718 (quote.3720 (if '#f '#f '#t)) '720000 '-1)))))
    (let ([t.3702 (let ([tmp.3707 (make-vector '3)])
                    (begin
                      (vector-set!
                        tmp.3707
                        '0
                        (cons '7 (cons '8 (cons '1 '()))))
                      (vector-set!
                        tmp.3707
                        '1
                        (let ([tmp.3706 (make-vector '3)])
                          (begin
                            (vector-set! tmp.3706 '0 '81)
                            (vector-set! tmp.3706 '1 '23)
                            (vector-set! tmp.3706 '2 '8)
                            tmp.3706)))
                      (vector-set!
                        tmp.3707
                        '2
                        (let ([tmp.3705 (make-vector '3)])
                          (begin
                            (vector-set!
                              tmp.3705
                              '0
                              (let ([tmp.3704 (make-vector '2)])
                                (begin
                                  (vector-set!
                                    tmp.3704
                                    '0
                                    (let ([tmp.3703 (make-vector '1)])
                                      (begin
                                        (vector-set! tmp.3703 '0 '12)
                                        tmp.3703)))
                                  (vector-set! tmp.3704 '1 '56)
                                  tmp.3704)))
                            (vector-set! tmp.3705 '1 '18)
                            (vector-set!
                              tmp.3705
                              '2
                              (cons
                                (cons '1 (cons '2 '()))
                                (cons
                                  (cons
                                    '3
                                    (cons
                                      (cons (cons '4 '()) '())
                                      (cons '5 '())))
                                  '())))
                            tmp.3705)))
                      tmp.3707))])
      (letrec ([sum-all.3695 (lambda (x.3701)
                               (if (fixnum? x.3701)
                                 x.3701
                                 (if (vector? x.3701)
                                   (sum-vector.3694 x.3701)
                                   (if (pair? x.3701)
                                     (sum-pair.3693 x.3701)
                                     (if (procedure? x.3701)
                                       (sum-all.3695 (x.3701))
                                       '0)))))]
               [sum-vector.3694 (lambda (v.3697)
                                  (letrec ([l.3698 (lambda (v.3700 i.3699)
                                                     (if (= i.3699 '0)
                                                       '0
                                                       (sum-all.3695
                                                         (vector-ref
                                                           v.3700
                                                           (- i.3699
                                                              '1)))))])
                                    (l.3698 v.3697 (vector-length v.3697))))]
               [sum-pair.3693 (lambda (p.3696)
                                (+ (sum-all.3695 (car p.3696))
                                   (sum-all.3695 (cdr p.3696))))])
        (sum-all.3695 (lambda () t.3702))))
    (letrec ([div.3654 (lambda (d.3656 n.3655)
                         (letrec ([f.3657 (lambda (d.3660 n.3659 q.3658)
                                            (if (> n.3659 d.3660)
                                              q.3658
                                              (f.3657
                                                (- d.3660 n.3659)
                                                n.3659
                                                (+ q.3658 '1))))])
                           (f.3657 d.3656 n.3655 '0)))])
      (letrec ([alloc.3663 (lambda (n.3669)
                             (make-vector (div.3654 n.3669 '8)))]
               [mref.3662 (lambda (x.3668 y.3667)
                            (if (vector? x.3668)
                              (vector-ref x.3668 (div.3654 y.3667 '8))
                              (vector-ref y.3667 (div.3654 x.3668 '8))))]
               [mset!.3661 (lambda (x.3666 y.3665 z.3664)
                             (begin
                               (if (vector? x.3666)
                                 (vector-set!
                                   x.3666
                                   (div.3654 y.3665 '8)
                                   z.3664)
                                 (vector-set!
                                   y.3665
                                   (div.3654 x.3666 '8)
                                   z.3664))
                               (if '#f '#f (void))))])
        (letrec ([stack-push.3672 (lambda (self.3676 val.3675)
                                    (begin
                                      (mset!.3661
                                        (mref.3662 self.3676 '16)
                                        (* (mref.3662 self.3676 '8) '8)
                                        val.3675)
                                      (mset!.3661
                                        self.3676
                                        '8
                                        (+ (mref.3662 self.3676 '8) '1))
                                      self.3676))]
                 [stack-pop.3671 (lambda (self.3674)
                                   (begin
                                     (mset!.3661
                                       self.3674
                                       '8
                                       (- (mref.3662 '8 self.3674) '1))
                                     (mref.3662
                                       (mref.3662 self.3674 '16)
                                       (* (mref.3662 self.3674 '8) '8))))]
                 [stack-top.3670 (lambda (self.3673)
                                   (mref.3662
                                     (mref.3662 self.3673 '16)
                                     (* (- (mref.3662 '8 self.3673) '1)
                                        '8)))])
          (let ([stack-new.3692 (void)] [invoke.3691 (void)])
            (let ([stack-new.3678 (cons stack-new.3692 (void))]
                  [invoke.3677 (cons invoke.3691 (void))])
              (begin
                (let ([stack-new.3690 (let ([meths.3681 (alloc.3663 (* '3 '8))])
                                        (begin
                                          (mset!.3661 meths.3681 '0 stack-push.3672)
                                          (mset!.3661 meths.3681 '8 stack-pop.3671)
                                          (mset!.3661 meths.3681 '16 stack-top.3670)
                                          (lambda (size.3682)
                                            (let ([self.3683 (alloc.3663
                                                               (* '3 '8))])
                                              (begin
                                                (mset!.3661 self.3683 '0 meths.3681)
                                                (mset!.3661 self.3683 '8 '0)
                                                (mset!.3661 self.3683 '16
                                                  (alloc.3663 (* '8 size.3682)))
                                                self.3683)))))]
                      [invoke.3689 (lambda (obj.3680 meth-idx.3679)
                                     (mref.3662
                                       (mref.3662 obj.3680 '0)
                                       (* meth-idx.3679 '8)))])
                  (begin
                    (set-car! stack-new.3678 stack-new.3690)
                    (set-car! invoke.3677 invoke.3689)))
                (let ([s1.3684 ((car stack-new.3678) '10)])
                  (begin
                    (((car invoke.3677) s1.3684 '0) s1.3684 '10) ;; push '10
                    (((car invoke.3677) s1.3684 '0) s1.3684 '20) ;; push '20
                    (((car invoke.3677) s1.3684 '0) s1.3684 '30) ;; push ... well you get the idea
                    (((car invoke.3677) s1.3684 '0) s1.3684 '40)
                    (((car invoke.3677) s1.3684 '0) s1.3684 '0)
                    (((car invoke.3677) s1.3684 '0) s1.3684 '60)
                    (((car invoke.3677) s1.3684 '0) s1.3684 '70)
                    (((car invoke.3677) s1.3684 '0) s1.3684 '80)
                    (((car invoke.3677) s1.3684 '0) s1.3684 '90)
                    (((car invoke.3677) s1.3684 '0) s1.3684 '100)
                    (let ([s2.3685 ((car stack-new.3678) '6)])
                      (begin
                        (((car invoke.3677) s2.3685 '0)
                         s2.3685
                         (((car invoke.3677) s1.3684 '1) s1.3684)) ;; push pop
                        (((car invoke.3677) s1.3684 '1) s1.3684) ;; pop
                        (((car invoke.3677) s2.3685 '0)
                         s2.3685
                         (((car invoke.3677) s1.3684 '1) s1.3684))
                        (((car invoke.3677) s1.3684 '1) s1.3684) ;; pop
                        (((car invoke.3677) s2.3685 '0)
                         s2.3685
                         (((car invoke.3677) s1.3684 '1) s1.3684))
                        (((car invoke.3677) s1.3684 '1) s1.3684) ;; pop
                        (((car invoke.3677) s2.3685 '0)
                         s2.3685
                         (((car invoke.3677) s1.3684 '1) s1.3684))
                        (((car invoke.3677) s1.3684 '1) s1.3684) ;; pop
                        (((car invoke.3677) s2.3685 '0)
                         s2.3685
                         (((car invoke.3677) s1.3684 '1) s1.3684))
                        (((car invoke.3677) s2.3685 '0)
                         s2.3685
                         (((car invoke.3677) s1.3684 '1) s1.3684))
                        (let ([x.3686 (+ (((car invoke.3677) s2.3685 '1)
                                          s2.3685)
                                         (((car invoke.3677) s2.3685 '1)
                                          s2.3685))])
                          (* (+ (let ([x.3688 (+ (((car invoke.3677)
                                                   s2.3685
                                                   '2)
                                                  s2.3685)
                                                 (((car invoke.3677)
                                                   s2.3685
                                                   '2)
                                                  s2.3685))])
                                  (- x.3688
                                     (+ (((car invoke.3677) s2.3685 '1)
                                         s2.3685)
                                        (((car invoke.3677) s2.3685 '1)
                                         s2.3685))))
                                (let ([x.3687 (+ (((car invoke.3677)
                                                   s2.3685
                                                   '2)
                                                  s2.3685)
                                                 (((car invoke.3677)
                                                   s2.3685
                                                   '2)
                                                  s2.3685))])
                                  (- (+ (((car invoke.3677) s2.3685 '1)
                                         s2.3685)
                                        (((car invoke.3677) s2.3685 '1)
                                         s2.3685))
                                     x.3687)))
                             x.3686))))))))))))
    (if (lambda () '1)
      (let ([a.3653 '2])
        (let ([a.3650 (cons a.3653 (void))])
          (if (if ((lambda (x.3651)
                     (let ([x.3652 (set-car! a.3650 (set-car! a.3650 '1))])
                       x.3652))
                   '1)
                (if (eq? (car a.3650) (void)) '#t '#f)
                '#f)
            '778477
            '14629)))
      (void))
   ; contributed by Ryan Newton
    (letrec ([dropsearch.3616 (lambda (cell.3637 tree.3636)
                                (letrec ([create-link.3639 (lambda (node.3643
                                                                     f.3642)
                                                             (lambda (g.3644)
                                                               (if (if (pair?
                                                                         node.3643)
                                                                     '#f
                                                                     '#t)
                                                                 (f.3642
                                                                   g.3644)
                                                                 (if (eq? node.3643
                                                                          cell.3637)
                                                                   '#f
                                                                   (f.3642
                                                                     (create-link.3639
                                                                       (car node.3643)
                                                                       (create-link.3639
                                                                         (cdr node.3643)
                                                                         g.3644)))))))]
                                         [loop.3638 (lambda (link.3640)
                                                      (lambda ()
                                                        (if link.3640
                                                          (loop.3638
                                                            (link.3640
                                                              (lambda (v.3641)
                                                                v.3641)))
                                                          '#f)))])
                                  (loop.3638
                                    (create-link.3639
                                      tree.3636
                                      (lambda (x.3645) x.3645)))))]
             [racethunks.3615 (lambda (thunkx.3635 thunky.3634)
                                (if (if thunkx.3635 thunky.3634 '#f)
                                  (racethunks.3615
                                    (thunkx.3635)
                                    (thunky.3634))
                                  (if thunky.3634
                                    '#t
                                    (if thunkx.3635 '#f '()))))]
             [higher?.3614 (lambda (x.3633 y.3632 tree.3631)
                             (racethunks.3615
                               (dropsearch.3616 x.3633 tree.3631)
                               (dropsearch.3616 y.3632 tree.3631)))]
             [under?.3613 (lambda (x.3630 y.3629 tree.3628)
                            (racethunks.3615
                              (dropsearch.3616 x.3630 y.3629)
                              (dropsearch.3616 x.3630 tree.3628)))]
             [explore.3612 (lambda (x.3626 y.3625 tree.3624)
                             (if (if (pair? y.3625) '#f '#t)
                               '#t
                               (if (eq? x.3626 y.3625)
                                 '#f
                                 (let ([result.3627 (higher?.3614
                                                      x.3626
                                                      y.3625
                                                      tree.3624)])
                                   (if (eq? result.3627 '#t)
                                     (if (explore.3612
                                           y.3625
                                           (car y.3625)
                                           tree.3624)
                                       (explore.3612
                                         y.3625
                                         (cdr y.3625)
                                         tree.3624)
                                       '#f)
                                     (if (eq? result.3627 '#f)
                                       (process-vertical-jump.3611
                                         x.3626
                                         y.3625
                                         tree.3624)
                                       (if (eq? result.3627 '())
                                         (process-horizontal-jump.3610
                                           x.3626
                                           y.3625
                                           tree.3624)
                                         (void))))))))]
             [process-vertical-jump.3611 (lambda (jumpedfrom.3623
                                                   jumpedto.3622 tree.3621)
                                           (if (under?.3613
                                                 jumpedfrom.3623
                                                 jumpedto.3622
                                                 tree.3621)
                                             '#f
                                             (fullfinite?.3609
                                               jumpedto.3622)))]
             [process-horizontal-jump.3610 (lambda (jumpedfrom.3620
                                                     jumpedto.3619 tree.3618)
                                             (fullfinite?.3609
                                               jumpedto.3619))]
             [fullfinite?.3609 (lambda (pair.3617)
                                 (if (if (pair? pair.3617) '#f '#t)
                                   '#t
                                   (if (explore.3612
                                         pair.3617
                                         (car pair.3617)
                                         pair.3617)
                                     (explore.3612
                                       pair.3617
                                       (cdr pair.3617)
                                       pair.3617)
                                     '#f)))])
      (cons
        (fullfinite?.3609 (cons '1 '2))
        (cons
          (fullfinite?.3609
            (let ([x.3649 (cons '1 '2)])
              (begin (set-car! x.3649 x.3649) x.3649)))
          (cons
            (fullfinite?.3609
              (let ([a.3648 (cons '0 '0)]
                    [b.3647 (cons '0 '0)]
                    [c.3646 (cons '0 '0)])
                (begin
                  (set-car! a.3648 b.3647)
                  (set-cdr! a.3648 c.3646)
                  (set-cdr! b.3647 c.3646)
                  (set-car! b.3647 c.3646)
                  (set-car! c.3646 b.3647)
                  (set-cdr! c.3646 b.3647)
                  a.3648)))
            '()))))
    (let ([t.3591 (cons '0 '())])
      (let ([zero?.3608 (void)]
            [sub1.3607 (void)]
            [assq.3606 (void)]
            [map.3605 (void)]
            [snoc.3604 (void)]
            [iota.3603 (void)]
            [fib.3602 (void)]
            [bounded-memoize.3601 (void)])
        (let ([zero?.3574 (cons zero?.3608 (void))]
              [sub1.3573 (cons sub1.3607 (void))]
              [assq.3572 (cons assq.3606 (void))]
              [map.3571 (cons map.3605 (void))]
              [snoc.3570 (cons snoc.3604 (void))]
              [iota.3569 (cons iota.3603 (void))]
              [fib.3568 (cons fib.3602 (void))]
              [bounded-memoize.3567 (cons bounded-memoize.3601 (void))])
          (begin
            (let ([zero?.3599 (lambda (x.3590) (= x.3590 '0))]
                  [sub1.3598 (lambda (n.3589) (- n.3589 '1))]
                  [assq.3597 (lambda (sym.3587 al.3586)
                               (if (null? al.3586)
                                 '#f
                                 (let ([entry.3588 (car al.3586)])
                                   (if (eq? sym.3587 (car entry.3588))
                                     (cdr entry.3588)
                                     ((car assq.3572)
                                      sym.3587
                                      (cdr al.3586))))))]
                  [map.3596 (lambda (p.3585 ls.3584)
                              (if (null? ls.3584)
                                '()
                                (cons
                                  (p.3585 (car ls.3584))
                                  ((car map.3571) p.3585 (cdr ls.3584)))))]
                  [snoc.3595 (lambda (ls.3583 sym.3582)
                               (if (null? ls.3583)
                                 (cons sym.3582 '())
                                 (cons
                                   (car ls.3583)
                                   ((car snoc.3570)
                                    (cdr ls.3583)
                                    sym.3582))))]
                  [iota.3594 (lambda (n.3581)
                               (if ((car zero?.3574) n.3581)
                                 t.3591
                                 ((car snoc.3570)
                                  ((car iota.3569)
                                   ((car sub1.3573) n.3581))
                                  n.3581)))]
                  [fib.3593 (lambda (n.3580)
                              (if ((car zero?.3574) n.3580)
                                '0
                                (if (= n.3580 '1)
                                  '1
                                  (+ ((car fib.3568) (- n.3580 '1))
                                     ((car fib.3568) (- n.3580 '2))))))]
                  [bounded-memoize.3592 (lambda (p.3576 bound.3575)
                                          (let ([memo.3600 '()])
                                            (let ([memo.3577 (cons
                                                               memo.3600
                                                               (void))])
                                              (lambda (arg.3578)
                                                (if (if (< arg.3578
                                                           bound.3575)
                                                      ((car assq.3572)
                                                       arg.3578
                                                       (car memo.3577))
                                                      '#f)
                                                  ((car assq.3572)
                                                   arg.3578
                                                   (car memo.3577))
                                                  (let ([ans.3579 (p.3576
                                                                    arg.3578)])
                                                    (begin
                                                      (if (< arg.3578
                                                             bound.3575)
                                                        (set-car!
                                                          memo.3577
                                                          (cons
                                                            (cons
                                                              arg.3578
                                                              ans.3579)
                                                            (car memo.3577)))
                                                        (void))
                                                      ans.3579)))))))])
              (begin
                (set-car! zero?.3574 zero?.3599)
                (set-car! sub1.3573 sub1.3598)
                (set-car! assq.3572 assq.3597)
                (set-car! map.3571 map.3596)
                (set-car! snoc.3570 snoc.3595)
                (set-car! iota.3569 iota.3594)
                (set-car! fib.3568 fib.3593)
                (set-car! bounded-memoize.3567 bounded-memoize.3592)))
            (set-car!
              fib.3568
              ((car bounded-memoize.3567) (car fib.3568) '5))
            ((car map.3571) (car fib.3568) ((car iota.3569) '10))))))

    ;; Thiago Rebello
    (let ([a.1 (lambda(x.2)
                 (lambda(y.3)
                   (* x.2 (- y.3 '1))))])
      (let ([b.4 (lambda(w.5)
                   ((a.1 '2) '4))])
        (b.4 '6)))

    ;; Francis Fernandez
    ((lambda (x.1)
       ((lambda (y.2)
          (letrec ([f.0 (lambda (z.3 n.4)
                          (if (<= n.4 '0) z.3 (f.0 (+ z.3 '1) (- n.4 '1))))])
            (f.0 x.1 y.2))) 
        '5))
     '5)

    ;; Brennon York
    ((lambda (r.5) (+ r.5 '10))
     ((lambda (x.1) (+ (let ([y.2 '3]) (- y.2 x.1)) '2))
      (let ([z.3 (lambda (w.4) (+ w.4 '1))]) (z.3 '7))))

    ;; Zhou Li
    (let ([x.1 (lambda (z.3) (+ z.3 '1))]
          [y.2 (lambda (m.4) (+ m.4 '1))])
      (+ (x.1 '2) (y.2 '2)))

    ;; Shiv Indap
    (letrec ([add.0 (lambda (n.2)
                      (lambda (n.3)
                        (+ n.2 n.3)))]
             [map.1 (lambda (fn.4 ls.5)
                      (if (null? ls.5)
                        '()
                        (cons (fn.4 (car ls.5)) (map.1 fn.4 (cdr ls.5)))))]
             [map.9 (lambda (fn.10 fnls.11 ls.12)
                      (if (null? ls.12)
                        '()
                        (cons (fn.10 (car fnls.11) (car ls.12))
                              (map.9 fn.10 (cdr fnls.11) (cdr ls.12)))))])
      (let ([ls.6 (cons '1 (cons '2 (cons '3 '())))])
        (map.9 (lambda (fn.7 elem.8)
                 (fn.7 elem.8)) (map.1 add.0 ls.6) ls.6)))

    ;; Patrick Jensen
    (let ([a.1 '1])
      (letrec ([f.6 (lambda () (lambda (b.2) (+ a.1 b.2)))]
               [f.7 (lambda (c.3)
                      (lambda (d.4 e.5) (+ (+ d.4 ((f.6) c.3)) e.5)))])
        ((f.7 a.1) ((lambda () a.1)) a.1)))
    
    ;; Melanie Dybvig
    (let ([fill.5 (lambda (x.1 v.2)
                    (if (vector? v.2)
                        (let ([length.4 (vector-length v.2)])
                          (if (fixnum? x.1)
                              (if (<= x.1 length.4)
                                  (letrec ([loop.6 (lambda (index.3)
                                                     (if (= index.3 length.4)
                                                         '#t
                                                         (begin
                                                           (vector-set!
                                                             v.2
                                                             index.3
                                                             x.1)
                                                           (loop.6
                                                             (+ index.3
                                                                '1)))))])
                                    (loop.6 '0))
                                  '#f)
                              '#f))
                        '#f))])
      (fill.5 '3 (make-vector '10)))
    
    ;; Ben Peters
    ((lambda (x.1)
       (x.1 ((lambda (y.2) (if y.2 '#f '#t)) '#t)))
      (lambda (z.3) z.3))

    ;; Chabane Maidi
    (begin
      (lambda (x.1)
        (begin
          x.1))
      (begin
        (let ([x.2 '1]
              [x.3 (lambda () '1)])
          (if (eq? x.2 (null? x.3))
            (let ()
              (let ([y.14 (lambda ()
                            (+ x.2 x.2))])
                ((lambda (y.15 y.16) (begin (vector? y.15) (fixnum? y.16))) (y.14) (y.14))))
            (let ([x.4 (let ([x.5 ((lambda (x.6 x.7)
                                     (if (< x.6 x.7)
                                       (+ x.2 x.6)
                                       (+ x.2 x.7))) x.2 '7)])
                         ((lambda (x.8) (<= ((lambda (x.9) x.8) '13) (+ x.2 x.8))) (x.3)))]
                  [y.10 (letrec ([f.11 (lambda (y.12) (+ y.12 (x.3)))])
                          (f.11 '13))])
              (if (< '9 '14)
                (null? ((lambda () '())))
                (procedure? (lambda (y.13) (cons x.4 y.10)))))))))

    ;; Kewal Karavinkoppa
    (letrec ([depth.1 (lambda (ls.2)
                        (if (null? ls.2)
                            '1
                            (if (pair? (car ls.2))
                                (let ([l.4 ((lambda (m.6) (+ m.6 '1))
                                            (depth.1 (car ls.2)))]
                                      [r.5 (depth.1 (cdr ls.2))])
                                  (if (< l.4 r.5) r.5 l.4))
                                (depth.1 (cdr ls.2)))))])
      (depth.1
        (cons
          '1
          (cons
            (cons (cons '2 (cons '3 '())) (cons '3 (cons '4 '())))
            (cons '5 '())))))

    ;; Yin Wang
    ((lambda (y.2)
       ((lambda (f.1) (f.1 (f.1 y.2)))
        (lambda (x.3) (+ x.3 '1))))
     '3)
    
    ;; Emily Lyons
    (letrec ([divide.1 (lambda (pred?.2 ls.3)
                         (if (null? ls.3)
                             '()
                             (divideh.4 pred?.2 ls.3 '() '())))]

             [divideh.4 (lambda (pred?.8 ls.9 left.5 right.6)
                          (if (null? ls.9)
                              (cons left.5 (cons right.6 '()))
                              (if (pred?.8 (car ls.9))

                                  (divideh.4
                                    pred?.8
                                    (cdr ls.9)
                                    (cons (car ls.9) left.5)
                                    right.6)

                                  (divideh.4
                                    pred?.8
                                    (cdr ls.9)
                                    left.5
                                    (cons (car ls.9) right.6)))))])

      (divide.1
        (lambda (x.7) (<= x.7 '5))
        (cons '2 (cons '8 (cons '9 (cons '2 (cons '1 '())))))))

    ;; Lindsey Kuper
    (letrec ([fold.0 (lambda (proc.1 base.2 ls.3)
                       (if (null? ls.3)
                           base.2
                           (proc.1 (car ls.3)
                                   (fold.0 proc.1 base.2 (cdr ls.3)))))])
      (fold.0 (lambda (x.4 y.5) (* x.4 y.5))
              '1
              (cons '1 (cons '2 (cons '3 (cons '4 (cons '5 '())))))))

    ;; Nilesh Mahajan
    (let ([f.1 (lambda () (void))]
          [x.2 '5]
          [f.3 (lambda () '10)])
      (eq? (f.1) x.2))
    ))