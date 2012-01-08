(define invalid-tests
  '(7
    '#(a b c)
    '5.5
    '#\a
    '"test"
    quote
    (quote)
    (quote '1 '2)
    foo.1
    set! 
    (set! set! '3)
    (set! '1 '2)
    (set! foo.1 '1) 
    (let ((foo.1 '0)) (set! foo.1))
    (let ((foo.1 '0)) (set! foo.1 '1 '2))
    (if '1)
    (if '1 '2 '3 '4)
    (begin)
    (let (foo.1 '3) foo.1)
    (let ([foo.1 '3 '4]) foo.1)
    (let ([foo.1 '3]))
    (letrec (foo.1 (lambda (x.2) x.2)) foo.1)
    (letrec ([foo.1 (lambda (x.2) x.2) (lambda (x.3) x.3)]) foo.1)
    (letrec ([foo.1 (lambda (x.2) x.2)]))
    (lambda)
    (lambda (x.2))
    (lambda (x.2 x.2) x.2)
    (lambda (x.2 '1) x.2)
    (cons '1)
    (foo.1 '1)
    (quote . '3)
    (lambda (x.1) . y.2)
    ((lambda (x.1) x.1) . '3)
    (if (true) '3 '4)
    (if (false) '3 '4)
    (let ([x.1 5] [x.1 10]) (+ x.1 x.1))
    (letrec ([x.1 (lambda () 5)] [x.1 (lambda () 10)]) (+ (x.1) (x.1)))
    (letrec () (let ([x.1 (alloc '8)])
                 (begin
                   (mset! x.1 '0 '10)
                   (mref x.1 '0))))
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

    ;; unbound variables
    (let ([x.1 '5]) (+ x.1 y.2))
    (let ([f.1 (lambda (x.2) (if (= x.2 '0) '1 (* x.2 (f.1 (- x.2 '1)))))])
      (f.1 '10))
  ))

(define tests
  '('7
    '()
    '#f
    '(1 2 3 4) 
    '#5(5 4 3 2 1) 
    '#2((1 2) (3 4))
    '(#2(1 2) #2(3 4))
    '(#3(#t #f 1) #3(#f #t 2))
    (let ([t.496 '10]) (if t.496 t.496 '#f))
    (if '#t (if '45 '7 '#f) '#f)
    (+ '4 '5)
    (- '1 '4)
    (* '7 '9)
    (cons '1 '())
    (car '(1 2))
    (cdr '(1 2))
    (if '#t '1 '2)
    (pair? '(1 2))
    (pair? '())
    (vector? '#2(1 2))
    (vector? '(1 2))
    (boolean? '#f)
    (boolean? '7)
    (null? '())
    (null? '(1 2))
    (fixnum? '1234)
    (fixnum? '())
    (procedure? (lambda (x.495) x.495))
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
    (let ([x.494 '5]) (begin (* '3 x.494) x.494))
    (let ([x.493 '5]) (begin (+ '3 x.493) x.493))
    (let ([x.492 '5]) (begin (- '3 x.492) x.492))
    (let ([x.491 (cons '1 '5)]) (begin (car x.491) x.491))
    (let ([x.490 (cons '1 '5)]) (begin (cdr x.490) x.490))
    (let ([x.489 '1] [y.488 '5])
      (begin (cons x.489 y.488) x.489))
    (begin (make-vector '5) '7)
    (let ([v.487 (make-vector '2)])
      (begin (vector-length v.487) '7))
    (let ([v.486 (make-vector '2)])
      (begin (vector-ref v.486 '0) '7))
    (begin (void) '5)
    ;; value primitives in pred
    (if (+ '3 '5) '7 '8)
    (if (if (* '3 '5) '#f '#t) '7 '8)
    (if (- '3 '5) '7 '8)
    (if (cons '3 '5) '7 '8)
    (if (car (cons '#t '#f)) '7 '8)
    (if (cdr (cons '#t '#f)) '7 '8)
    (if (make-vector '10) '7 '8)
    (let ([v.485 (make-vector '10)])
      (if (vector-length v.485) '7 '8))
    (let ([v.484 (make-vector '10)])
      (begin
        (vector-set! v.484 '0 '#t)
        (if (vector-ref v.484 '0) '7 '8)))
    (if (void) '7 '8)
    ;; pred prims in value
    (< '7 '8)
    (let () (<= '7 '8))
    (= '7 '8)
    (letrec () (>= '7 '8))
    (> '7 '8)
    (let () (boolean? '#f))
    (if '#t '#f '#t)
    (let ([x.483 (cons '1 '())] [y.482 (cons '1 '())])
      (eq? x.483 y.482))
    (fixnum? '7)
    (null? '())
    (letrec () (pair? (cons '1 '())))
    (vector? (make-vector '1))
    (let ([t.478 '5])
      (if t.478
          t.478
          (let ([t.479 '7])
            (if t.479
                t.479
                (let ([t.480 '#f])
                  (if t.480
                    t.480
                    (let ([t.481 '10]) (if t.481 t.481 '11))))))))
    (if '#t (if '#t (if '10 '100 '#f) '#f) '#f)
    ;; pred prims in effect
    (letrec () (begin (< '7 '8) '7))
    (begin (<= '7 '8) '7)
    (letrec () (begin (= '7 '8) '7))
    (begin (>= '7 '8) '7)
    (letrec () (begin (> '7 '8) '8))
    (letrec () (begin (boolean? '#f) '9))
    (letrec ()
      (let ([x.477 (cons '1 '())] [y.476 (cons '1 '())])
        (begin (eq? x.477 y.476) '10)))
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

    (let ([x.475 '(1 2)]) (eq? x.475 x.475))
    (let ([x.474 '(1 2)] [y.473 '(1 2)]) (eq? x.474 y.473))
    (+ (let ([x.472 '7] [y.471 '2])
         (if (if (= x.472 '7) (< y.471 '0) (<= '0 y.471)) '77 '88))
       '99)
    (if (= (+ '7 (* '2 '4))
           (- '20 (+ (+ '1 '1) (+ (+ '1 '1) '1))))
        (+ '1 (+ '1 (+ '1 (+ '1 (+ '1 '10)))))
        '0)
    (let ([v.470 (make-vector '3)])
      (begin
        (vector-set! v.470 '0 '1)
        (vector-set! v.470 '1 '2)
        (vector-set! v.470 '2 '3)
        v.470))
    (cons
      (let ([f.463 (lambda (h.462 v.461) (* h.462 v.461))])
        (let ([k.465 (lambda (x.464) (+ x.464 '5))])
          (letrec ([x.466 '15])
            (letrec ([g.467 (lambda (x.468) (+ '1 x.468))])
              (k.465 (g.467 (let ([g.469 '3]) (f.463 g.469 x.466))))))))
      '())
    (let ([n.455 '4])
      (let ([v.456 (make-vector n.455)])
        (letrec ([iota-fill!.457 (lambda (v.460 i.459 n.458)
                                   (if (< i.459 n.458)
                                       (begin
                                         (vector-set! v.460 i.459 i.459)
                                         (iota-fill!.457
                                           v.460
                                           (+ i.459 '1)
                                           n.458))
                                       (void)))])
          (begin (iota-fill!.457 v.456 '0 n.455) v.456))))
    (let ([x.450 (cons '1 '())])
      (let ([x.451 (cons '2 x.450)])
        (let ([x.452 (cons '3 x.451)])
          (let ([x.453 (cons '4 x.452)])
            (let ([x.454 (cons '5 x.453)]) x.454)))))
    (let ([n.441 '5])
      (let ([a.442 '1])
        (let ([a.443 (* a.442 n.441)])
          (let ([n.444 (- n.441 '1)])
            (let ([a.445 (* a.443 n.444)])
              (let ([n.446 (- n.444 '1)])
                (let ([a.447 (* a.445 n.446)])
                  (let ([n.448 (- n.446 '1)])
                    (let ([a.449 (* a.447 n.448)]) a.449)))))))))
    (let ([n.439 '17] [s.438 '18] [t.437 '19])
      (let ([st.440 (make-vector '5)])
        (begin
          (vector-set! st.440 '0 n.439)
          (vector-set! st.440 '1 s.438)
          (vector-set! st.440 '2 t.437)
          (if (if (vector? st.440) '#f '#t)
              '10000
              (vector-length st.440)))))
    (letrec ([list4.430 (lambda (a.434 b.433 c.432 d.431)
                          (cons
                            a.434
                            (cons b.433 (cons c.432 (cons d.431 '())))))])
      (let ([pair.436 '(1 . 2)] [vect.435 (make-vector '3)])
        (list4.430
          (set-car! pair.436 '7)
          (set-cdr! pair.436 '10)
          (vector-set! vect.435 '0 '16)
          '())))
    (letrec ([f.428 (lambda (p.429)
                      (- (vector-ref
                           (vector-ref
                             (vector-ref
                               (vector-ref (vector-ref p.429 '0) '0)
                               '1)
                             '0)
                           (vector-ref
                             (vector-ref p.429 '1)
                             (vector-ref (vector-ref p.429 '0) '4)))
                         (vector-ref
                           (vector-ref p.429 (vector-ref p.429 '2))
                           (vector-ref
                             (vector-ref p.429 '0)
                             (vector-ref p.429 '4)))))]
             [x.427 (make-vector '6)]
             [y.426 (make-vector '7)])
      (begin
        (vector-set! x.427 '0 y.426)
        (vector-set! x.427 '1 x.427)
        (vector-set! y.426 '0 x.427)
        (vector-set! y.426 '1 '-4421)
        (vector-set! x.427 '2 '0)
        (vector-set! x.427 '3 '-37131)
        (vector-set! x.427 '4 '4)
        (vector-set! x.427 '5 '6)
        (vector-set! y.426 '2 '-55151)
        (vector-set! y.426 '3 '-32000911)
        (vector-set! y.426 '4 '5)
        (vector-set! y.426 '5 '55)
        (vector-set! y.426 '6 '-36)
        (* (f.428 x.427) '2)))
    (let ([vect.414 (make-vector '5)])
      (begin
        (vector-set! vect.414 '0 '123)
        (vector-set! vect.414 '1 '10)
        (vector-set! vect.414 '2 '7)
        (vector-set! vect.414 '3 '12)
        (vector-set! vect.414 '4 '57)
        (letrec ([vector-scale!.419
                   (lambda (vect.421 scale.420)
                     (let ([size.422 (vector-length vect.421)])
                       (letrec ([f.423
                                  (lambda (idx.424)
                                    (if (>= idx.424 '1)
                                        (let ([idx.425 (- idx.424 '1)])
                                          (begin
                                            (vector-set!
                                              vect.421 idx.425
                                              (* (vector-ref vect.421 idx.425)
                                                 scale.420))
                                            (f.423 idx.425)))
                                        (void)))])
                         (f.423 size.422))))])
          (vector-scale!.419 vect.414 '10))
        (letrec ([vector-sum.415
                   (lambda (vect.416)
                     (letrec ([f.417
                                (lambda (idx.418)
                                  (if (< idx.418 '1)
                                      '0
                                      (+ (vector-ref vect.416 (- idx.418 '1))
                                         (f.417 (- idx.418 '1)))))])
                       (f.417 (vector-length vect.416))))])
          (vector-sum.415 vect.414))))
    (letrec ([a.397 (lambda (u.412 v.411 w.410 x.409)
                      (if (= u.412 '0)
                          (b.396 v.411 w.410 x.409)
                          (a.397 (- u.412 '1) v.411 w.410 x.409)))]
             [b.396 (lambda (q.407 r.406 x.405)
                      (let ([p.408 (* q.407 r.406)])
                        (e.394 (* q.407 r.406) p.408 x.405)))]
             [c.395 (lambda (x.404) (* '5 x.404))]
             [e.394 (lambda (n.403 p.402 x.401)
                      (if (= n.403 '0)
                          (c.395 p.402)
                          (o.393 (- n.403 '1) p.402 x.401)))]
             [o.393 (lambda (n.400 p.399 x.398)
                      (if (= '0 n.400)
                          (c.395 x.398)
                          (e.394 (- n.400 '1) p.399 x.398)))])
      (let ([x.413 '5]) (a.397 '3 '2 '1 x.413)))
    ((letrec ([length.391 (lambda (ptr.392)
                            (if (null? ptr.392)
                                '0
                                (+ '1 (length.391 (cdr ptr.392)))))])
       length.391)
     '(5 10 11 5 15))
    (letrec ([count-leaves.389 (lambda (p.390)
                                 (if (pair? p.390)
                                     (+ (count-leaves.389 (car p.390))
                                        (count-leaves.389 (cdr p.390)))
                                     '1))])
      (count-leaves.389
        (cons
          (cons '0 (cons '0 '0))
          (cons
            (cons (cons (cons '0 (cons '0 '0)) '0) '0)
            (cons
              (cons (cons '0 '0) (cons '0 (cons '0 '0)))
              (cons (cons '0 '0) '0))))))
    (letrec ([add1.382 (lambda (n.386) (+ n.386 '1))]
             [map.381 (lambda (f.385 ls.384)
                        (if (null? ls.384)
                            '()
                            (cons
                              (f.385 (car ls.384))
                              (map.381 f.385 (cdr ls.384)))))]
             [sum.380 (lambda (ls.383)
                        (if (null? ls.383)
                            '0
                            (+ (car ls.383) (sum.380 (cdr ls.383)))))])
      (let ([ls.387 '(5 4 3 2 1)])
        (let ([ls.388 (cons '10 (cons '9 (cons '8 (cons '7 (cons '6 ls.387)))))])
          (sum.380 (map.381 add1.382 ls.388)))))
    (letrec ([list-ref.364 (lambda (ls.379 offset.378)
                             (if (= offset.378 '0)
                                 (car ls.379)
                                 (list-ref.364 (cdr ls.379) (- offset.378 '1))))]
             [add.363 (lambda (v.377 w.376) (+ v.377 w.376))]
             [sub.362 (lambda (v.375 w.374) (- v.375 w.374))]
             [mult.361 (lambda (v.373 w.372) (* v.373 w.372))]
             [expt.360 (lambda (v.371 w.370)
                         (if (= w.370 '0)
                             '1
                             (* v.371 (expt.360 v.371 (- w.370 '1)))))]
             [selector.359 (lambda (op*.369 sel.368 rand1.367 rand2.366)
                             (if (null? sel.368)
                                 '0
                                 (cons
                                   ((list-ref.364 op*.369 (car sel.368))
                                    (car rand1.367)
                                    (car rand2.366))
                                   (selector.359
                                     op*.369
                                     (cdr sel.368)
                                     (cdr rand1.367)
                                     (cdr rand2.366)))))]
             [sum.358 (lambda (ls.365)
                        (if (pair? ls.365)
                            (+ (car ls.365) (sum.358 (cdr ls.365)))
                            '0))])
      (sum.358
        (selector.359
          (cons add.363 (cons sub.362 (cons mult.361 (cons expt.360 '()))))
          '(2 0 1 3 2)
          '(5 9 10 2 3)
          '(3 1 3 3 8))))
    (letrec ([thunk-num.351 (lambda (n.357) (lambda () n.357))]
             [force.350 (lambda (th.356) (th.356))]
             [add-ths.349 (lambda (th1.355 th2.354 th3.353 th4.352)
                            (+ (+ (force.350 th1.355) (force.350 th2.354))
                               (+ (force.350 th3.353) (force.350 th4.352))))])
      (add-ths.349
        (thunk-num.351 '5)
        (thunk-num.351 '17)
        (thunk-num.351 '7)
        (thunk-num.351 '9)))
    (letrec ([x.348 '7] [f.347 (lambda () x.348)]) (f.347))
    ((lambda (y.344)
       ((lambda (f.346) (f.346 (f.346 y.344)))
        (lambda (y.345) y.345)))
     '4)
    (let ([double.343 (lambda (a.342) (+ a.342 a.342))])
      (double.343 '10))
    (let ([t.337 '#t] [f.336 '#f])
      (letrec ([even.339 (lambda (x.341)
                           (if (= x.341 '0) t.337 (odd.338 (- x.341 '1))))]
               [odd.338 (lambda (x.340)
                          (if (= x.340 '0) f.336 (even.339 (- x.340 '1))))])
        (odd.338 '13)))
    (letrec ([remq.333 (lambda (x.335 ls.334)
                         (if (null? ls.334)
                             '()
                             (if (eq? (car ls.334) x.335)
                                 (remq.333 x.335 (cdr ls.334))
                                 (cons
                                   (car ls.334)
                                   (remq.333 x.335 (cdr ls.334))))))])
      (remq.333 '3 '(3 1 3)))
    (letrec ([make-param.326 (lambda (val.327)
                               (let ([x.328 val.327])
                                 (letrec ([param.329 (lambda (set.331 val.330)
                                                       (if set.331
                                                           (set! x.328 val.330)
                                                           x.328))])
                                   param.329)))])
      (let ([p.332 (make-param.326 '10)])
        (begin (p.332 '#t '15) (p.332 '#f '#f))))
    (let ([x.323 '0])
      (letrec ([inc.325 (lambda () (set! x.323 (+ x.323 '1)))]
               [dec.324 (lambda () (set! x.323 (- x.323 '1)))])
        (begin
          (inc.325)
          (dec.324)
          (dec.324)
          (inc.325)
          (inc.325)
          (inc.325)
          (dec.324)
          (inc.325)
          x.323)))
    (letrec ([gcd.320 (lambda (x.322 y.321)
                        (if (= y.321 '0)
                            x.322
                            (gcd.320
                              (if (> x.322 y.321) (- x.322 y.321) x.322)
                              (if (> x.322 y.321) y.321 (- y.321 x.322)))))])
      (gcd.320 '1071 '1029))
    (letrec ([sub1.317 (lambda (n.319) (- n.319 '1))]
             [fib.316 (lambda (n.318)
                        (if (= '0 n.318)
                            '0
                            (if (= '1 n.318)
                                '1
                                (+ (fib.316 (sub1.317 n.318))
                                   (fib.316 (sub1.317 (sub1.317 n.318)))))))])
      (fib.316 '10))
    (letrec ([ack.313 (lambda (m.315 n.314)
                        (if (= m.315 '0)
                            (+ n.314 '1)
                            (if (if (> m.315 '0) (= n.314 '0) '#f)
                                (ack.313 (- m.315 '1) '1)
                                (ack.313
                                  (- m.315 '1)
                                  (ack.313 m.315 (- n.314 '1))))))])
      (ack.313 '2 '4))
    (letrec ([fib.307 (lambda (n.308)
                        (letrec ([fib.309 (lambda (n.312 a.311 b.310)
                                            (if (= n.312 '0)
                                                a.311
                                                (fib.309
                                                  (- n.312 '1)
                                                  b.310
                                                  (+ b.310 a.311))))])
                          (fib.309 n.308 '0 '1)))])
      (fib.307 '5))
    ((((((lambda (x.302)
           (lambda (y.303)
             (lambda (z.304)
               (lambda (w.305)
                 (lambda (u.306)
                   (+ x.302 (+ y.303 (+ z.304 (+ w.305 u.306)))))))))
         '5)
        '6)
       '7)
      '8)
     '9)
    (let ([t.294 '#t] [f.293 '#f])
      (let ([bools.297 (cons t.294 f.293)]
            [id.296 (lambda (x.295)
                      (if (if x.295 '#f '#t) f.293 t.294))])
        (letrec ([even.299 (lambda (x.301)
                             (if (= x.301 '0)
                                 (id.296 (car bools.297))
                                 (odd.298 (- x.301 '1))))]
                 [odd.298 (lambda (y.300)
                            (if (= y.300 '0)
                                (id.296 (cdr bools.297))
                                (even.299 (- y.300 '1))))])
          (odd.298 '5))))
    (let ([x.291 '7] [y.290 '4])
      (let ([t.292 (if (fixnum? x.291)
                       (if (= x.291 '4)
                           (if (fixnum? y.290) (= y.290 '7) '#f)
                           '#f)
                       '#f)])
        (if t.292
            t.292
            (if (fixnum? x.291)
                (if (= x.291 '7) (if (fixnum? y.290) (= y.290 '4) '#f) '#f)
                '#f))))
    (let ([y.278 '()] [z.277 '10])
      (let ([test-ls.279 (cons '5 y.278)])
        (begin
          (set! y.278
            (lambda (f.280)
              ((lambda (g.283)
                 (f.280 (lambda (x.284) ((g.283 g.283) x.284))))
               (lambda (g.281)
                 (f.280 (lambda (x.282) ((g.281 g.281) x.282)))))))
          (set! test-ls.279 (cons z.277 test-ls.279))
          (letrec ([length.285 (lambda (ls.286)
                                 (if (null? ls.286)
                                     '0
                                     (+ '1 (length.285 (cdr ls.286)))))])
            (let ([len.287 (length.285 test-ls.279)])
              (eq? (begin
                     (set! length.285
                       (y.278
                         (lambda (len.288)
                           (lambda (ls.289)
                             (if (null? ls.289)
                                 '0
                                 (+ '1 (len.288 (cdr ls.289))))))))
                     (length.285 test-ls.279))
                   len.287))))))
    (letrec ([if-test.271 
               (lambda (n.274 x.273 y.272)
                 (begin
                   (if (= n.274 '0)
                       (vector-set! x.273 '0 
                                    (+ (vector-ref x.273 '0)
                                       (vector-ref y.272 '0)))
                       (vector-set!  y.272 '0
                                     (+ (vector-ref y.272 '0)
                                        (vector-ref x.273 '0))))
                   (vector-set!  x.273 '0 (+ (vector-ref x.273 '0) n.274))
                   (if (if (= n.274 (vector-ref y.272 '0)) '#f '#t)
                       (+ n.274 (vector-ref x.273 '0))
                       (+ n.274 (vector-ref y.272 '0)))))])
      (let ([q.276 (make-vector '1)] [p.275 (make-vector '1)])
        (begin
          (vector-set! q.276 '0 '1)
          (vector-set! p.275 '0 '2)
          (if-test.271 '3 q.276 p.275))))
    (letrec ([if-test.266
               (lambda (n.267)
                 (let ([m.270 (make-vector '1)]
                       [x.269 (make-vector '1)]
                       [y.268 (make-vector '1)])
                   (begin
                     (vector-set! m.270 '0 n.267)
                     (vector-set! x.269 '0 '1)
                     (vector-set! y.268 '0 '1)
                     (if (eq? (vector-ref m.270 '0) '0)
                         (vector-set! (vector-ref x.269 '0) '0
                                      (+ (vector-ref x.269 '0)
                                         (vector-ref y.268 '0)))
                         (vector-set! y.268 '0
                                      (+ (vector-ref y.268 '0)
                                         (vector-ref x.269 '0))))
                     (vector-set! x.269 '0
                                  (+ (vector-ref x.269 '0)
                                     (vector-ref m.270 '0)))
                     (if (if (eq? (vector-ref m.270 '0) (vector-ref y.268 '0))
                             '#f
                             '#t)
                         (vector-set! m.270 '0
                                      (+ (vector-ref m.270 '0)
                                         (vector-ref x.269 '0)))
                         (vector-set! m.270 '0
                                      (+ (vector-ref m.270 '0)
                                         (vector-ref y.268 '0))))
                     (+ (vector-ref x.269 '0)
                        (vector-ref m.270 '0)))))])
      (if-test.266 '1))
    (letrec ([f.255 (lambda (x.261) (+ '1 x.261))]
             [g.254 (lambda (x.260) (- x.260 '1))]
             [t.253 (lambda (x.259) (- x.259 '1))]
             [j.252 (lambda (x.258) (- x.258 '1))]
             [i.251 (lambda (x.257) (- x.257 '1))]
             [h.250 (lambda (x.256) (- x.256 '1))])
      (let ([x.262 '80])
        (let ([a.265 (f.255 x.262)]
              [b.264 (g.254 x.262)]
              [c.263 (h.250 (i.251 (j.252 (t.253 x.262))))])
          (* a.265 (* b.264 (+ c.263 '0))))))
    (let ([f.237 (lambda (x.235) (+ '1 x.235))]
          [g.236 (lambda (x.234) (- x.234 '1))])
      (let ([x.238 '80])
        (let ([a.249 (f.237 x.238)]
              [b.248 (g.236 x.238)]
              [c.247 (letrec ([h.239 (lambda (x.240) (- x.240 '1))])
                       (h.239
                         (letrec ([i.241 (lambda (x.242) (- x.242 '1))])
                           (i.241
                             (letrec ([t.244 (lambda (x.246) (- x.246 '1))]
                                      [j.243 (lambda (x.245) (- x.245 '1))])
                               (j.243 (t.244 x.238)))))))])
          (* a.249 (* b.248 (+ c.247 '0))))))
    (letrec ([fact.230 (lambda (n.231)
                         (if (= n.231 '0)
                             '1
                             (let ([t.232 (- n.231 '1)])
                               (let ([t.233 (fact.230 t.232)])
                                 (* n.231 t.233)))))])
      (fact.230 '10))
    (letrec ([fib.223 (lambda (n.225 k.224)
                        (if (let ([t.228 (= n.225 '0)])
                              (if t.228 t.228 (= n.225 '1)))
                            (k.224 '1)
                            (fib.223
                              (- n.225 '1)
                              (lambda (w.226)
                                (fib.223
                                  (- n.225 '2)
                                  (lambda (v.227)
                                    (k.224 (+ w.226 v.227))))))))])
      (fib.223 '10 (lambda (x.229) x.229)))
    (letrec ()
      (let ([n.220 (let ([p.219 (make-vector '1)])
                     (begin (vector-set! p.219 '0 '1) p.219))])
        (begin
          (let ([a.221 '2])
            (begin
              (let ([b.222 '3])
                (begin
                  (vector-set! n.220 '0
                    (+ (vector-ref n.220 '0)
                       (if (= (+ (vector-ref n.220 '0) b.222) b.222) '5 '10)))
                  (vector-set! n.220 '0 (+ (vector-ref n.220 '0) b.222))))
              (vector-set! n.220 '0 (+ (vector-ref n.220 '0) a.221))))
          (+ (vector-ref n.220 '0) (vector-ref n.220 '0)))))
    (let ([dot-product.218
            (lambda (v1.214 v2.213)
              (if (if (vector? v1.214)
                      (if (vector? v2.213)
                          (= (vector-length v1.214) (vector-length v2.213))
                          '#f)
                      '#f)
                  (letrec ([f.215
                             (lambda (i.216)
                               (if (= i.216 '0)
                                   '1
                                   (let ([i.217 (- i.216 '1)])
                                     (+ (* (vector-ref v1.214 i.217)
                                           (vector-ref v2.213 i.217))
                                        (f.215 i.217)))))])
                    (f.215 (vector-length v1.214)))
                  '#f))])
      (cons
        (dot-product.218 '(1 2) '#2(3 4))
        (cons
          (dot-product.218 '#2(1 2) '#3(3 4 5))
          (cons (dot-product.218 '#4(4 5 6 7) '#4(2 9 8 1)) '()))))
    (letrec ([num-list?.206 (lambda (ls.210)
                              (if (null? ls.210)
                                  '#t
                                  (if (fixnum? (car ls.210))
                                      (num-list?.206 (cdr ls.210))
                                      '#f)))]
             [length.205 (lambda (ls.209)
                           (if (null? ls.209)
                               '0
                               (+ (length.205 (cdr ls.209)) '1)))]
             [dot-prod.204 (lambda (ls1.208 ls2.207)
                             (if (if (null? ls1.208) (null? ls2.207) '#f)
                                 '0
                                 (+ (* (car ls1.208) (car ls2.207))
                                    (dot-prod.204
                                      (cdr ls1.208)
                                      (cdr ls2.207)))))])
      (let ([ls1.212 '(1 2 3 4 5)] [ls2.211 '(5 4 3 2 1)])
        (if (if (if (eq? (num-list?.206 ls1.212) '#f) '#f '#t)
                (if (if (eq? (num-list?.206 ls2.211) '#f) '#f '#t)
                    (= (length.205 ls1.212) (length.205 ls2.211))
                    '#f)
                '#f)
            (dot-prod.204 ls1.212 ls2.211)
            '#f)))
    (letrec ([num-list?.197 (lambda (ls.201)
                              (let ([t.202 (null? ls.201)])
                                (if t.202
                                    t.202
                                    (if (fixnum? (car ls.201))
                                        (num-list?.197 (cdr ls.201))
                                        '#f))))]
             [map.196 (lambda (f.200 ls.199)
                        (if (null? ls.199)
                             '()
                             (cons (f.200 (car ls.199))
                                   (map.196 f.200 (cdr ls.199)))))]
             [square.195 (lambda (n.198) (* n.198 n.198))])
      (let ([ls.203 '(1 2 3 4 5)])
        (begin
          (if (num-list?.197 ls.203)
              (set-car! ls.203 (map.196 square.195 ls.203))
              (void))
          ls.203)))
    (letrec ([num-list?.191 (lambda (ls.193)
                              (if (null? ls.193)
                                  '#t
                                  (if (fixnum? (car ls.193))
                                      (num-list?.191 (cdr ls.193))
                                      '#f)))]
             [list-product.190 (lambda (ls.192)
                                 (if (null? ls.192)
                                     '1
                                     (* (car ls.192)
                                        (list-product.190 (cdr ls.192)))))])
      (let ([ls.194 '(1 2 3 4 5)])
        (if (num-list?.191 ls.194) (list-product.190 ls.194) '#f)))
    (letrec ([f.177 (lambda (x.189 y.188)
                      (if x.189
                          (h.175 (+ x.189 y.188))
                          (g.176 (+ x.189 '1) (+ y.188 '1))))]
             [g.176 (lambda (u.180 v.179)
                      (let ([a.182 (+ u.180 v.179)] [b.181 (* u.180 v.179)])
                        (letrec ([e.183 (lambda (d.184)
                                          (let ([p.185 (cons a.182 b.181)])
                                            (letrec ([q.186 (lambda (m.187)
                                                              (if (< m.187 u.180)
                                                                  (f.177 m.187 d.184)
                                                                  (h.175 (car p.185))))])
                                              (q.186 (f.177 a.182 b.181)))))])
                          (e.183 u.180))))]
             [h.175 (lambda (w.178) w.178)])
      (f.177 '4 '5))
    (let ([y.163 '()] [z.162 '10])
      (let ([test-ls.164 (cons '5 y.163)])
        (begin
          (set! y.163
            (lambda (f.165)
              ((lambda (g.168)
                 (f.165 (lambda (x.169) ((g.168 g.168) x.169))))
               (lambda (g.166)
                 (f.165 (lambda (x.167) ((g.166 g.166) x.167)))))))
          (set! test-ls.164 (cons z.162 test-ls.164))
          (letrec ([length.170 (lambda (ls.171)
                                 (if (null? ls.171)
                                     '0
                                     (+ '1 (length.170 (cdr ls.171)))))])
            (let ([len.172 (length.170 test-ls.164)])
              (eq? (begin
                     (set! length.170
                       (y.163
                         (lambda (len.173)
                           (lambda (ls.174)
                             (if (null? ls.174)
                                 '0
                                 (+ '1 (len.173 (cdr ls.174))))))))
                     (length.170 test-ls.164))
                   len.172))))))
    (letrec ([curry-list.155 (lambda (x.158)
                               (lambda (y.159)
                                 (lambda (z.160)
                                   (lambda (w.161)
                                     (cons x.158
                                       (cons y.159
                                         (cons z.160 (cons w.161 '()))))))))]
             [append.154 (lambda (ls1.157 ls2.156)
                           (if (null? ls1.157)
                               ls2.156
                               (cons
                                 (car ls1.157)
                                 (append.154 (cdr ls1.157) ls2.156))))])
      (append.154
        ((((curry-list.155 '1) '2) '3) '4)
        ((((curry-list.155 '5) '6) '7) '8)))
    (letrec ([quotient.130 (lambda (x.132 y.131)
                             (if (< x.132 '0)
                                 (- '0 (quotient.130 (- '0 x.132) y.131))
                                 (if (< y.131 '0)
                                     (- '0 (quotient.130 x.132 (- '0 y.131)))
                                     (letrec ([f.133 (lambda (x.135 a.134)
                                                       (if (< x.135 y.131)
                                                           a.134
                                                           (f.133
                                                             (- x.135 y.131)
                                                             (+ a.134 '1))))])
                                       (f.133 x.132 '0)))))])
      (let ([sub-interval.136 '1])
        (letrec ([sub-and-continue.138 (lambda (n.144 acc.143 k.142)
                                         (k.142
                                           (- n.144 sub-interval.136)
                                           (* n.144 acc.143)))]
                 [strange-fact.137 (lambda (n.140 acc.139)
                                     (if (= n.140 '0)
                                         (lambda (proc.141)
                                           (proc.141 acc.139))
                                         (sub-and-continue.138
                                           n.140
                                           acc.139
                                           strange-fact.137)))])
          (let ([x.148 '20]
                [fact.147 (let ([seed.145 '1])
                            (lambda (n.146)
                              (strange-fact.137 n.146 seed.145)))])
            (let ([x.149 (cons x.148 (if '#f '#f (void)))])
              (letrec ([answer-user.150 (lambda (ans.151)
                                          (quotient.130 ans.151 (car x.149)))])
                (let ([give-fact5-answer.153 (fact.147 '5)]
                      [give-fact6-answer.152 (fact.147 '6)])
                  (begin
                    (set-car! x.149 (give-fact5-answer.153 answer-user.150))
                    (set-car! x.149 (give-fact6-answer.152 answer-user.150))
                    (car x.149)))))))))
    (letrec ([fib.126 (lambda (x.127)
                        (let ([decrx.129 (lambda ()
                                           (lambda (i.128)
                                             (set! x.127 (- x.127 i.128))))])
                          (if (< x.127 '2)
                              '1
                              (+ (begin ((decrx.129) '1) (fib.126 x.127))
                                 (begin
                                   ((decrx.129) '1)
                                   (fib.126 x.127))))))])
      (fib.126 '10))
    ; test use of keywords/primitives as variables
    (let ([quote.125 (lambda (x.120) x.120)]
          [let.124 (lambda (x.119 y.118) (- y.118 x.119))]
          [if.123 (lambda (x.117 y.116 z.115) (cons x.117 z.115))]
          [cons.122 (lambda (x.114 y.113) (cons y.113 x.114))]
          [|+.121| '16])
      (begin
        (set! |+.121| (* '16 '2))
        (cons.122
          (let.124 ((quote.125 (lambda () '0))) |+.121|)
          (if.123 (quote.125 (if '#f '#f '#t)) '720000 '-1))))
    (letrec ([sum-all.106 (lambda (x.112)
                            (if (fixnum? x.112)
                                x.112
                                (if (vector? x.112)
                                  (sum-vector.105 x.112)
                                  (if (pair? x.112)
                                      (sum-pair.104 x.112)
                                      (if (procedure? x.112)
                                          (sum-all.106 (x.112))
                                          '0)))))]
             [sum-vector.105 (lambda (v.108)
                               (letrec ([l.109 (lambda (v.111 i.110)
                                                 (if (= i.110 '0)
                                                     '0
                                                     (sum-all.106
                                                       (vector-ref
                                                         v.111
                                                         (- i.110 '1)))))])
                                 (l.109 v.108 (vector-length v.108))))]
             [sum-pair.104 (lambda (p.107)
                             (+ (sum-all.106 (car p.107))
                                (sum-all.106 (cdr p.107))))])
      (sum-all.106
        (lambda ()
          '#3((7 8 1) #3(81 23 8)
                      #3(#2(#1(12) 56) 18 ((1 2) (3 ((4)) 5)))))))
    (letrec ([div.69 (lambda (d.71 n.70)
                       (letrec ([f.72 (lambda (d.75 n.74 q.73)
                                        (if (> n.74 d.75)
                                            q.73
                                            (f.72
                                              (- d.75 n.74)
                                              n.74
                                              (+ q.73 '1))))])
                         (f.72 d.71 n.70 '0)))])
      (letrec ([alloc.78 (lambda (n.84)
                           (make-vector (div.69 n.84 '8)))]
               [mref.77 (lambda (x.83 y.82)
                          (if (vector? x.83)
                              (vector-ref x.83 (div.69 y.82 '8))
                              (vector-ref y.82 (div.69 x.83 '8))))]
               [mset!.76 (lambda (x.81 y.80 z.79)
                           (begin
                             (if (vector? x.81)
                                 (vector-set! x.81 (div.69 y.80 '8) z.79)
                                 (vector-set! y.80 (div.69 x.81 '8) z.79))
                             (if '#f '#f (void))))])
        (letrec ([stack-push.87 (lambda (self.91 val.90)
                                  (begin
                                    (mset!.76
                                      (mref.77 self.91 '16)
                                      (* (mref.77 self.91 '8) '8)
                                      val.90)
                                    (mset!.76
                                      self.91
                                      '8
                                      (+ (mref.77 self.91 '8) '1))
                                    self.91))]
                 [stack-pop.86 (lambda (self.89)
                                 (begin
                                   (mset!.76
                                     self.89
                                     '8
                                     (- (mref.77 '8 self.89) '1))
                                   (mref.77
                                     (mref.77 self.89 '16)
                                     (* (mref.77 self.89 '8) '8))))]
                 [stack-top.85 (lambda (self.88)
                                 (mref.77
                                   (mref.77 self.88 '16)
                                   (* (- (mref.77 '8 self.88) '1) '8)))])
          (letrec ([stack-new.93 (let ([meths.96 (alloc.78
                                                   (* '3 '8))])
                                   (begin
                                     (mset!.76 meths.96 '0 stack-push.87)
                                     (mset!.76 meths.96 '8 stack-pop.86)
                                     (mset!.76 meths.96 '16 stack-top.85)
                                     (lambda (size.97)
                                       (let ([self.98 (alloc.78 (* '3 '8))])
                                         (begin
                                           (mset!.76 self.98 '0 meths.96)
                                           (mset!.76 self.98 '8 '0)
                                           (mset!.76
                                             self.98
                                             '16
                                             (alloc.78 (* '8 size.97)))
                                           self.98)))))]
                   [invoke.92 (lambda (obj.95 meth-idx.94)
                                (mref.77
                                  (mref.77 obj.95 '0)
                                  (* meth-idx.94 '8)))])
            (let ([s1.99 (stack-new.93 '10)])
              (begin
                ((invoke.92 s1.99 '0) s1.99 '10) ;; push '10
                ((invoke.92 s1.99 '0) s1.99 '20) ;; push '20
                ((invoke.92 s1.99 '0) s1.99 '30) ;; push ... you get the idea
                ((invoke.92 s1.99 '0) s1.99 '40)
                ((invoke.92 s1.99 '0) s1.99 '0)
                ((invoke.92 s1.99 '0) s1.99 '60)
                ((invoke.92 s1.99 '0) s1.99 '70)
                ((invoke.92 s1.99 '0) s1.99 '80)
                ((invoke.92 s1.99 '0) s1.99 '90)
                ((invoke.92 s1.99 '0) s1.99 '100)
                (let ([s2.100 (stack-new.93 '6)])
                  (begin
                    ((invoke.92 s2.100 '0)
                     s2.100
                     ((invoke.92 s1.99 '1) s1.99))  ;; push pop
                    ((invoke.92 s1.99 '1) s1.99)  ;; pop
                    ((invoke.92 s2.100 '0)
                     s2.100
                     ((invoke.92 s1.99 '1) s1.99))
                    ((invoke.92 s1.99 '1) s1.99)  ;; pop
                    ((invoke.92 s2.100 '0)
                     s2.100
                     ((invoke.92 s1.99 '1) s1.99))
                    ((invoke.92 s1.99 '1) s1.99)
                    ((invoke.92 s2.100 '0)
                     s2.100
                     ((invoke.92 s1.99 '1) s1.99))
                    ((invoke.92 s1.99 '1) s1.99)
                    ((invoke.92 s2.100 '0)
                     s2.100
                     ((invoke.92 s1.99 '1) s1.99))
                    ((invoke.92 s2.100 '0)
                     s2.100
                     ((invoke.92 s1.99 '1) s1.99))
                    (let ([x.101 (+ ((invoke.92 s2.100 '1) s2.100)
                                    ((invoke.92 s2.100 '1) s2.100))])
                      (* (+ (let ([x.103 (+ ((invoke.92 s2.100 '2) s2.100)
                                            ((invoke.92 s2.100 '2) s2.100))])
                              (- x.103
                                 (+ ((invoke.92 s2.100 '1) s2.100)
                                    ((invoke.92 s2.100 '1) s2.100))))
                            (let ([x.102 (+ ((invoke.92 s2.100 '2) s2.100)
                                            ((invoke.92 s2.100 '2) s2.100))])
                              (- (+ ((invoke.92 s2.100 '1) s2.100)
                                    ((invoke.92 s2.100 '1) s2.100))
                                 x.102)))
                         x.101))))))))))
    (if (lambda () '1)
        (let ([a.66 '2])
          (if (if ((lambda (x.67)
                     (let ([x.68 (set! a.66 (set! a.66 '1))]) x.68))
                   '1)
                  (if (eq? a.66 (void)) '#t '#f)
                  '#f)
              '778477
              '14629))
        (void))
   ; contributed by Ryan Newton
    (letrec ([dropsearch.32
               (lambda (cell.53 tree.52)
                 (letrec ([create-link.55
                            (lambda (node.59 f.58)
                              (lambda (g.60)
                                (if (if (pair? node.59) '#f '#t)
                                    (f.58 g.60)
                                    (if (eq? node.59 cell.53)
                                        '#f
                                        (f.58 
                                          (create-link.55
                                            (car node.59)
                                            (create-link.55 (cdr node.59)
                                                            g.60)))))))]
                          [loop.54
                            (lambda (link.56)
                              (lambda ()
                                (if link.56
                                    (loop.54 (link.56 (lambda (v.57) v.57)))
                                    '#f)))])
                   (loop.54 (create-link.55 tree.52 (lambda (x.61) x.61)))))]
             [racethunks.31
               (lambda (thunkx.51 thunky.50)
                 (if (if thunkx.51 thunky.50 '#f)
                     (racethunks.31 (thunkx.51) (thunky.50))
                     (if thunky.50 '#t (if thunkx.51 '#f '()))))]
             [higher?.30
               (lambda (x.49 y.48 tree.47)
                 (racethunks.31
                   (dropsearch.32 x.49 tree.47)
                   (dropsearch.32 y.48 tree.47)))]
             [under?.29
               (lambda (x.46 y.45 tree.44)
                 (racethunks.31
                   (dropsearch.32 x.46 y.45)
                   (dropsearch.32 x.46 tree.44)))]
             [explore.28
               (lambda (x.42 y.41 tree.40)
                 (if (if (pair? y.41) '#f '#t)
                     '#t
                     (if (eq? x.42 y.41)
                         '#f  ;This will take out anything that points to itself
                         (let ([result.43 (higher?.30 x.42 y.41 tree.40)])
                           (if (eq? result.43 '#t)
                               (if (explore.28 y.41 (car y.41) tree.40)
                                   (explore.28 y.41 (cdr y.41) tree.40)
                                   '#f)
                               (if (eq? result.43 '#f)
                                   (process-vertical-jump.27 x.42 y.41 tree.40)
                                   (if (eq? result.43 '())
                                       (process-horizontal-jump.26 x.42 y.41 tree.40)
                                       (void))))))))]
             [process-vertical-jump.27
               (lambda (jumpedfrom.39 jumpedto.38 tree.37)
                 (if (under?.29 jumpedfrom.39 jumpedto.38 tree.37)
                     '#f
                     (fullfinite?.25 jumpedto.38)))]
             [process-horizontal-jump.26
               (lambda (jumpedfrom.36 jumpedto.35 tree.34)
                 (fullfinite?.25 jumpedto.35))]
             [fullfinite?.25
               (lambda (pair.33)
                 (if (if (pair? pair.33) '#f '#t)
                     '#t
                     (if (explore.28 pair.33 (car pair.33) pair.33)
                         (explore.28 pair.33 (cdr pair.33) pair.33)
                         '#f)))])
      (cons
        (fullfinite?.25 (cons '1 '2))
        (cons
          (fullfinite?.25
            (let ([x.65 (cons '1 '2)])
              (begin (set-car! x.65 x.65) x.65)))
          (cons
            (fullfinite?.25
              (let ([a.64 (cons '0 '0)]
                    [b.63 (cons '0 '0)]
                    [c.62 (cons '0 '0)])
                (begin
                  (set-car! a.64 b.63)
                  (set-cdr! a.64 c.62)
                  (set-cdr! b.63 c.62)
                  (set-car! b.63 c.62)
                  (set-car! c.62 b.63)
                  (set-cdr! c.62 b.63)
                  a.64)))
            '()))))
    (letrec ([zero?.8 (lambda (x.24) (= x.24 '0))]
             [sub1.7 (lambda (n.23) (- n.23 '1))]
             [assq.6 (lambda (sym.21 al.20)
                       (if (null? al.20)
                           '#f
                           (let ([entry.22 (car al.20)])
                             (if (eq? sym.21 (car entry.22))
                                 (cdr entry.22)
                                 (assq.6 sym.21 (cdr al.20))))))]
             [map.5 (lambda (p.19 ls.18)
                      (if (null? ls.18)
                          '()
                          (cons
                            (p.19 (car ls.18))
                            (map.5 p.19 (cdr ls.18)))))]
             [snoc.4 (lambda (ls.17 sym.16)
                       (if (null? ls.17)
                           (cons sym.16 '())
                           (cons (car ls.17) (snoc.4 (cdr ls.17) sym.16))))]
             [iota.3 (lambda (n.15)
                       (if (zero?.8 n.15)
                           '(0)
                           (snoc.4 (iota.3 (sub1.7 n.15)) n.15)))]
             [fib.2 (lambda (n.14)
                      (if (zero?.8 n.14)
                          '0
                          (if (= n.14 '1)
                            '1
                            (+ (fib.2 (- n.14 '1)) (fib.2 (- n.14 '2))))))]
             [bounded-memoize.1 
               (lambda (p.10 bound.9)
                 (let ([memo.11 '()])
                   (lambda (arg.12)
                     (if (if (< arg.12 bound.9)
                             (assq.6 arg.12 memo.11)
                             '#f)
                         (assq.6 arg.12 memo.11)
                         (let ([ans.13 (p.10 arg.12)])
                           (begin
                             (if (< arg.12 bound.9)
                                 (set! memo.11 (cons (cons arg.12 ans.13) memo.11))
                                 (void))
                             ans.13))))))])
      (begin
        (set! fib.2 (bounded-memoize.1 fib.2 '5))
        (map.5 fib.2 (iota.3 '10))))

    ;; Thiago Rebello
    (let ([x.1 '(1 2 3)])
      (letrec ([y.2 (lambda (a.3 b.4)
                      (if (null? a.3)
                          '()
                          (cons (cons (car a.3) b.4) (y.2 (cdr a.3) b.4))))])
        (let ([z.5 '(4 5 6)])
          (y.2 x.1 z.5))))

    ;; Francis Fernandez
    (let ([x.1 '(1 2 3 4)]
          [y.2 (+ (let ([x1.3 '(5 6)])
                    (begin 
                      (set! x1.3 '4)
                      x1.3))
                  '4)])
      (begin 
        (set! y.2 (+ y.2 '1))
        (cons y.2 x.1)))

    ;; Patrick Jensen
    (let ([x.1 '(0 1)] [y.2 '(2 3 4)] [z.3 '(5)])
      (letrec ([f.4 (cons x.1 y.2)]
               [g.5 (lambda (a.6) (cons a.6 z.3))])
        (g.5 f.4)))

    ;; Chabane Maidi
    (let ([y.3 '12])
      (let ([x.1 (letrec ([x.2 y.3]
                          [f.4 (lambda () x.2)])
                   (cons (f.4) '(1 9)))])
        (cons (cons x.1 x.1) '())))

    ;; Ben Peters
    (if (car (cdr '(#t #f))) '(0 0 0) '(1 1 1))

    ;; Zhou Li
    (let ([x.1 '0])
      (begin
        (let ([x.2 '1])
          (begin 
            (set! x.1 (+ x.1 '1))
            (set! x.2 (+ x.2 x.1))
            x.2))))

    ;; Kewal Karavinkoppa
    (letrec ([depth.12 (lambda (ls.13)
                         (if (null? ls.13)
                             '1
                             (if (pair? (car ls.13))
                                 (let ([l.16 ((lambda (m.14)
                                                (begin
                                                  (set! m.14 (+ m.14 '1))
                                                  m.14))
                                              (depth.12 (car ls.13)))]
                                       [r.15 (depth.12 (cdr ls.13))])
                                   (if (< l.16 r.15) r.15 l.16))
                                 (depth.12 (cdr ls.13)))))])
      (depth.12 '(1 2 (3 (4 (5 (6 7)))))))
    
    ;; Yu-Shan Huang
    (letrec ([xs.5 '5] [xc.6 '6] [xl.7 (lambda () '7)])
      (let ([f.1 (lambda () '#(#(1 (2 . 3) 4)))]
            [f.2 (lambda () '(5 . 6))])
        (begin (set! xc.6 '#(6 6)) xc.6)))
    
    ;; Melanie Dybvig
    (let [(x.1 '(4))
          (y.2 '(1 2 3))
          (v.4 '#3(0))]
      (letrec [(z.3 (cons y.2 x.1))]
        (begin
          (vector-set! v.4 '0 z.3)
          (set! x.1 '(3))
          (vector-set! v.4 '1 z.3)
          (vector-set! v.4 '2 (cons y.2 x.1))
          v.4)))

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
                          (cons (fn.10 (car fnls.11) 
                                       (car ls.12)) 
                                (map.9 fn.10 (cdr fnls.11) (cdr ls.12)))))])
      (let ([ls.6 '(1 2 3 4 5 6)])
        (map.9 (lambda (fn.7 elem.8)
                 (fn.7 elem.8)) (map.1 add.0 ls.6) ls.6)))
    
    ;; Yin Wang
    (letrec ([x.1 (lambda () (begin (set! x.1 '2) '1))])
      (let ([y.2 (x.1)]
            [z.3 x.1])
        (cons y.2 z.3)))
    
    ;; Emily Lyons
    (letrec ([filter.1 (lambda (pred?.2 ls.3)
                         (if (null? ls.3) 
                             '()
                             (if (pred?.2 (car ls.3))
                                 (filter.1 pred?.2 (cdr ls.3))
                                 (cons (car ls.3) 
                                       (filter.1 pred?.2 (cdr ls.3))))))])
      (filter.1 (lambda (x.4) (< x.4 '0)) '(3 -5 91 6 -32 8)))
    
    ;; Nilesh Mahajan
    (letrec ([x.1 (if (= '2 '3) '3 '4)]) x.1)
  ))
