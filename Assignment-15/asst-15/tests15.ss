(define invalid-tests
  '(#(a b c)
    5.5
    #\a
    "test"
    quote
    (quote)
    (quote 1 2)
    foo
    set! 
    (set! set! 3)
    (set! 1 2)
    (set! foo 1) 
    (let ((foo 0)) (set! foo))
    (let ((foo 0)) (set! foo 1 2))
    (if 1)
    (if 1 2 3 4)
    (begin)
    (let (foo 3) foo)
    (let ([foo 3 4]) foo)
    (let ([foo 3]))
    (letrec (foo (lambda (x) x)) foo)
    (letrec ([foo (lambda (x) x) (lambda (x) x)]) foo)
    (letrec ([foo (lambda (x) x)]))
    (lambda)
    (lambda (x))
    (lambda (x x) x)
    (lambda (x 1) x)
    (cons 1)
    (foo 1)
    (quote . 3)
    (lambda (x) . y)
    ((lambda (x) x) . 3)
    (if (true) 3 4)
    (if (false) 3 4)
    (let ([x 5] [x 10]) (+ x x))
    (letrec ([x (lambda () 5)] [x (lambda () 10)]) (+ (x) (x)))
    ((lambda (x x) (+ x x)) 5 10)
    (letrec () (let ([x (alloc 8)])
                 (mset! x 0 10)
                 (mref x 0)))
    ;; wrong number of args -- nullary with argument
    (letrec () (void 1))
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
    (letrec () (let ([x (cons 1 2)]) (car x (cons 3 4))))
    (letrec () (let ([x (cons 1 2)] [y (cons 3 4)])
                 (cdr x y)))
    (letrec () (make-vector 5 6))
    (letrec () (vector-length (make-vector 7) 1))
    (letrec () (boolean? #t #f))
    (letrec () (fixnum? 7 8))
    (letrec () (null? '() '()))
    (letrec () (pair? (cons 1 2) (cons 3 4)))
    (letrec () (vector? (make-vector 1) (make-vector 2)))
    ;; wrong number of args -- 1 for 2
    (letrec () (* 1))
    (letrec () (+ 2))
    (letrec () (- 3))
    (letrec () (cons 4))
    (letrec () (vector-ref (make-vector 5)))
    (letrec () (< 6))
    (letrec () (<= 7))
    (letrec () (= 8))
    (letrec () (>= 9))
    (letrec () (> 10))
    (letrec () (eq? 11))
    (letrec () (let ([x (cons (void) (void))])
                 (begin
                   (set-car! x)
                   x)))
    (letrec () (let ([x (cons (void) (void))])
                 (begin
                   (set-car! x)
                   x)))
    ;; wrong number of args -- 3 for 2
    (letrec () (* 1 2 3))
    (letrec () (+ 2 3 4))
    (letrec () (- 3 5 6))
    (letrec () (cons 4 5 6))
    (letrec () (vector-ref (make-vector 5) 0 10))
    (letrec () (< 6 7 8))
    (letrec () (<= 7 8 9))
    (letrec () (= 8 9 10))
    (letrec () (>= 9 10 11))
    (letrec () (> 10 11 12))
    (letrec () (eq? 11 12 13))
    (letrec () (let ([x (cons (void) (void))])
                 (begin
                   (set-car! x 0 1)
                   x)))
    (letrec () (let ([x (cons (void) (void))])
                 (begin
                   (set-car! x 2 3)
                   x)))
    ;; wrong number of args -- 2 for 3
    (letrec () (let ([x (make-vector 2)])
                 (begin
                   (vector-set! x 0)
                   x)))
    ;; wrong number of args -- 4 for 3
    (letrec () (let ([x (make-vector 2)])
                 (begin
                   (vector-set! x 0 3 1)
                   x)))

    ;; unbound variables
    (let ([x 5]) (+ x y))
    (let ([f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))])
      (f 10))
  ))

(define tests
  '(7
    '()
    #f
    '(1 2 3 4)
    '#(5 4 3 2 1)
    '#((1 2) (3 4))
    '(#(1 2) #(3 4))
    '(#(#t #f 1) #(#f #t 2))
    (or 10 #f)
    (and #t 45 7)
    (+ 4 5)
    (- 1 4)
    (* 7 9)
    (cons 1 ())
    (car '(1 2))
    (cdr '(1 2))
    (if #t 1 2)
    (pair? '(1 2))
    (pair? ())
    (vector? '#(1 2))
    (vector? '(1 2))
    (boolean? #f)
    (boolean? 7)
    (null? '())
    (null? '(1 2))
    (fixnum? 1234)
    (fixnum? '())
    (procedure? (lambda (x) x))
    (procedure? 7)
    (<= 1 8)
    (<= 8 1)
    (<= 1 1)
    (< 8 1)
    (< 1 8)
    (= 1 1)
    (= 1 0)
    (>= 8 1)
    (>= 1 8)
    (>= 1 1)
    (> 8 1)
    (> 1 8)
    (not #f)
    (not 10)
    ;; value primitives in effect context
    (let ([x 5]) (* 3 x) x)
    (let ([x 5]) (+ 3 x) x)
    (let ([x 5]) (- 3 x) x)
    (let ([x (cons 1 5)]) (car x) x)
    (let ([x (cons 1 5)]) (cdr x) x)
    (let ([x 1] [y 5]) (cons x y) x)
    (begin (make-vector 5) 7)
    (let ([v (make-vector 2)]) (vector-length v) 7)
    (let ([v (make-vector 2)]) (vector-ref v 0) 7)
    (begin (void) 5)
    ;; value primitives in pred
    (if (+ 3 5) '7 8)
    (if (not (* 3 5)) '7 8)
    (if (- 3 5) '7 8)
    (if (cons 3 5) 7 8)
    (if (car (cons #t #f)) 7 8)
    (if (cdr (cons #t #f)) 7 8)
    (if (make-vector 10) 7 8)
    (let ([v (make-vector 10)]) (if (vector-length v) 7 8))
    (let ([v (make-vector 10)])
      (vector-set! v 0 #t)
      (if (vector-ref v 0) 7 8))
    (if (void) 7 8)
    ;; pred prims in value
    (< 7 8)
    (let () (<= 7 8))
    (= 7 8)
    (letrec () (>= 7 8))
    (> 7 8)
    (let () (boolean? #f))
    (not #t)
    (let ([x (cons 1 ())] [y (cons 1 ())]) (eq? x y))
    (fixnum? 7)
    (null? '())
    (letrec () (pair? (cons 1 ())))
    (vector? (make-vector 1))
    (or 5 7 #f 10 11)
    (and #t #t 10 100)
    ;; pred prims in effect
    (letrec () (begin (< 7 8) 7))
    (begin (<= '7 '8) '7)
    (letrec () (= 7 8) 7)
    (begin (>= 7 8) 7)
    (letrec () (begin (> 7 8) 8))
    (letrec () (boolean? #f) 9)
    (letrec () 
      (let ([x (cons 1 ())] [y (cons 1 ())])
        (begin (eq? x y) 10)))
    (letrec () (begin (fixnum? 7) 10))
    (let () (null? '()) 15)
    (letrec () (pair? (cons 1 '())) 20)
    (let () (begin (vector? (make-vector '1)) '10))
    ;; effect prims in value
    (letrec () (set-car! (cons 1 2) 10))
    (let () (set-cdr! (cons 1 2) 14))
    (vector-set! (make-vector 4) 0 10)
    ;; effect prims in pred
    (if (set-car! (cons 1 2) 10) 7 8)
    (letrec () (if (set-cdr! (cons 1 2) 14) 9 10))
    (letrec () (if (vector-set! (make-vector 4) 0 10) 11 12))

    (let ([x '(1 2)]) (eq? x x))
    (let ([x '(1 2)] [y '(1 2)]) (eq? x y))
    (+ (let ([x 7] [y 2])
         (if (if (= x 7) (< y 0) (<= 0 y)) 77 88))
       99)
    (if (= (+ 7 (* 2 4)) (- 20 (+ (+ 1 1) (+ (+ 1 1) 1))))
        (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 10)))))
        0)
    (let ([v (make-vector 3)])
      (vector-set! v 0 1)
      (vector-set! v 1 2)
      (vector-set! v 2 3)
      v)
    (cons (let ([f (lambda (h v) (* h v))])
            (let ([k (lambda (x) (+ x 5))])
              (letrec ([x 15])
                (letrec ([g (lambda (x) (+ 1 x))])
                  (k (g (let ([g 3]) (f g x))))))))
          '())
    (let ([n 4])
      (let ([v (make-vector n)])
        (letrec ([iota-fill! (lambda (v i n)
                               (if (< i n)
                                   (begin
                                     (vector-set! v i i)
                                     (iota-fill! v (+ i 1) n))))])
          (iota-fill! v 0 n)
          v)))
    (let ([x (cons '1 '())])
      (let ([x (cons '2 x)])
        (let ([x (cons '3 x)])
          (let ([x (cons '4 x)])
            (let ([x (cons '5 x)])
              x)))))
    (let ([n 5])
      (let ([a 1])
        (let ([a (* a n)])
          (let ([n (- n 1)])
            (let ([a (* a n)])
              (let ([n (- n 1)])
                (let ([a (* a n)])
                  (let ([n (- n 1)])
                    (let ([a (* a n)])
                      a)))))))))
    (let ((n 17) (s 18) (t 19))
      (let ((st (make-vector 5)))
        (vector-set! st 0 n)
        (vector-set! st 1 s)
        (vector-set! st 2 t)
        (if (not (vector? st)) 10000 (vector-length st))))
    (letrec ([list4 (lambda (a b c d) (cons a (cons b (cons c (cons d '())))))])
      (let ([pair '(1 . 2)] [vect (make-vector 3)])
        (list4 (set-car! pair 7) (set-cdr! pair 10) (vector-set! vect 0 16) '())))
    (letrec ([f (lambda (p)
                    (- (vector-ref
                         (vector-ref (vector-ref (vector-ref (vector-ref p 0) 0) 1) 0)
                         (vector-ref (vector-ref p 1) (vector-ref (vector-ref p 0) 4)))
                       (vector-ref
                         (vector-ref p (vector-ref p 2))
                         (vector-ref (vector-ref p 0) (vector-ref p 4)))))]
             [x (make-vector 6)]
             [y (make-vector 7)])
        (begin
          (vector-set! x 0 y)
          (vector-set! x 1 x)
          (vector-set! y 0 x)
          (vector-set! y '1 '-4421)
          (vector-set! x '2 '0)
          (vector-set! x '3 '-37131)
          (vector-set! x '4 '4)
          (vector-set! x '5 '6)
          (vector-set! y '2 '-55151)
          (vector-set! y '3 '-32000911)
          (vector-set! y '4 '5)
          (vector-set! y '5 '55)
          (vector-set! y '6 '-36)
          (* (f x) 2)))
    (let ([vect (make-vector 5)])
      (vector-set! vect 0 123)
      (vector-set! vect 1 10)
      (vector-set! vect 2 7)
      (vector-set! vect 3 12)
      (vector-set! vect 4 57)
      (letrec ([vector-scale! 
                 (lambda (vect scale)
                   (let ([size (vector-length vect)])
                     (letrec ([f (lambda (idx)
                                   (if (>= idx 1)
                                     (let ([idx (- idx 1)])
                                       (vector-set! vect idx
                                                    (* (vector-ref vect idx)
                                                       scale))
                                       (f idx))))])
                       (f size))))])
        (vector-scale! vect 10))
        (letrec ([vector-sum (lambda (vect)
                               (letrec ([f (lambda (idx)
                                             (if (< idx 1)
                                                 0
                                                 (+ (vector-ref vect (- idx 1))
                                                    (f (- idx 1)))))])
                                 (f (vector-length vect))))])
          (vector-sum vect)))
    (letrec ([a (lambda (u v w x) 
                  (if (= u 0) 
                      (b v w x)
                      (a (- u 1) v w x)))]
             [b (lambda (q r x)
                  (let ([p (* q r)])
                    (e (* q r) p x)))]
             [c (lambda (x) (* 5 x))]
             [e (lambda (n p x)
                  (if (= n '0) 
                      (c p)
                      (o (- n 1) p x)))]
             [o (lambda (n p x) 
                  (if (= 0 n)
                      (c x)
                      (e (- n 1) p x)))])
      (let ([x 5])
        (a 3 2 1 x)))
    ((letrec ([length (lambda (ptr)
                        (if (null? ptr) 0 (+ 1 (length (cdr ptr)))))])
       length)
     '(5 10 11 5 15))
    (letrec ([count-leaves (lambda (p)
                             (if (pair? p)
                                 (+ (count-leaves (car p))
                                    (count-leaves (cdr p)))
                                 1))])
      (count-leaves 
        (cons 
          (cons '0 (cons '0 '0))
          (cons 
            (cons (cons (cons '0 (cons '0 '0)) '0) '0)
            (cons 
              (cons (cons '0 '0) (cons '0 (cons '0 '0)))
              (cons (cons '0 '0) '0))))))
    (letrec ([add1 (lambda (n) (+ n 1))]
             [map (lambda (f ls)
                    (if (null? ls) '() (cons (f (car ls)) (map f (cdr ls)))))]
             [sum (lambda (ls)
                      (if (null? ls) 0 (+ (car ls) (sum (cdr ls)))))])
      (let ([ls '(5 4 3 2 1)])
        (let ([ls (cons '10 (cons '9 (cons '8 (cons '7 (cons '6 ls)))))])
          (sum (map add1 ls)))))
    (letrec ([list-ref (lambda (ls offset)
                           (if (= offset 0)
                               (car ls)
                               (list-ref (cdr ls) (- offset 1))))]
             [add (lambda (v w) (+ v w))]
             [sub (lambda (v w) (- v w))]
             [mult (lambda (v w) (* v w))]
             [expt (lambda (v w) (if (= w 0) 1 (* v (expt v (- w 1)))))]
             [selector (lambda (op* sel rand1 rand2)
                           (if (null? sel)
                               0
                               (cons ((list-ref op* (car sel))
                                      (car rand1) (car rand2))
                                     (selector op* (cdr sel) (cdr rand1)
                                               (cdr rand2)))))]
             [sum (lambda (ls) (if (pair? ls) (+ (car ls) (sum (cdr ls))) 0))])
      (sum (selector (cons add (cons sub (cons mult (cons expt ()))))
                     '(2 0 1 3 2) '(5 9 10 2 3) '(3 1 3 3 8))))
    (letrec ([thunk-num (lambda (n) (lambda () n))]
             [force (lambda (th) (th))]
             [add-ths (lambda (th1 th2 th3 th4)
                        (+ (+ (force th1) (force th2))
                           (+ (force th3) (force th4))))])
      (add-ths (thunk-num 5) (thunk-num 17) (thunk-num 7) (thunk-num 9)))
    (letrec ([x 7] [f (lambda () x)]) (f))
    ((lambda (y) ((lambda (f) (f (f y))) (lambda (y) y))) 4)
    (let ([double (lambda (a) (+ a a))]) (double 10))
    (let ([t #t] [f #f])
      (letrec ((even (lambda (x) (if (= x 0) t (odd (- x 1)))))
               (odd (lambda (x) (if (= x 0) f (even (- x 1))))))
        (odd 13)))
    (letrec ([remq (lambda (x ls)
                     (if (null? ls)
                         '()
                         (if (eq? (car ls) x)
                             (remq x (cdr ls))
                             (cons (car ls) (remq x (cdr ls))))))])
      (remq 3 '(3 1 3)))
    (letrec ([make-param (lambda (val)
                           (let ([x val])
                             (letrec ([param (lambda (set val)
                                               (if set (set! x val) x))])
                               param)))])
      (let ([p (make-param 10)])
        (p #t 15)
        (p #f #f)))
    (let ([x 0])
      (letrec ([inc (lambda () (set! x (+ x 1)))]
               [dec (lambda () (set! x (- x 1)))])
        (inc) (dec) (dec) (inc) (inc) (inc) (dec) (inc) x))
    (letrec ([gcd (lambda (x y)
                    (if (= y 0) 
                        x 
                        (gcd (if (> x y) (- x y) x)
                             (if (> x y) y (- y x)))))])
      (gcd 1071 1029))
    (letrec ([sub1 (lambda (n) (- n 1))]
             [fib (lambda (n)
                    (if (= 0 n)
                        0
                        (if (= 1 n)
                            1
                            (+ (fib (sub1 n))
                               (fib (sub1 (sub1 n)))))))])
      (fib 10))
    (letrec ([ack (lambda (m n)
                    (if (= m 0)
                        (+ n 1)
                        (if (if (> m 0) (= n 0) #f)
                            (ack (- m 1) 1)
                            (ack (- m 1) (ack m (- n 1))))))])
      (ack 2 4))
    (letrec ([fib (lambda (n) 
                    (letrec ([fib (lambda (n a b)
                                    (if (= n 0)
                                        a
                                        (fib (- n 1) b (+ b a))))])
                      (fib n 0 1)))])
      (fib 5))
    ((((((lambda (x)
            (lambda (y)
              (lambda (z)
                (lambda (w)
                  (lambda (u)
                    (+ x (+ y (+ z (+ w u)))))))))
         5) 6) 7) 8) 9)
    (let ([t #t] [f #f])
      (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
        (letrec
          ([even (lambda (x) (if (= x 0) (id (car bools)) (odd (- x 1))))]
           [odd (lambda (y) (if (= y 0) (id (cdr bools)) (even (- y 1))))])
          (odd 5))))
    (let ([x 7] [y 4])
      (or (and (fixnum? x) (= x 4) (fixnum? y) (= y 7))
          (and (fixnum? x) (= x 7) (fixnum? y) (= y 4))))
    (let ((y '()) (z 10))
      (let ((test-ls (cons 5 y)))
        (set! y (lambda (f)
                  ((lambda (g) (f (lambda (x) ((g g) x))))
                   (lambda (g) (f (lambda (x) ((g g) x)))))))
        (set! test-ls (cons z test-ls))
        (letrec ((length (lambda (ls)
                           (if (null? ls) 0 (+ 1 (length (cdr ls)))))))
          (let ((len (length test-ls)))
            (eq? (begin
                   (set! length (y (lambda (len)
                                     (lambda (ls)
                                       (if (null? ls)
                                           0
                                           (+ 1 (len (cdr ls))))))))
                   (length test-ls))
                 len)))))
    (letrec ([if-test (lambda (n x y)
                        (if (= n 0)
                            (vector-set! x 0 (+ (vector-ref x 0)
                                                (vector-ref y 0)))
                            (vector-set! y 0 (+ (vector-ref y 0)
                                                (vector-ref x 0))))
                        (vector-set! x 0 (+ (vector-ref x 0) n))
                        (if (if (= n (vector-ref y 0)) #f #t)
                            (+ n (vector-ref x 0))
                            (+ n (vector-ref y 0))))])
      (let ([q (make-vector 1)] [p (make-vector 1)])
        (vector-set! q 0 1)
        (vector-set! p 0 2)
        (if-test 3 q p)))
    (letrec ([if-test (lambda (n)
                        (let ([m (make-vector 1)]
                              [x (make-vector 1)]
                              [y (make-vector 1)])
                          (vector-set! m 0 n)
                          (vector-set! x 0 1)
                          (begin
                            (vector-set! y 0 1)
                            (if (eq? (vector-ref m 0) 0)
                                (vector-set! (vector-ref x 0) 0
                                             (+ (vector-ref x 0)
                                                (vector-ref y 0)))
                                (vector-set! y 0 (+ (vector-ref y 0)
                                                    (vector-ref x 0))))
                            (vector-set! x 0 (+ (vector-ref x 0)
                                                (vector-ref m 0))))
                          (if (if (eq? (vector-ref m 0) (vector-ref y 0)) #f #t)
                              (vector-set! m 0 (+ (vector-ref m 0)
                                                  (vector-ref x 0)))
                              (vector-set! m 0 (+ (vector-ref m 0)
                                                  (vector-ref y 0))))
                          (+ (vector-ref x 0) (vector-ref m 0))))])
      (if-test 1))
    (letrec ([f (lambda (x) (+ 1 x))]
             [g (lambda (x) (- x 1))]
             [t (lambda (x) (- x 1))]
             [j (lambda (x) (- x 1))]
             [i (lambda (x) (- x 1))]
             [h (lambda (x) (- x 1))])
      (let ([x 80])
        (let ([a (f x)]
              [b (g x)]
              [c (h (i (j (t x))))])
          (* a (* b (+ c 0))))))
    (let ([f (lambda (x) (+ 1 x))] [g (lambda (x) (- x 1))])
      (let ([x 80])
        (let ([a (f x)]
              [b (g x)]
              [c (letrec ([h (lambda (x) (- x 1))])
                   (h (letrec ([i (lambda (x) (- x 1))])
                        (i
                          (letrec ([t (lambda (x) (- x 1))]
                                   [j (lambda (x) (- x 1))])
                            (j (t x)))))))])
          (* a (* b (+ c 0))))))
    (letrec ([fact (lambda (n)
                     (if (= n 0)
                         1
                         (let ([t (- n 1)])
                           (let ([t (fact t)])
                             (* n t)))))])
      (fact 10))
    (letrec ([fib (lambda (n k)
                    (if (or (= n 0) (= n 1))
                        (k 1)
                        (fib (- n 1) (lambda (w)
                                       (fib (- n 2) (lambda (v)
                                                      (k (+ w v))))))))])
      (fib 10 (lambda (x) x)))
    (letrec ()
      (let ([n (let ([p (make-vector 1)]) (vector-set! p 0 1) p)])
        (let ([a 2])
          (let ([b 3])
            (vector-set! n 0 (+ (vector-ref n 0) 
                                (if (= (+ (vector-ref n 0) b) b) 5 10)))
            (vector-set! n 0 (+ (vector-ref n 0) b)))
          (vector-set! n 0 (+ (vector-ref n 0) a)))
        (+ (vector-ref n 0) (vector-ref n 0))))
    (let ([dot-product (lambda (v1 v2)
                         (if (and (vector? v1) (vector? v2)
                                  (= (vector-length v1) (vector-length v2)))
                             (letrec ([f (lambda (i)
                                           (if (= i 0)
                                               1
                                               (let ([i (- i 1)])
                                                 (+ (* (vector-ref v1 i)
                                                       (vector-ref v2 i))
                                                    (f i)))))])
                               (f (vector-length v1)))
                             #f))])
      (cons (dot-product '(1 2) '#(3 4))
            (cons (dot-product '#(1 2) '#(3 4 5))
                  (cons (dot-product '#(4 5 6 7) '#(2 9 8 1)) '()))))
    (letrec ([num-list? (lambda (ls)
                          (if (null? ls)
                              #t
                              (if (fixnum? (car ls))
                                  (num-list? (cdr ls))
                                  #f)))]
             [length (lambda (ls)
                       (if (null? ls)
                           0
                           (+ (length (cdr ls)) 1)))]
             [dot-prod (lambda (ls1 ls2)
                         (if (if (null? ls1) (null? ls2) #f)
                             0
                             (+ (* (car ls1) (car ls2))
                                (dot-prod (cdr ls1) (cdr ls2)))))])
      (let ([ls1 '(1 2 3 4 5)]
            [ls2 '(5 4 3 2 1)])
        (if (if (if (eq? (num-list? ls1) #f) #f #t)
                (if (if (eq? (num-list? ls2) #f) #f #t)
                    (= (length ls1) (length ls2))
                    #f)
                #f)
            (dot-prod ls1 ls2)
            #f)))
    (letrec ([num-list? (lambda (ls)
                          (or (null? ls) 
                              (and (fixnum? (car ls)) (num-list? (cdr ls)))))]
             [map (lambda (f ls)
                      (if (null? ls) 
                          '()
                          (cons (f (car ls)) (map f (cdr ls)))))]
             [square (lambda (n) (* n n))])
      (let ([ls '(1 2 3 4 5)])
        (if (num-list? ls) (set-car! ls (map square ls)))
        ls))
    (letrec ([num-list? (lambda (ls)
                          (if (null? ls)
                              #t
                              (if (fixnum? (car ls))
                                  (num-list? (cdr ls))
                                  #f)))]
             [list-product (lambda (ls)
                             (if (null? ls)
                                 1
                                 (* (car ls) (list-product (cdr ls)))))])
      (let ([ls '(1 2 3 4 5)])
        (if (num-list? ls) (list-product ls) #f)))
    (letrec ([f (lambda (x y)
                    (if x (h (+ x y)) (g (+ x 1) (+ y 1))))]
             [g (lambda (u v)
                  (let ([a (+ u v)] [b (* u v)])
                    (letrec ([e (lambda (d)
                                  (let ([p (cons a b)])
                                    (letrec ([q (lambda (m)
                                                  (if (< m u)
                                                      (f m d)
                                                      (h (car p))))])
                                      (q (f a b)))))])
                      (e u))))]
             [h (lambda (w) w)])
      (f 4 5))
    (let ((y '())
          (z 10))
      (let ((test-ls (cons 5 y)))
        (set! y (lambda (f)
                  ((lambda (g) (f (lambda (x) ((g g) x))))
                   (lambda (g) (f (lambda (x) ((g g) x)))))))
        (set! test-ls (cons z test-ls))
        (letrec ((length (lambda (ls)
                            (if (null? ls) 0 (+ 1 (length (cdr ls)))))))
          (let ((len (length test-ls)))
            (eq? (begin
                  (set! length (y (lambda (len)
                                    (lambda (ls)
                                      (if (null? ls)
                                          0
                                          (+ 1 (len (cdr ls))))))))
                  (length test-ls))
                 len)))))
    (letrec ([curry-list
               (lambda (x)
                 (lambda (y)
                   (lambda (z)
                     (lambda (w)
                       (cons x (cons y (cons z (cons w ()))))))))]
             [append (lambda (ls1 ls2)
                       (if (null? ls1)
                           ls2
                           (cons (car ls1)
                                 (append (cdr ls1) ls2))))])
      (append
        ((((curry-list 1) 2) 3) 4)
        ((((curry-list 5) 6) 7) 8)))
    (letrec ([quotient (lambda (x y)
                         (if (< x 0)
                             (- 0 (quotient (- 0 x) y))
                             (if (< y 0)
                                 (- 0 (quotient x (- 0 y)))
                                 (letrec ([f (lambda (x a)
                                               (if (< x y)
                                                   a
                                                   (f (- x y) (+ a '1))))])
                                   (f x 0)))))])
      (let ([sub-interval 1])
        (letrec ([sub-and-continue (lambda (n acc k)
                                     (k (- n sub-interval) (* n acc)))]
                 [strange-fact (lambda (n acc)
                                 (if (= n 0)
                                   (lambda (proc) (proc acc))
                                   (sub-and-continue n acc strange-fact)))])
          (let ([x 20] [fact (let ([seed 1])
                               (lambda (n) (strange-fact n seed)))])
            (let ([x (cons x (if #f #f))])
              (letrec ([answer-user (lambda (ans) (quotient ans (car x)))])
                (let ([give-fact5-answer (fact 5)] [give-fact6-answer (fact 6)])
                  (begin
                    (set-car! x (give-fact5-answer answer-user))
                    (set-car! x (give-fact6-answer answer-user))
                    (car x)))))))))
    
    (letrec ([fib (lambda (x)
                    (let ([decrx (lambda () (lambda (i) (set! x (- x i))))])
                      (if (< x 2)
                          1
                          (+ (begin ((decrx) 1) (fib x))
                             (begin ((decrx) 1) (fib x))))))])
      (fib 10))
    ; test use of keywords/primitives as variables
    (let ([quote (lambda (x) x)]
          [let (lambda (x y) (- y x))]
          [if (lambda (x y z) (cons x z))]
          [cons (lambda (x y) (cons y x))]
          [+ 16])
      (set! + (* 16 2))
      (cons (let ((quote (lambda () 0))) +)
            (if (quote (not #f)) 720000 -1)))
    (letrec ([sum-all (lambda (x)
                        (if (fixnum? x)
                            x
                            (if (vector? x)
                                (sum-vector x)
                                (if (pair? x)
                                    (sum-pair x)
                                    (if (procedure? x)
                                        (sum-all (x))
                                        0)))))]
             [sum-vector (lambda (v)
                           (letrec ([l (lambda (v i)
                                         (if (= i 0) 
                                             0 
                                             (sum-all 
                                               (vector-ref v (- i 1)))))])
                             (l v (vector-length v))))]
             [sum-pair (lambda (p)
                         (+ (sum-all (car p)) (sum-all (cdr p))))])
      (sum-all (lambda () '#((7 8 1) 
                             #(81 23 8)
                             #(#(#(12) 56) 18 ((1 2) (3 ((4)) 5)))))))
    (letrec ([div (lambda (d n)
                    (letrec ([f (lambda (d n q)
                                  (if (> n d)
                                      q
                                      (f (- d n) n (+ q 1))))])
                      (f d n 0)))])
      (letrec ([alloc (lambda (n) (make-vector (div n 8)))]
               [mref (lambda (x y)
                       (if (vector? x)
                           (vector-ref x (div y 8))
                           (vector-ref y (div x 8))))]
               [mset! (lambda (x y z)
                        (if (vector? x)
                            (vector-set! x (div y 8) z)
                            (vector-set! y (div x 8) z))
                        (if #f #f))])
        (letrec ([stack-push (lambda (self val)
                               (mset! (mref self 16) (* (mref self 8) 8) val)
                               (mset! self 8 (+ (mref self 8) 1))
                               self)]
                 [stack-pop (lambda (self)
                              (mset! self 8 (- (mref 8 self) 1))
                              (mref (mref self 16) (* (mref self 8) 8)))]
                 [stack-top (lambda (self)
                              (mref (mref self 16) 
                                    (* (- (mref 8 self) 1) 8)))])
          (letrec ([stack-new
                     (let ([meths (alloc (* 3 8))])
                       (mset! meths 0 stack-push)
                       (mset! meths 8 stack-pop)
                       (mset! meths 16 stack-top)
                       (lambda (size)
                         (let ([self (alloc (* 3 8))])
                           (mset! self 0 meths)
                           (mset! self 8 0)
                           (mset! self 16 (alloc (* 8 size)))
                           self)))]
                   [invoke (lambda (obj meth-idx)
                             (mref (mref obj 0) (* meth-idx 8)))])
            (let ([s1 (stack-new 10)])
              (begin
                ((invoke s1 0) s1 10) ;; push '10
                ((invoke s1 0) s1 20) ;; push '20
                ((invoke s1 0) s1 30) ;; push ... well you get the idea
                ((invoke s1 0) s1 40)
                ((invoke s1 0) s1 0)
                ((invoke s1 0) s1 60)
                ((invoke s1 0) s1 70)
                ((invoke s1 0) s1 80)
                ((invoke s1 0) s1 90)
                ((invoke s1 0) s1 100)
                (let ([s2 (stack-new 6)])
                  (begin
                    ((invoke s2 0) s2 ((invoke s1 1) s1)) ;; push pop
                    ((invoke s1 1) s1) ;; pop
                    ((invoke s2 0) s2 ((invoke s1 1) s1))
                    ((invoke s1 1) s1) ;; pop
                    ((invoke s2 0) s2 ((invoke s1 1) s1))
                    ((invoke s1 1) s1) ;; pop
                    ((invoke s2 0) s2 ((invoke s1 1) s1))
                    ((invoke s1 1) s1) ;; pop
                    ((invoke s2 0) s2 ((invoke s1 1) s1))
                    ((invoke s2 0) s2 ((invoke s1 1) s1))
                    (let ([x (+ ((invoke s2 1) s2) ((invoke s2 1) s2))])
                      (* (+ (let ([x (+ ((invoke s2 2) s2)
                                        ((invoke s2 2) s2))])
                              (- x (+ ((invoke s2 1) s2) ((invoke s2 1) s2))))
                            (let ([x (+ ((invoke s2 2) s2)
                                        ((invoke s2 2) s2))])
                              (- (+ ((invoke s2 1) s2) ((invoke s2 1) s2)) x)))
                         x))))))))))
    (if (lambda () 1)
        (let ((a 2))
          (if (if ((lambda (x)
                     (let ((x (set! a (set! a 1))))
                       x)) 1)
                  (if (eq? a (void))
                      #t
                      #f)
                  #f)
              #36rgood        ; dyb: cannot use symbols, so use radix 36
              #36rbad)))

   ; contributed by Ryan Newton
    (letrec
      ([dropsearch
         (lambda (cell tree)
           (letrec
             ([create-link
                  (lambda (node f)
                    (lambda (g)
                      (if (not (pair? node))
                          (f g)
                          (if (eq? node cell)
                              #f
                              (f (create-link (car node)
                                              (create-link (cdr node) g)))))))]
              [loop
                (lambda (link)
                  (lambda ()
                    (if link
                        (loop (link (lambda (v) v)))
                        #f)))])
             (loop (create-link tree (lambda (x) x)))))]
         [racethunks
           (lambda (thunkx thunky)
             (if (if thunkx thunky #f)
                 (racethunks (thunkx) (thunky))
                 (if thunky
                     #t
                     (if thunkx
                         #f
                         '()))))]
         [higher?
           (lambda (x y tree)
             (racethunks (dropsearch x tree)
                         (dropsearch y tree)))]
         [under?
           (lambda (x y tree)
             (racethunks (dropsearch x y)
                         (dropsearch x tree)))]
         [explore
           (lambda (x y tree)
             (if (not (pair? y))
                 #t
                 (if (eq? x y)
                     #f    ;This will take out anything that points to itself
                     (let ((result (higher? x y tree)))
                       (if (eq? result #t)
                           (if (explore y (car y) tree)
                               (explore y (cdr y) tree)
                               #f)
                           (if (eq? result #f)
                               (process-vertical-jump x y tree)
                               (if (eq? result '())
                                   (process-horizontal-jump x y tree)
                                   )))))))]
         [process-vertical-jump
           (lambda (jumpedfrom jumpedto tree)
             (if (under? jumpedfrom jumpedto tree)
                 #f
                 (fullfinite? jumpedto)))]
         [process-horizontal-jump
           (lambda (jumpedfrom jumpedto tree)
             (fullfinite? jumpedto))]
         [fullfinite?
           (lambda (pair)
             (if (not (pair? pair))
                 #t
                 (if (explore pair (car pair) pair)
                     (explore pair (cdr pair) pair)
                     #f)))])
       (cons
         (fullfinite? (cons 1 2))
         (cons
           (fullfinite? (let ((x (cons 1 2))) (set-car! x x) x))
           (cons
             (fullfinite? (let ([a (cons 0 0)] [b (cons 0 0)] [c (cons 0 0)])
                            (set-car! a b) (set-cdr! a c) (set-cdr! b c)
                            (set-car! b c) (set-car! c b) (set-cdr! c b) a))
             '()))))
    (letrec ([zero? (lambda (x) (= x 0))]
         [sub1 (lambda (n) (- n 1))]
         [assq (lambda (sym al)
                 (if (null? al)
                     #f
                     (let ([entry (car al)])
                       (if (eq? sym (car entry))
                           (cdr entry)
                           (assq sym (cdr al))))))]
         [map (lambda (p ls)
                (if (null? ls)
                    '()
                    (cons (p (car ls)) (map p (cdr ls)))))]
         [snoc (lambda (ls sym)
                 (if (null? ls)
                     (cons sym '())
                     (cons (car ls) (snoc (cdr ls) sym))))]
         [iota (lambda (n)
                 (if (zero? n)
                     '(0)
                     (snoc (iota (sub1 n)) n)))]
         [fib (lambda (n)
                (if (zero? n)
                    0
                    (if (= n 1)
                        1
                        (+ (fib (- n 1)) (fib (- n 2))))))]
         [bounded-memoize (lambda (p bound)
                            (let ([memo '()])
                              (lambda (arg)
                                (if (if (< arg bound) (assq arg memo) #f)
                                    (assq arg memo)
                                    (let ([ans (p arg)])
                                      (if (< arg bound)
                                          (set! memo (cons (cons arg ans) memo)))
                                      ans)))))])
      (set! fib (bounded-memoize fib 5))
      (map fib (iota 10)))

    ;; Francis Fernandez
    (and (+ ((if (not (cons '1 '(2))) 
                 '#t 
                 (letrec ([f.1 '3] [f.2 (lambda (x.3) (+ x.3 '4))])
                   f.2))
             '5) '6) '#f)

    ;; Thiago Rebello
    (let ([a 5]
          [b 4])
      (letrec ([c (lambda(d e) (* d e))]
               [f (lambda(g h) (cons g h))])
        (if (or (> (c a b) 15) (= (c a b) 20))
            (f a b))))

    ;; Yin Wang
    (let ([begin (lambda (x y) (+ x y))]
          [set! (lambda (x y) (* x y))])
      (let ([lambda (lambda (x) (begin 1 x))])
        (let ([lambda (lambda (set! 1 2))])
          (let ([let (set! lambda lambda)])
            (begin let (set! lambda (set! 4 (begin 2 3))))))))

    ;; Ben Peters
    (let ([x '(4 5 6)]
          [y '(7 8 9)])
      (cons 1 (cons 2 (cons 3 (cons (car x) (cons (car (cdr x)) (cons (car (cdr (cdr x))) y)))))))
    
    ;; Patrick Jensen
    (let ([a 1])
      (letrec ([add1 (lambda (b) (+ b 1))]
               [sub1 (lambda (b) (- b 1))])
        (let ([c (lambda (a)
                   (if (or (not (= a 1)) (and (> a 1) (< a 4)))
                       (add1 a)
                       (sub1 a)))])
          (let ([d (c a)] [e (c (add1 a))] [f (c (sub1 a))])
            (cons d (cons e (cons f '())))))))

    ;; Melanie Dybvig
    (letrec ((not (lambda (x) x))
             (a (if (< (* 3 3) (+ 3 3)) #t #f))
             (b 7))
      (if (not a)
          (set! b (+ b 2))
          (if (not (not a))
              (set! b (- b 2))))
      (cons b (or (not (not a)) (not a))))

    ;; Lindsey Kuper
    (let ([foo (lambda (lambda)
                 (lambda))])
      (let ([lambda foo]
            [bar (lambda () #t)])
        (foo bar)))
 
    ;; Yu-Shan Huang
    (let ([x 1])
      (let ([x 2])
        (if (and (< x 5) (not #f))
          (set! x 6)))
      x)

    ;; Chabane Maidi
    (letrec ([merge (lambda (ls ls2)
                      (if (null? ls)
                          ls2
                          (if (null? ls2)
                              ls
                              (if (< (car ls) (car ls2))
                                  (cons (car ls) (merge (cdr ls) ls2))
                                  (cons (car ls2) (merge ls (cdr ls2)))))))]
             [sort (lambda (ls)
                     (if (null? ls)
                         ls
                         (if (null? (cdr ls))
                             ls
                             (let ([halves (halves ls '() '() #t)])
                               (let ([first (car halves)]
                                     [second (car (cdr halves))])
                                 (merge (sort first) (sort second)))))))]
             [halves (lambda (ls first second first?)
                       (if (null? ls)
                           (cons first (cons second '()))
                           (if first?
                               (halves (cdr ls) (cons (car ls) first) second #f)
                               (halves (cdr ls) first (cons (car ls) second) #t))))]
             [pend (lambda (ls ls2)
                     (if (null? ls)
                         ls2
                         (cons (car ls) (pend (cdr ls) ls2))))])
      (pend (sort '(1 5 5 8 2 3 9)) (sort '(5 9 5 7 7 8 7))))

    ;; Kewal Karavinkoppa
    (letrec ([depth (lambda (ls)
                      (if (null? ls)
                          1
                          (if (pair? (car ls))
                              (let ([l ((lambda (m)
                                          (+ m 1))
                                        (depth (car ls)))]
                                    [r (depth (cdr ls))])
                                (if (< l r) r l))
                              (depth (cdr ls)))))])
      (depth '(1 2 (3 (4 (5 (6 7)))))))

    ;; Brennon York
    ((lambda (x) (if (if (eq? x 5) x (and x 1 2 3 4 (or 6 7 8 9))) 3)) 4)

    ;; Nilesh Mahajan
    (letrec ([F (lambda (func-arg)
                  (lambda (n)
                    (if (= n 0)
                        1
                        (* n (func-arg (- n 1))))))])
      (letrec ([Y (lambda (X)
                    ((lambda (procedure)
                       (X (lambda (arg) ((procedure procedure) arg))))
                     (lambda (procedure)
                       (X (lambda (arg) ((procedure procedure) arg))))))])
        (letrec ([fact (Y F)])
          (fact 5))))

    ;; Joseph Knecht
    (letrec ([f (lambda () '(1 . 2))]) (eq? (f) (f)))

    ;; Emily Lyons
    (letrec ([extend (lambda (num alist)
                       (if (null? alist)
                           (cons (cons num 1) '())
                           (if (= num (car (car alist)))
                               (cons (cons num (+ 1 (cdr (car alist))))
                                     (cdr alist))
                               (cons (car alist)
                                     (extend num (cdr alist))))))]
             [loop (lambda (ls alist)
                     (if (null? ls)
                         alist
                         (loop (cdr ls) (extend (car ls) alist))))])
      (loop '(1 3 4 5 5 4 5 2 3 4 1) '()))
    ))
