(language-wrapper
  (lambda (pass-name x)
    (define rewrite-opnds
      (lambda (x)
        (match x
          [,r (guard (disp-opnd? r))
           `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
          [,r (guard (index-opnd? r))
           `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
          [(set! ,r ,[expr]) (guard (disp-opnd? r))
           `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
          [(set! ,r ,[expr]) (guard (index-opnd? r))
           `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
          [(,[expr] ...) `(,expr ...)]
          [,x x])))
    (case pass-name
      [(source verify-scheme)
       `(let ()
          (import (except scheme set!))
          (define int64-in-range?
            (lambda (x)
              (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
          (define handle-overflow
            (lambda (x)
              (cond
                [(not (number? x)) x]
                [(int64-in-range? x) x]
                [(not (= x (logand 18446744073709551615 x)))
                 (handle-overflow (logand 18446744073709551615 x))]
                [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                [else (handle-overflow (- x (expt 2 64)))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define-syntax locate
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ ([x* loc*] ...) body)
                 (let-syntax ([x* (identifier-syntax 
                                    (id loc*) 
                                    ((set! id e) 
                                     (set! loc* (handle-overflow e))))] ...)
                   body)])))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! r15 k)
              ,x))
          rax)]
      [(finalize-locations)
       `(let ()
          (import (except scheme set!))
          (define int64-in-range?
            (lambda (x)
              (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
          (define handle-overflow
            (lambda (x)
              (cond
                [(not (number? x)) x]
                [(int64-in-range? x) x]
                [(not (= x (logand 18446744073709551615 x)))
                 (handle-overflow (logand 18446744073709551615 x))]
                [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                [else (handle-overflow (- x (expt 2 64)))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! r15 k)
              ,x))
          rax)]
      [(expose-frame-var)
       `(let ()
          (import (except scheme set!))
          (define int64-in-range?
            (lambda (x)
              (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
          (define handle-overflow
            (lambda (x)
              (cond
                [(not (number? x)) x]
                [(int64-in-range? x) x]
                [(not (= x (logand 18446744073709551615 x)))
                 (handle-overflow (logand 18446744073709551615 x))]
                [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                [else (handle-overflow (- x (expt 2 64)))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! r15 k)
              ,(rewrite-opnds x)))
          rax)]
      [(expose-basic-blocks)
       `(let ()
          (import (except scheme set!))
          (define int64-in-range?
            (lambda (x)
              (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
          (define handle-overflow
            (lambda (x)
              (cond
                [(not (number? x)) x]
                [(int64-in-range? x) x]
                [(not (= x (logand 18446744073709551615 x)))
                 (handle-overflow (logand 18446744073709551615 x))]
                [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                [else (handle-overflow (- x (expt 2 64)))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (call/cc 
            (lambda (k)
              (set! r15 k)
              ,(rewrite-opnds x)))
          rax)]
      [(flatten-program)
       `(let ()
          (import (except scheme set!))
          (define int64-in-range?
            (lambda (x)
              (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
          (define handle-overflow
            (lambda (x)
              (cond
                [(not (number? x)) x]
                [(int64-in-range? x) x]
                [(not (= x (logand 18446744073709551615 x)))
                 (handle-overflow (logand 18446744073709551615 x))]
                [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                [else (handle-overflow (- x (expt 2 64)))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define-syntax code
            (lambda (x)
              (define build
                (lambda (body)
                  (syntax-case body ()
                    [() #'(())]
                    [(label expr ...)
                     (identifier? #'label)
                     (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
                       #'(((bounce label))
                          (define label
                            (lambda ()
                              (bounce (lambda () expr ...))))
                          defn ...))]
                    [(expr1 expr ...)
                     (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
                       #'((expr1 expr ...) defn ...))])))
              (syntax-case x ()
                [(k expr ...)
                 (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
                   #'((call/cc
                        (lambda (bounce)
                          defn ...
                          expr ...))))])))
          (define-syntax jump
            (syntax-rules ()
              [(_ target) (target)]))
          (call/cc 
            (lambda (k)
              (set! r15 k)
              ,(rewrite-opnds x)))
          rax)]
      [else x])))
