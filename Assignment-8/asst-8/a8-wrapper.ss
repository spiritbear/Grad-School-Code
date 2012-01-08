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
    (define compute-frame-size
      (lambda (x)
        (match x
          [(,[fs*] ...) (apply max 0 fs*)]
          [,x (if (frame-var? x) (+ (frame-var->index x) 1) 0)])))
    (case pass-name
      [(source verify-uil remove-complex-opera* flatten-set!)
       `(let ()
          (import (except scheme set! lambda))
          (define int64-in-range?
            (let ()
              (import scheme)
              (lambda (x)
                (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
          (define handle-overflow
            (let ()
              (import scheme)
              (lambda (x)
                (cond
                  [(not (number? x)) x]
                  [(int64-in-range? x) x]
                  [(not (= x (logand 18446744073709551615 x)))
                   (handle-overflow (logand 18446744073709551615 x))]
                  [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                  [else (handle-overflow (- x (expt 2 64)))]))))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules ()
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define-syntax locals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-who alloc
            (lambda (nbytes)
              (unless (let ([nwords (fxsrl nbytes align-shift)])
                        (= (fxsll nwords align-shift) nbytes))
                (error who "~s is not a multiple of word size" nbytes))
              (let ([addr ,allocation-pointer-register])
                (set! ,allocation-pointer-register (+ addr nbytes))
                ($check-heap-overflow ,allocation-pointer-register)
                addr)))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          ,x)]
      [(impose-calling-conventions)
       (let ([frame-size (compute-frame-size x)])
         `(let ()
            (import (except scheme set! letrec))
            (define int64-in-range?
              (let ()
                (import scheme)
                (lambda (x)
                  (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
            (define handle-overflow
              (let ()
                (import scheme)
                (lambda (x)
                  (cond
                    [(not (number? x)) x]
                    [(int64-in-range? x) x]
                    [(not (= x (logand 18446744073709551615 x)))
                     (handle-overflow (logand 18446744073709551615 x))]
                    [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                    [else (handle-overflow (- x (expt 2 64)))]))))
            (define-syntax letrec
              (let ()
                (import scheme)
                (syntax-rules (lambda)
                  [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                   (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                     (fluid-let ([$fp-offset 0]) letrec-body))])))
            (define-syntax set!
              (let ()
                (import scheme)
                (syntax-rules ()
                  [(_ x expr)
                   (set! x (handle-overflow expr))])))
            (define-syntax locals
              (syntax-rules ()
                [(_ (x* ...) body) (let ([x* 0] ...) body)]))
            (define-syntax new-frames
              (lambda (x)
                (import scheme)
                (syntax-case x (return-point)
                  [(_ ((nfv ...) ...) expr)
                   (with-syntax ([((i ...) ...) (map enumerate #'((nfv ...) ...))])
                     #'(let ([top (fxsll ,frame-size align-shift)])
                         (define-syntax nfv
                           (identifier-syntax
                             [id (mref (- ,frame-pointer-register $fp-offset)
                                       (fxsll (+ i ,frame-size) align-shift))]
                             [(set! id e) 
                              (mset! (- ,frame-pointer-register $fp-offset)
                                     (fxsll (+ i ,frame-size) align-shift)
                                     e)]))
                         ...
                         ...
                         expr))])))
            (define-syntax return-point
              (lambda (x)
                (import scheme)
                (syntax-case x ()
                  [(_ rplab expr)
                   #'(let ([top (fxsll ,frame-size align-shift)]
                           [rplab (lambda args (void))])
                       (fluid-let ([$fp-offset (+ $fp-offset top)])
                         (set! ,frame-pointer-register
                           (+ ,frame-pointer-register top))
                         expr
                         (set! ,frame-pointer-register
                           (- ,frame-pointer-register top))))])))
            (define (true) #t)
            (define (false) #f)
            (define (nop) (void))
            (call/cc 
              (lambda (k)
                (set! ,return-address-register k)
                ,x))
            ,return-value-register))]
      [(uncover-frame-conflict)
       (let ([frame-size (compute-frame-size x)])
         `(let ()
            (import (except scheme set! letrec))
            (define int64-in-range?
              (let ()
                (import scheme)
                (lambda (x)
                  (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
            (define handle-overflow
              (let ()
                (import scheme)
                (lambda (x)
                  (cond
                    [(not (number? x)) x]
                    [(int64-in-range? x) x]
                    [(not (= x (logand 18446744073709551615 x)))
                     (handle-overflow (logand 18446744073709551615 x))]
                    [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                    [else (handle-overflow (- x (expt 2 64)))]))))
            (define-syntax letrec
              (let ()
                (import scheme)
                (syntax-rules (lambda)
                  [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                   (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                     (fluid-let ([$fp-offset 0]) letrec-body))])))
            (define-syntax set!
              (let ()
                (import scheme)
                (syntax-rules ()
                  [(_ x expr)
                   (set! x (handle-overflow expr))])))
            (define-syntax locals
              (syntax-rules ()
                [(_ (x* ...) body) (let ([x* 0] ...) body)]))
            (define-syntax spills
              (syntax-rules ()
                [(_ (x* ...) body) (let ([x* 0] ...) body)]))
            (define-syntax call-live
              (syntax-rules ()
                [(_ (x* ...) body) body]))
            (define-syntax frame-conflict
              (syntax-rules ()
                [(_ ct body) body]))
            (define-syntax new-frames
              (lambda (x)
                (import scheme)
                (syntax-case x (return-point)
                  [(_ ((nfv ...) ...) expr)
                   (with-syntax ([((i ...) ...) (map enumerate #'((nfv ...) ...))])
                     #'(let ([top (fxsll ,frame-size align-shift)])
                         (define-syntax nfv
                           (identifier-syntax
                             [id (mref (- ,frame-pointer-register $fp-offset)
                                       (fxsll (+ i ,frame-size) align-shift))]
                             [(set! id e) 
                              (mset! (- ,frame-pointer-register $fp-offset)
                                     (fxsll (+ i ,frame-size) align-shift)
                                     e)]))
                         ...
                         ...
                         expr))])))
            (define-syntax return-point
              (lambda (x)
                (import scheme)
                (syntax-case x ()
                  [(_ rplab expr)
                   #'(let ([top (fxsll ,frame-size align-shift)]
                           [rplab (lambda args (void))])
                       (fluid-let ([$fp-offset (+ $fp-offset top)])
                         (set! ,frame-pointer-register
                           (+ ,frame-pointer-register top))
                         expr
                         (set! ,frame-pointer-register
                           (- ,frame-pointer-register top))))])))
            (define (true) #t)
            (define (false) #f)
            (define (nop) (void))
            (call/cc 
              (lambda (k)
                (set! ,return-address-register k)
                ,x))
            ,return-value-register))]
      [(pre-assign-frame)
       (let ([frame-size (compute-frame-size x)])
         `(let ()
            (import (except scheme set! letrec))
            (define int64-in-range?
              (let ()
                (import scheme)
                (lambda (x)
                  (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
            (define handle-overflow
              (let ()
                (import scheme)
                (lambda (x)
                  (cond
                    [(not (number? x)) x]
                    [(int64-in-range? x) x]
                    [(not (= x (logand 18446744073709551615 x)))
                     (handle-overflow (logand 18446744073709551615 x))]
                    [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                    [else (handle-overflow (- x (expt 2 64)))]))))
            (define-syntax letrec
              (let ()
                (import scheme)
                (syntax-rules (lambda)
                  [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                   (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                     (fluid-let ([$fp-offset 0]) letrec-body))])))
            (define-syntax set!
              (let ()
                (import scheme)
                (syntax-rules ()
                  [(_ x expr)
                   (set! x (handle-overflow expr))])))
            (define-syntax locals
              (syntax-rules ()
                [(_ (x* ...) body) (let ([x* 0] ...) body)]))
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
            (define-syntax call-live
              (syntax-rules ()
                [(_ (x* ...) body) body]))
            (define-syntax frame-conflict
              (syntax-rules ()
                [(_ ct body) body]))
            (define-syntax new-frames
              (lambda (x)
                (import scheme)
                (syntax-case x (return-point)
                  [(_ ((nfv ...) ...) expr)
                   (with-syntax ([((i ...) ...) (map enumerate #'((nfv ...) ...))])
                     #'(let ([top (fxsll ,frame-size align-shift)])
                         (define-syntax nfv
                           (identifier-syntax
                             [id (mref (- ,frame-pointer-register $fp-offset)
                                       (fxsll (+ i ,frame-size) align-shift))]
                             [(set! id e) 
                              (mset! (- ,frame-pointer-register $fp-offset)
                                     (fxsll (+ i ,frame-size) align-shift)
                                     e)]))
                         ...
                         ...
                         expr))])))
            (define-syntax return-point
              (lambda (x)
                (import scheme)
                (syntax-case x ()
                  [(_ rplab expr)
                   #'(let ([top (fxsll ,frame-size align-shift)]
                           [rplab (lambda args (void))])
                       (fluid-let ([$fp-offset (+ $fp-offset top)])
                         (set! ,frame-pointer-register
                           (+ ,frame-pointer-register top))
                         expr
                         (set! ,frame-pointer-register
                           (- ,frame-pointer-register top))))])))
            (define (true) #t)
            (define (false) #f)
            (define (nop) (void))
            (call/cc 
              (lambda (k)
                (set! ,return-address-register k)
                ,x))
            ,return-value-register))]
      [(assign-new-frame)
       `(let ()
          (import (except scheme set! letrec))
          (define int64-in-range?
            (let ()
              (import scheme)
              (lambda (x)
                (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
          (define handle-overflow
            (let ()
              (import scheme)
              (lambda (x)
                (cond
                  [(not (number? x)) x]
                  [(int64-in-range? x) x]
                  [(not (= x (logand 18446744073709551615 x)))
                   (handle-overflow (logand 18446744073709551615 x))]
                  [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                  [else (handle-overflow (- x (expt 2 64)))]))))
          (define-syntax letrec
            (let ()
              (import scheme)
              (syntax-rules (lambda)
                [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                 (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                   (fluid-let ([$fp-offset 0]) letrec-body))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules (,frame-pointer-register)
                [(_ ,frame-pointer-register (op xxx n))
                 (begin
                   (set! $fp-offset (op $fp-offset n))
                   (set! ,frame-pointer-register (op xxx n)))]
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define-syntax locals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax ulocals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax spills
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
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
          (define-syntax frame-conflict
            (syntax-rules ()
              [(_ ct body) body]))
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,x))
          ,return-value-register)]
      [(finalize-frame-locations select-instructions assign-frame)
       `(let ()
          (import (except scheme set! letrec))
          (define int64-in-range?
            (let ()
              (import scheme)
              (lambda (x)
                (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
          (define handle-overflow
            (let ()
              (import scheme)
              (lambda (x)
                (cond
                  [(not (number? x)) x]
                  [(int64-in-range? x) x]
                  [(not (= x (logand 18446744073709551615 x)))
                   (handle-overflow (logand 18446744073709551615 x))]
                  [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                  [else (handle-overflow (- x (expt 2 64)))]))))
          (define-syntax letrec
            (let ()
              (import scheme)
              (syntax-rules (lambda)
                [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                 (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                   (fluid-let ([$fp-offset 0]) letrec-body))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules (,frame-pointer-register)
                [(_ ,frame-pointer-register (op xxx n))
                 (begin
                   (set! $fp-offset (op $fp-offset n))
                   (set! ,frame-pointer-register (op xxx n)))]
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
          (define-syntax locals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax ulocals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax frame-conflict
            (syntax-rules ()
              [(_ ct body) body]))
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,x))
          ,return-value-register)]
      [(uncover-register-conflict)
       `(let ()
          (import (except scheme set! letrec))
          (define int64-in-range?
            (let ()
              (import scheme)
              (lambda (x)
                (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
          (define handle-overflow
            (let ()
              (import scheme)
              (lambda (x)
                (cond
                  [(not (number? x)) x]
                  [(int64-in-range? x) x]
                  [(not (= x (logand 18446744073709551615 x)))
                   (handle-overflow (logand 18446744073709551615 x))]
                  [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                  [else (handle-overflow (- x (expt 2 64)))]))))
          (define-syntax letrec
            (let ()
              (import scheme)
              (syntax-rules (lambda)
                [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                 (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                   (fluid-let ([$fp-offset 0]) letrec-body))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules (,frame-pointer-register)
                [(_ ,frame-pointer-register (op xxx n))
                 (begin
                   (set! $fp-offset (op $fp-offset n))
                   (set! ,frame-pointer-register (op xxx n)))]
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
          (define-syntax locals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax ulocals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax frame-conflict
            (syntax-rules ()
              [(_ ct body) body]))
          (define-syntax register-conflict
            (syntax-rules ()
              [(_ ct body) body]))
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,x))
          ,return-value-register)]
      [(assign-registers)
       `(let ()
          (import (except scheme set! letrec))
          (define int64-in-range?
            (let ()
              (import scheme)
              (lambda (x)
                (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
          (define handle-overflow
            (let ()
              (import scheme)
              (lambda (x)
                (cond
                  [(not (number? x)) x]
                  [(int64-in-range? x) x]
                  [(not (= x (logand 18446744073709551615 x)))
                   (handle-overflow (logand 18446744073709551615 x))]
                  [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                  [else (handle-overflow (- x (expt 2 64)))]))))
          (define-syntax letrec
            (let ()
              (import scheme)
              (syntax-rules (lambda)
                [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                 (letrec ([lab (lambda ignore (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                   (fluid-let ([$fp-offset 0]) letrec-body))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules (,frame-pointer-register)
                [(_ ,frame-pointer-register (op xxx n))
                 (begin
                   (set! $fp-offset (op $fp-offset n))
                   (set! ,frame-pointer-register (op xxx n)))]
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
          (define-syntax locals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax ulocals
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax spills
            (syntax-rules ()
              [(_ (x* ...) body) (let ([x* 0] ...) body)]))
          (define-syntax frame-conflict
            (syntax-rules ()
              [(_ ct body) body]))
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,x))
          ,return-value-register)]
      [(discard-call-live)
       `(let ()
          (import (except scheme letrec set!))
          (define int64-in-range?
            (let ()
              (import scheme)
              (lambda (x)
                (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
          (define handle-overflow
            (let ()
              (import scheme)
              (lambda (x)
                (cond
                  [(not (number? x)) x]
                  [(int64-in-range? x) x]
                  [(not (= x (logand 18446744073709551615 x)))
                   (handle-overflow (logand 18446744073709551615 x))]
                  [(< x 0) (handle-overflow (+ x (expt 2 64)))]
                  [else (handle-overflow (- x (expt 2 64)))]))))
          (define-syntax letrec
            (let ()
              (import scheme)
              (syntax-rules (lambda)
                [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                 (letrec ([lab (lambda () (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                   (fluid-let ([$fp-offset 0]) letrec-body))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules (,frame-pointer-register)
                [(_ ,frame-pointer-register (op xxx n))
                 (begin
                   (set! $fp-offset (op $fp-offset n))
                   (set! ,frame-pointer-register (op xxx n)))]
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
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,x))
          ,return-value-register)]
      [(finalize-locations)
       `(let ()
          (import (except scheme letrec set!))
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
          (define-syntax letrec
            (let ()
              (import scheme)
              (syntax-rules (lambda)
                [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
                 (letrec ([lab (lambda () (fluid-let ([$fp-offset 0]) lambda-body))] ...)
                   (fluid-let ([$fp-offset 0]) letrec-body))])))
          (define-syntax set!
            (let ()
              (import scheme)
              (syntax-rules (,frame-pointer-register)
                [(_ ,frame-pointer-register (op xxx n))
                 (begin
                   (set! $fp-offset (op $fp-offset n))
                   (set! ,frame-pointer-register (op xxx n)))]
                [(_ x expr)
                 (set! x (handle-overflow expr))])))
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,x))
          ,return-value-register)]
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
          (define-syntax return-point
            (syntax-rules ()
              [(_ lab expr)
               (let ([lab (lambda args (void))]) expr)]))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          (call/cc 
            (lambda (k)
              (set! ,return-address-register k)
              ,(rewrite-opnds x)))
          ,return-value-register)]
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
              (set! ,return-address-register k)
              ,(rewrite-opnds x)))
          ,return-value-register)]
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
              (set! ,return-address-register k)
              ,(rewrite-opnds x)))
          ,return-value-register)]
      [else x])))
