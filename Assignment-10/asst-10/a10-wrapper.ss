(define ptr->datum
  (lambda (ptr)
    (define istype?
      (lambda (mask tag x)
        (= (logand x mask) tag)))
    (define tagref
      (lambda (tag disp p)
        (mref p (- disp tag))))
    (let f ([ptr ptr])
      (cond
        [(eqv? ptr $false) #f]
        [(eqv? ptr $true) #t]
        [(eqv? ptr $nil) '()]
        [(eqv? ptr $void) (void)]
        [(istype? mask-fixnum tag-fixnum ptr)
         (ash ptr (- shift-fixnum))]
        [(istype? mask-pair tag-pair ptr)
         (cons (f (tagref tag-pair disp-car ptr))
               (f (tagref tag-pair disp-cdr ptr)))]
        [(istype? mask-vector tag-vector ptr)
         (let ([n (f (tagref tag-vector disp-vector-length ptr))])
           (let ([v (make-vector n)])
             (do ([i 0 (+ i 1)])
                 ((= i n) v)
               (vector-set! v i
                 (f (tagref tag-vector
                            (+ disp-vector-data (ash i align-shift))
                            ptr))))))]
        [else (error 'ptr->datum "can't handle ~s" ptr)]))))

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
      [(source verify-scheme)
       `(let ()
          (import (except scheme * + -))
          (define-who *
            (lambda (x y)
              (import scheme)
              (let ([ans (* x y)])
                (unless (fixnum-range? ans)
                  (error who "result ~s is outside of fixnum range" ans))
                ans)))
          (define-who +
            (lambda (x y)
              (import scheme)
              (let ([ans (+ x y)])
                (unless (fixnum-range? ans)
                  (error who "result ~s is outside of fixnum range" ans))
                ans)))
          (define-who -
            (lambda (x y)
              (import scheme)
              (let ([ans (- x y)])
                (unless (fixnum-range? ans)
                  (error who "result ~s is outside of fixnum range" ans))
                ans)))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          ,x)]
      [(specify-representation)
       `(let ()
          (import (except scheme set!))
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
          (ptr->datum ,x))]
      [(uncover-locals)
       `(let ()
          (import (except scheme set!))
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
          (ptr->datum ,x))]
      [(remove-let verify-uil)
       `(let ()
          (import (except scheme set!))
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
          (ptr->datum ,x))]
      [else (error 'language-wrapper "no wrapper for ~s" pass-name)])))
