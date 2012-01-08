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
      [(source verify-scheme convert-complex-datum convert-assignments
        optimize-direct-call remove-anonymous-lambda sanitize-binding-forms)
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
          ,x)]
      [(uncover-assigned purify-letrec)
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
          (define-syntax assigned
            (syntax-rules ()
              [(_ (var ...) expr) expr]))
          ,x)]
      [(uncover-free)
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
          (define-syntax free
            (syntax-rules ()
              [(_ (var ...) expr) expr]))
          ,x)]
      [(convert-closures optimize-known-call
        analyze-closure-size optimize-free optimize-self-reference)
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
          (define cookie (cons "snicker" "doodle"))
          (define-syntax bind-free
            (lambda (x)
              (syntax-case x ()
                [(_ (cp fv ...) body)
                 (with-syntax ([(i ...) (enumerate #'(fv ...))])
                   #'(let ()
                       (define-syntax fv
                         (identifier-syntax
                           (vector-ref (cp cookie) i)))
                       ...
                       body))])))
          (define fill-closure!
            (lambda (cp . free)
              (let ([env (cp cookie)])
                (for-each
                  (lambda (i x) (vector-set! env i x))
                  (enumerate free)
                  free))))
          (define-syntax closures
            (syntax-rules ()
              [(_ ([name code free ...] ...) body)
               (letrec ([name (let ([env (make-vector (length '(free ...)))])
                                (lambda args
                                  (if (and (= (length args) 1)
                                           (eq? (car args) cookie))
                                      env
                                      (apply code args))))]
                        ...)
                 (fill-closure! name free ...)
                 ...
                 body)]))
          ,x)]
      [(uncover-well-known)
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
          (define cookie (cons "snicker" "doodle"))
          (define-syntax bind-free
            (lambda (x)
              (syntax-case x ()
                [(_ (cp fv ...) body)
                 (with-syntax ([(i ...) (enumerate #'(fv ...))])
                   #'(let ()
                       (define-syntax fv
                         (identifier-syntax
                           (vector-ref (cp cookie) i)))
                       ...
                       body))])))
          (define fill-closure!
            (lambda (cp . free)
              (let ([env (cp cookie)])
                (for-each
                  (lambda (i x) (vector-set! env i x))
                  (enumerate free)
                  free))))
          (define-syntax closures
            (syntax-rules ()
              [(_ ([name code free ...] ...) body)
               (letrec ([name (let ([env (make-vector (length '(free ...)))])
                                (lambda args
                                  (if (and (= (length args) 1)
                                           (eq? (car args) cookie))
                                      env
                                      (apply code args))))]
                        ...)
                 (fill-closure! name free ...)
                 ...
                 body)]))
          (define-syntax well-known
            (syntax-rules ()
              [(_ (wk ...) body) body]))
          ,x)]
      [(introduce-procedure-primitives)
       `(let ()
          (import (except scheme * + - procedure?))
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
          (define-record procedure ((immutable code) (immutable env)) ()
            ([constructor $make-procedure]))
          (define make-procedure
            (lambda (code i)
              ($make-procedure code (make-vector i))))
          (define procedure-ref
            (lambda (cp i)
              (vector-ref (procedure-env cp) i)))
          (define procedure-set!
            (lambda (cp i v)
              (vector-set! (procedure-env cp) i v)))
          ,x)]
      [(lift-letrec)
       `(let ()
          (import (except scheme * + - procedure?))
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
          (define-record procedure ((immutable code) (immutable env)) ()
            ([constructor $make-procedure]))
          (define make-procedure
            (lambda (code i)
              ($make-procedure code (make-vector i))))
          (define procedure-ref
            (lambda (cp i)
              (vector-ref (procedure-env cp) i)))
          (define procedure-set!
            (lambda (cp i v)
              (vector-set! (procedure-env cp) i v)))
          ,x)]
      [(normalize-context verify-a11-output)
       `(let ()
          (import (except scheme * + - procedure?))
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
          (define-record procedure ((immutable code) (immutable env)) ()
            ([constructor $make-procedure]))
          (define make-procedure
            (lambda (code i)
              ($make-procedure code (make-vector i))))
          (define procedure-ref
            (lambda (cp i)
              (vector-ref (procedure-env cp) i)))
          (define procedure-set!
            (lambda (cp i v)
              (vector-set! (procedure-env cp) i v)))
          (define (true) #t)
          (define (false) #f)
          (define (nop) (void))
          ,x)]
      [(specify-representation verify-a10-output)
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
