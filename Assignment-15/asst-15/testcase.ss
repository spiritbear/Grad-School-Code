(letrec ([merge$4 (lambda (fp.22 ls.16 ls2.15)
                    (if (null? ls.16)
                        ls2.15
                        (if (null? ls2.15)
                            ls.16
                            (if (< (car ls.16) (car ls2.15))
                                (cons
                                  (car ls.16)
                                  ((procedure-code
                                     (procedure-ref fp.22 '0))
                                    (procedure-ref fp.22 '0)
                                    (cdr ls.16)
                                    ls2.15))
                                (cons
                                  (car ls2.15)
                                  ((procedure-code
                                     (procedure-ref fp.22 '0))
                                    (procedure-ref fp.22 '0)
                                    ls.16
                                    (cdr ls2.15)))))))]
         [sort$3 (lambda (fp.21 ls.11)
                   (if (null? ls.11)
                       ls.11
                       (if (null? (cdr ls.11))
                           ls.11
                           (let ([halves.12 ((procedure-code
                                               (procedure-ref fp.21 '0))
                                              (procedure-ref fp.21 '0)
                                              ls.11 '() '() '#t)])
                             (let ([first.14 (car halves.12)]
                                   [second.13 (car (cdr halves.12))])
                               ((procedure-code (procedure-ref fp.21 '1))
                                 (procedure-ref fp.21 '1)
                                 ((procedure-code (procedure-ref fp.21 '2))
                                   (procedure-ref fp.21 '2)
                                   first.14)
                                 ((procedure-code (procedure-ref fp.21 '2))
                                   (procedure-ref fp.21 '2)
                                   second.13)))))))]
         [halves$2 (lambda (fp.20 ls.10 first.9 second.8 first?.7)
                     (if (null? ls.10)
                         (cons first.9 (cons second.8 '()))
                         (if (if (eq? first?.7 '#f) (false) (true))
                             ((procedure-code (procedure-ref fp.20 '0))
                               (procedure-ref fp.20 '0) (cdr ls.10)
                               (cons (car ls.10) first.9) second.8 '#f)
                             ((procedure-code (procedure-ref fp.20 '0))
                               (procedure-ref fp.20 '0) (cdr ls.10) first.9
                               (cons (car ls.10) second.8) '#t))))]
         [pend$1 (lambda (fp.19 ls.6 ls2.5)
                   (if (null? ls.6)
                       ls2.5
                       (cons
                         (car ls.6)
                         ((procedure-code (procedure-ref fp.19 '0))
                           (procedure-ref fp.19 '0)
                           (cdr ls.6)
                           ls2.5))))])
  (let ([u.18 (cons
                '1
                (cons
                  '5
                  (cons '5 (cons '8 (cons '2 (cons '3 (cons '9 '())))))))]
        [u.17 (cons
                '5
                (cons
                  '9
                  (cons '5 (cons '7 (cons '7 (cons '8 (cons '7 '())))))))])
    (let ([merge.4 (make-procedure merge$4 '1)]
          [sort.3 (make-procedure sort$3 '3)]
          [halves.2 (make-procedure halves$2 '1)]
          [pend.1 (make-procedure pend$1 '1)])
      (begin
        (procedure-set! merge.4 '0 merge.4)
        (procedure-set! sort.3 '0 halves.2)
        (procedure-set! sort.3 '1 merge.4)
        (procedure-set! sort.3 '2 sort.3)
        (procedure-set! halves.2 '0 halves.2)
        (procedure-set! pend.1 '0 pend.1)
        ((procedure-code pend.1)
          pend.1
          ((procedure-code sort.3) sort.3 u.18)
          ((procedure-code sort.3) sort.3 u.17))))))

					(let ([t (cons 1 2)] [v (make-vector 2)])
							 (begin
									(vector-set! v 1 t)
									(vector-set! v 2 10)
									v))