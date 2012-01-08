;;; driver.ss
;;; Copyright (c) 2001-2009 R. Kent Dybvig, Oscar Waddell, and Daniel P. Friedman

;;; This file contains driver code for running p423 compilers through
;;; their paces.

;;; Overview:
;;;
;;; The driver runs each pass of the compiler and tests its output to
;;; make sure it returns the same answer as the source program.  It can
;;; test one test at a time or all of the tests in a list of tests.

;;; Basic Usage:
;;;
;;; To use the driver for Assignment 1, create a file containing:
;;;
;;; (eval-when (compile load eval)
;;;   (optimize-level 2)
;;;   (case-sensitive #t)
;;; )
;;;
;;; (load "match.ss")
;;; (load "helpers.ss")
;;; (load "fmts.pretty")     ; inform pretty-print about new forms
;;; (load "driver.ss")
;;;
;;; (load "a1.ss")
;;; (load "a1-wrapper.ss")   ; defines syntactic forms and procedures
;;;                          ; needed to output of each pass
;;; (compiler-passes '(
;;;   verify-scheme
;;;   generate-x86-64
;;; ))
;;;
;;; (load "tests1.ss")
;;;
;;; Load this into Chez Scheme, and type (test-all) to run all of the
;;; valid tests, or (test-all-invalid) to run all of the invalid tests.
;;; Use test-one or test-one-invalid to run an individual test.
;;;
;;; To use the driver for later assignments, change the last several
;;; lines to include the appropriate files and list the current passes.


;;; Detailed description of the driver routines:

;;;  (compiler-passes '(<spec> ...))
;;;    Tells the driver which compiler passes to run.  This should be
;;;    set once at the top of the file containing your compiler.  <spec>
;;;    may be one of the following:
;;;
;;;    pass-name
;;;      run the pass named by pass-name.  Each pass is fed the output
;;;      of the preceding pass, except the first, which is fed the
;;;      original input.
;;;
;;;    (iterate <spec> ...)
;;;      run <spec> ... repeatedly
;;;
;;;    (break when predicate?)
;;;      break out of an iteration, or if not in an iteration, exit
;;;      from the driver, if predicate? returns true when passed the
;;;      the output of the pass just run.
;;;
;;;    (break unless predicate?)
;;;      break out of an iteration, or if not in an iteration, exit
;;;      from the driver, if predicate? returns false when passed the
;;;      the output of the pass just run.
;;;
;;;    Example:
;;;      (compiler-passes '(a (iterate b (break when c?) d) e))
;;;      runs a on the input and feeds its output to b.  If c? applied
;;;      to the output of b is true, feeds the output of b to e;
;;;      otherwise, feeds the output of b to d and the output of d to
;;;      b, then tests c? on the output of b again, and so on.

;;;  (language-wrapper procedure)
;;;    Tells the driver a procedure it should call to wrap each expression
;;;    before evaluation.  Defaults to the procedure value of:
;;;      (lambda (x) `(let () (import scheme) ,x))

;;;  (test-one-invalid '<program>)
;;;  (test-one-invalid '<program> <verbose?>)
;;;    Runs the first pass of the compiler, assumed to be a verifier,
;;;    displays the error message it produces or, if it doesn't signal
;;;    an error, raises one of its own to indicate failure.
;;;
;;;    If <verbose?> is false, less diagnostic information is printed.
;;;    <verbose?> defaults to #t.

;;;  (test-all-invalid)
;;;  (test-all-invalid <verbose?>)
;;;    Runs test-one-invalid in order on the tests in the list bound
;;;    to the variable invalid-tests, stopping if any test fails.  Usually
;;;    used with tests.ss loaded.
;;;
;;;    If <verbose?> is false, less diagnostic information is printed.
;;;    <verbose?> defaults to #t.

;;;  (test-last-invalid)
;;;  (test-last-invalid <verbose?>)
;;;    Like test-one-invalid, except <program> is the last input to
;;;    test-one-invalid or the last test run by test-all-invalid.
;;;
;;;    If <verbose?> is false, less diagnostic information is printed.
;;;    <verbose?> defaults to #t.

;;;  (test-one '<program>)
;;;  (test-one '<program> <emit?>)
;;;  (test-one '<program> <emit?> <verbose?>)
;;;    Compile and test <program>.  test-one first computes the correct
;;;    answer by evaluting it with Chez Scheme's interpreter.  It then
;;;    passes the program to the first pass, evalutes the resulting
;;;    program, and compares the value with the correct answer.  If it
;;;    compares equal?, it goes on to the next pass, passing it the
;;;    program returned by the first.  It repeats this process until
;;;    all passes have been run, then exits.
;;;
;;;    If a pass is a code-generation pass (generate-C-code or
;;;    generate-Sparc-code), its output is compiled or assembled, linked
;;;    with the run-time code in startup.c, and run.  The output is read
;;;    back in and compared with the correct answer.  The next pass, if
;;;    any, is run on the output of the preceding pass.
;;;
;;;    If the result of evaluating the program returned by any of the
;;;    passes does not equal the correct answer or if the evaluation
;;;    causes an error, test-one stops, prints the input to the pass,
;;;    prints the output to the pass, and displays the error message.
;;;    If a problem occurs while running a pass, test-one stops, prints
;;;    the input to the pass, and displays the error message.
;;;
;;;    If <emit?> is false, test-one doesn't build or run the code
;;;    generated by the code generation pass(es).  This is useful when
;;;    trying to test changes to earlier parts of the compiler, since
;;;    the process of building an executable file and running it is
;;;    rather slow.  <emit?> defaults to #t.
;;;
;;;    If <verbose?> is false, less diagnostic information is printed.
;;;    <verbose?> defaults to #t.

;;;  (test-all)
;;;  (test-all <emit?>)
;;;  (test-all <emit?> <verbose?>)
;;;    Runs test-one in order on the tests in the list bound to the
;;;    variable tests, stopping if any test fails.  Usually used with
;;;    tests.ss loaded.  If <verbose?>  is true, test-all prints each
;;;    test before it is compiled and run.  Passes <emit?>  and <verbose?>
;;;    along to test-one.  <emit?>  and <verbose?> both default to #t.

;;;  (test-last)
;;;  (test-last <emit?>)
;;;  (test-last <emit?> <verbose?>)
;;;    Like test-one, except <program> is the last input to test-one
;;;    or the last test run by test-all or analyze-all.

;;;  (analyze-all)
;;;  (analyze-all <emit?>)
;;;    Runs test-one in order on the tests in the list bound to the
;;;    variable tests.  Does not stop for when a test fails.  Usually
;;;    used with tests.ss loaded.  Passes <emit?> along to test-one,
;;;    with <verbose?> false.  <emit?> defaults to #t.  Prints a "." for
;;;    each test before running it, but doesn't print the test itself.
;;;    Prints a summary after all tests have been run.

;;;  (tracer #t)
;;;    Causes test-one to print the output of each pass.
;;;
;;;  (tracer '<pass-name>)
;;;    Causes test-one to print only the output of the specified
;;;    pass.
;;;
;;;  (tracer '(<pass-name> ...))
;;;    Causes test-one to print only the output of the specified
;;;    passes.  (tracer '()) or (tracer #f) disables tracing.

;;;  (suppress-language-definition #t)
;;;    Causes the language definition part of an intermediate language
;;;    program to be omitted from tracer output, i.e., if the output
;;;    appears as (let () (begin <definitions> ...) expr), only expr
;;;    is printed.
;;;
;;;  (suppress-language-definition #f)
;;;    Disables suppression of the language definition.

;;;  (starting-pass '<pass-name>)
;;;    Causes driver to start running each test from (first run of)
;;;    <pass-name>.  Usually used with test-one, but can be used with
;;;    test-all and analyze-all if the variable tests is bound to a
;;;    list of valid inputs to <pass-name>.  also respected by
;;;    test-last.
;;;
;;;  (starting-pass #f)
;;;    Resets starting pass to original first pass.

;;;  (game-eval)
;;;  (game-eval <proc>)
;;;    game-eval is a parameter that determines the evaluator used to
;;;    evaluate the output of each nongenerator pass.  When called without
;;;    arguments, it returns the current game evaluator.  When called
;;;    with one argument, <proc>, it sets the game evaluator to <proc>.
;;;    The initial game evaluator is interpret, which provides quick
;;;    turnaround for small test cases.  Set it to compile to get better
;;;    error messages and inspector information.

;;;  (print-file '<pathname>)
;;;    Prints the contents of the file specified by <pathname> to the
;;;    current output port.

;;;  (trusted-passes)
;;;  (trusted-passes '(<pass-name> ...))
;;;    Without arguments, returns a list naming the trusted passes, i.e.,
;;;    those whose output is not to be compared against the original input.
;;;    Otherwise, sets the list of trusted passes.  (trusted-passes #t)
;;;    is short-hand for trusting all passes, and (trusted-passes #f)
;;;    has the same effect as (trusted-passes '()).

;;;  (check-final-output-only)
;;;  (check-final-output-only <boolean>)
;;;    check-final-output is a parameter.  If set to true, the value
;;;    of the final output of the compiler is compared against the value
;;;    of the original input, and the normal checking of each intermediate
;;;    program is suppressed, i.e., all passes are considered trusted.

;;;  (timed-passes)
;;;  (timed-passes '(<pass-name> ...))
;;;    Without arguments, returns a list naming the timed passes, i.e.,
;;;    those for which compile times are displayed.  Otherwise, sets the
;;;    list of timed passes.  (timed-passes #t) is short-hand for timing
;;;    all passes, and (timed-passes #f) has the same effect as
;;;    (trusted-passes '()).

(module (language-wrapper tracer suppress-language-definition game-eval analyze-all
         $analyze test-all test-one test-last trusted-passes print-file
         starting-pass compiler-passes timed-passes check-final-output-only
         test-one-invalid test-last-invalid test-all-invalid)
(define test-ordinal #f)
(import scheme)

(define compiler-passes
  (let ([passes #f])
    (case-lambda
      [() (or passes
             ; backward compatibility:
              (if (top-level-bound? 'pass-names)
                  (let ([p pass-names])
                    (unless (valid-passes? p)
                      (error 'pass-names "invalid pass-names value ~s" p))
                    p)
                  '()))]
      [(p)
       (unless (valid-passes? p)
         (error 'compiler-passes "invalid pass list ~s" p))
       (set! passes p)])))

(define valid-passes?
  (lambda (p)
    (define ipass?
      (lambda (p)
        (match p
          [(break when ,pred) (guard (symbol? pred)) #t]
          [(break unless ,pred) (guard (symbol? pred)) #t]
          [,p (pass? p)])))
    (define pass?
      (lambda (p)
        (match p
          [(iterate ,p ...) (andmap ipass? `(,p ...))]
          [,p (guard (symbol? p)) #t]
          [,p #f])))
    (match p
      [(,p ...) (andmap pass? `(,p ...))]
      [,p #f])))

(define all-pass-names
  (lambda ()
    (define ipass
      (lambda (p)
        (match p
          [(break when ,pred) (guard (symbol? pred)) '()]
          [(break unless ,pred) (guard (symbol? pred)) '()]
          [,p (pass p)])))
    (define pass
      (lambda (p)
        (match p
          [(iterate ,[ipass -> p*] ...) (apply append p*)]
          [,p (guard (symbol? p)) (list p)]
          [,p #f])))
    (match (compiler-passes)
      [(,[pass -> p*] ...) (apply append p*)])))

(define language-wrapper
  (make-parameter (lambda (x) `(let () (import scheme) ,x))
    (lambda (x)
      (unless (procedure? x)
        (error 'language-wrapper "~s is not a procedure" x))
      x)))

(define suppress-language-definition (make-parameter #f))

(define tracer-print
  (lambda (x)
    (pretty-print
      (if (suppress-language-definition)
          (match x
            [(let () ,ld ,x) `(let () <lang-defn> ,x)]
            [(let () ,ld ,gd ,x) `(let () <lang-defn> ,gd ,x)]
            [,x x])
          x))))

(define tracer
  (let ([trace-list '()])
    (case-lambda
      [() trace-list]
      [(x)
       (set! trace-list
         (cond
           [(eq? x #t) (all-pass-names)]
           [(eq? x #f) '()]
           [(and (symbol? x) (memq x (all-pass-names))) (list x)]
           [(and (list? x) (andmap (lambda (x) (memq x (all-pass-names))) x)) x]
           [else (error 'tracer "invalid argument ~s" x)]))])))

(define timed-passes
  (make-parameter
    '()
    (lambda (x)
      (cond
        [(eq? x #t) (all-pass-names)]
        [(eq? x #f) '()]
        [(and (list? x) (andmap (lambda (x) (memq x (all-pass-names))) x)) x]
        [else (error 'timed-passes "invalid pass names ~s"
                (if (list? x) (difference x (all-pass-names)) x))]))))

(define trusted-passes
  (make-parameter
    '()
    (lambda (x)
      (cond
        [(eq? x #t) (all-pass-names)]
        [(eq? x #f) '()]
        [(and (list? x) (andmap (lambda (x) (memq x (all-pass-names))) x)) x]
        [else (error 'trusted-passes "invalid pass names ~s"
                (if (list? x) (difference x (all-pass-names)) x))]))))

(define check-final-output-only (make-parameter #f))

(define game-eval
  (make-parameter interpret
    (lambda (x)
      (unless (procedure? x)
        (error 'game-eval "~s is not a procedure" x))
      x)))

(define starting-pass
  (make-parameter #f
    (lambda (p)
      (unless (or (not p) (memq p (all-pass-names)))
        (error 'starting-pass "unrecognized pass ~s" p))
      p)))

(define analyze-all
  (case-lambda
    [() (analyze-all #t)]
    [(emit?)
     (let-values ([(passed total) ($analyze emit? #t)])
       (printf "~%~s of ~s tests passed~%" passed total))]))

(define $analyze
  (lambda (emit? echo?)
    (define mod 72)
    (let f ([tests tests] [n 0] [passed 0] [m mod])
      (if (null? tests)
          (values passed n)
          (begin
            (if (call/cc
                  (lambda (k)
                    (parameterize ([reset-handler (lambda () (k #f))])
                      ($test-one (car tests) emit? #f n)
                      #t)))
                (begin
                  (when echo?
                    (when (= m mod) (newline))
                    (write-char #\.)
                    (flush-output-port))
                  (f (cdr tests) (+ n 1) (+ passed 1) (+ (modulo m mod) 1)))
                (f (cdr tests) (+ n 1) passed mod)))))))

(define test-all
  (case-lambda
    [() (test-all #t #t)]
    [(emit?) (test-all emit? #t)]
    [(emit? verbose?)
     (let f ([tests tests] [n 0])
       (unless (null? tests)
         (when verbose?
           (let ([s (format "~s: " n)])
             (display s)
             (parameterize ([pretty-initial-indent (string-length s)])
               (pretty-print (car tests)))))
         ($test-one (car tests) emit? verbose? n)
         (f (cdr tests) (+ n 1))))]))

(define print-file
  (lambda (path)
    (with-input-from-file path
      (rec f
        (lambda ()
          (unless (eof-object? (peek-char))
            (write-char (read-char))
            (f)))))))

(define *last-input-expr*)

(define test-one-invalid
  (case-lambda
    [(expr) ($test-one-invalid expr #t #f)]
    [(expr verbose?) ($test-one-invalid expr verbose? #f)]))

(define test-last-invalid
  (case-lambda
    [() ($test-one-invalid *last-input-expr* #t #f)]
    [(verbose?) ($test-one-invalid *last-input-expr* verbose? #f)]))

(define test-all-invalid
  (case-lambda
    [() (test-all-invalid #t)]
    [(verbose?)
     (let f ([tests invalid-tests] [n 0])
       (unless (null? tests)
         (when verbose?
           (let ([s (format "~s: " n)])
             (display s)
             (parameterize ([pretty-initial-indent (string-length s)])
               (pretty-print (car tests)))))
         ($test-one-invalid (car tests) verbose? n)
         (f (cdr tests) (+ n 1))))]))

(define $test-one-invalid
  (lambda (expr verbose? ordinal)
    (let ([verifier (car (compiler-passes))])
      (set! *last-input-expr* expr)
      ((call/cc
         (lambda (k)
           (parameterize ([error-handler
                           (lambda (who msg . args)
                             (parameterize ([print-level 3] [print-length 6])
                               (k (lambda ()
                                    (printf "~@[~a~]: ~?.\n"
                                      (and who (not (eqv? who "")) who)
                                      msg args)))))])
             ((eval verifier) expr)
             (lambda ()
               (if ordinal
                   (error #f "no error from ~s on test ~s" verifier ordinal)
                   (error #f "no error from ~s" verifier))))))))))

(define test-one
  (case-lambda
    [(expr) ($test-one expr #t #t #f)]
    [(expr emit?) ($test-one expr emit? #t #f)]
    [(expr emit? verbose?) ($test-one expr emit? verbose? #f)]))

(define test-last
  (case-lambda
    [() ($test-one *last-input-expr* #t #t #f)]
    [(emit?) ($test-one *last-input-expr* emit? #t #f)]
    [(emit? verbose?) ($test-one *last-input-expr* emit? verbose? #f)]))

(define fmt
  (lambda (x n)
    (define digit
      (lambda (d)
        (string-ref "000123456789" (+ d 2))))
    (let ([x (exact->inexact x)])
      (let ([ls (#%\#flonum->digits x 10 'absolute (- n))])
        (let ([s (car ls)] [e (cadr ls)] [digits (cddr ls)])
          (let ([p (open-output-string)])
            (when (= s -1) (write-char #\- p))
            (when (< e 0) (write-char #\0 p))
            (when (< e -1)
              (write-char #\. p)
              (display (make-string (min n (- -1 e)) #\0) p))
            (let f ([digits digits] [e e])
              (when (>= e (- n))
                (when (= e -1) (write-char #\. p))
                (write-char (digit (car digits)) p)
                (f (cdr digits) (- e 1))))
            (get-output-string p)))))))

(define-syntax time-it
  (syntax-rules ()
    [(_ e1 e2 ...)
     (let ([before (statistics)])
       (let ([v (begin e1 e2 ...)])
         (let ([elapsed (sstats-difference (statistics) before)])
           (values
             v
             (format "~asec, ~amb"
               (fmt (/ (sstats-cpu elapsed) 1000) 2)
               (fmt (/ (sstats-bytes elapsed) (expt 2 20)) 2))))))]))

(define $test-one
  (lambda (original-input-expr emit? verbose? ordinal)
   ; caution: this doesn't stop error from happening, just allows
   ; something to be done on the way down
    (define-syntax on-error
      (syntax-rules ()
        [(_ e0 e1 e2 ...)
         (parameterize ([error-handler
                         (let ([eh (error-handler)])
                           (lambda args
                             (parameterize ([current-output-port
                                             (console-output-port)])
                               e0)
                             (apply eh args)))])
           e1 e2 ...)]))
    (define (eval-it pass-name x)
      (reset-machine-state!)
      ((game-eval) ((language-wrapper) pass-name x)))
    (define adjust-unc!
      (lambda (x)
        (unique-name-count
          (let f ([x x])
            (cond
              [(and (symbol? x) (extract-suffix x)) => string->number]
              [(pair? x) (max (f (car x)) (f (cdr x)))]
              [(vector? x) (f (vector->list x))]
              [else 0])))))

    (adjust-unc! original-input-expr)
    (set! *last-input-expr* original-input-expr)
    (let ([answer (delay (eval-it 'source original-input-expr))])
      (define check-eval
        (lambda (pass-name input-expr output-expr)
          (on-error
            (when verbose?
              (unless (eq? input-expr output-expr)
                (printf "~%~s input:~%" pass-name)
                (pretty-print input-expr))
              (printf "========~%~s output:~%" pass-name)
              (pretty-print output-expr))
            (let ([t (parameterize ([run-cp0 (lambda (cp0 x) x)])
                       (on-error
                         (if ordinal
                             (printf "~%Error occurred running output of pass ~s on test ~s"
                               pass-name ordinal)
                             (printf "~%Error occurred running output of pass ~s" pass-name))
                         (eval-it pass-name output-expr)))])
              (unless (equal? t (force answer))
                (if ordinal
                    (error #f
                      "evaluating output of ~s for test ~s produces ~s, should have been ~s"
                      pass-name ordinal t (force answer))
                    (error #f
                      "evaluating output of ~s produces ~s, should have been ~s"
                      pass-name t (force answer))))))))
      (define check-build-eval
        (lambda (pass-name input-expr output-string)
          (on-error
            (begin
              (if ordinal
                  (printf "~%Error occurred while running code generated by ~s for test ~s~%"
                    pass-name ordinal)
                  (printf "~%Error occurred while running code generated by ~s~%"
                    pass-name))
              (when verbose?
                (printf "~s input:~%" pass-name)
                (pretty-print input-expr)
                (printf "========~%~s output:~%" pass-name)
                (display-string output-string)))
            (let ([t (build-and-run input-expr output-string)])
              (unless (equal? t (force answer))
                (if ordinal
                    (error #f
                      "evaluating output of ~s for test ~s produces ~s, should have been ~s"
                      pass-name ordinal t (force answer))
                    (error #f
                      "evaluating output of ~s produces ~s, should have been ~s"
                      pass-name t (force answer))))))))
      (define run-pass
        (lambda (input-expr pass-name)
          (when (memq pass-name (tracer)) (printf "~%~s:~%" pass-name))
          (let ([pass (eval pass-name)])
            (case pass-name
              [(generate-x86-64)
               (let ([output-string
                      (on-error
                        (begin
                          (if ordinal
                              (printf "~%Error occurred within pass ~s on test ~s" pass-name ordinal)
                              (printf "~%Error occurred within pass ~s" pass-name))
                          (when verbose?
                            (printf "~%~s input:~%" pass-name)
                            (pretty-print input-expr)))
                        (with-output-to-string
                          (lambda ()
                            (if (memq pass-name (timed-passes))
                                (begin
                                  (printf "~s: " pass-name)
                                  (flush-output-port)
                                  (let-values ([(v t) (time-it (pass input-expr))])
                                    (printf "~a\n" t)
                                    v))
                                (pass input-expr)))))])
                 (when emit? (check-build-eval pass-name input-expr output-string))
                 (when (memq pass-name (tracer)) (display-string output-string))
                 input-expr)]
              [else
               (let ([output-expr
                      (on-error
                        (begin
                          (if ordinal
                              (printf "~%Error occurred within pass ~s on test ~s" pass-name ordinal)
                              (printf "~%Error occurred within pass ~s" pass-name))
                          (when verbose?
                            (printf "~%~s input:~%" pass-name)
                            (pretty-print input-expr)))
                        (if (memq pass-name (timed-passes))
                            (begin
                              (printf "~s: " pass-name)
                              (flush-output-port)
                              (let-values ([(v t) (time-it (pass input-expr))])
                                (printf "~a\n" t)
                                v))
                            (pass input-expr)))])
                 (unless (or (check-final-output-only)
                             (memq pass-name (trusted-passes)))
                   (check-eval pass-name input-expr output-expr))
                 (when (memq pass-name (tracer))
                   (tracer-print output-expr))
                 output-expr)]))))
      (define run
        (lambda (input-expr passes)
          (define exit
            (lambda (input-expr sp)
              (if sp
                  (error 'driver "starting pass ~s not found" sp)
                  (when (check-final-output-only)
                    (check-eval 'final-output input-expr input-expr)))))
          (let run ([input-expr input-expr]
                    [passes passes]
                    [sp (starting-pass)]
                    [continue exit]
                    [break exit])
            (if (null? passes)
                (continue input-expr sp)
                (match (car passes)
                  [(break when ,pred)
                   (if (and (not sp) ((eval pred) input-expr))
                       (break input-expr sp)
                       (run input-expr (cdr passes) sp continue break))]
                  [(break unless ,pred)
                   (if (and (not sp) (not ((eval pred) input-expr)))
                       (break input-expr sp)
                       (run input-expr (cdr passes) sp continue break))]
                  [(iterate ,ipass* ...)
                   (let next-iter ([input-expr input-expr] [sp sp])
                     (run input-expr
                          ipass*
                          sp
                          (lambda (input-expr sp)
                            (if sp
                                (run input-expr (cdr passes) sp continue break)
                                (next-iter input-expr sp)))
                          (lambda (input-expr sp)
                            (run input-expr (cdr passes) sp continue break))))]
                  [,pass-name
                   (guard (symbol? pass-name))
                   (if (or (not sp) (eq? pass-name sp))
                       (run (run-pass input-expr pass-name) (cdr passes) #f continue break)
                       (begin
                         (when (eq? pass-name 'rename-var) (adjust-unc! input-expr))
                         (run input-expr (cdr passes) sp continue break)))])))))
      (if (equal? (timed-passes) (all-pass-names))
          (let-values ([(v t) (time-it (run original-input-expr (compiler-passes)))])
            (printf "overall: ~a\n" t)
            v)
            (run original-input-expr (compiler-passes))))))
)
