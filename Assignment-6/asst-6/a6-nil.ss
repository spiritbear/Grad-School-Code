;;; Andy Keep, Kent Dybvig, Nilesh Mahajan
;;; P523
;;; Spring 2009

;;;  verify-scheme accepts a signle value and verifies that the value
;;;  is a valid program in the grammar of the sixth assignment.
;;;
;;;  Grammar for verify-scheme (assignment 6):
;;;
;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Body>)]*) <Body>)
;;;  Body    --> (locals (<uvar>*) <Tail>)
;;;  Tail    --> <Triv>
;;;           |  (binop <Value> <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Tail> <Tail>)
;;;           |  (begin <Effect>* <Tail>)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (<predop> <Value> <Value>)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;  Effect  --> (nop)
;;;           |  (set! <uvar> <Value>)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;  Value   --> <Triv>
;;;           |  (<binop> <Value> <Value>)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;  Triv    --> <uvar> | <int64> | <label>
;;;  
;;;  Where uvar is symbol.n where (n >= 0)
;;;        label is symbol$n where (n >= 0)
;;;        binop is +, -, *, logand, logor, or sra
;;;        predop is <, >, <=, >=, =
;;;
;;;  We still have a couple constraints based on our machine and
;;;  testing framework. Namely, we expect calls target values to
;;;  evaluate to uvars or labels, and we expect computations to be 
;;;  done with uvars or integers.
;;;
;;;  Note that we also expect the sra binop to have a uint6 in the
;;;  second argument.
;;;

(define-who verify-scheme
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))
  (define Triv
    (lambda (label* uvar*)
      (lambda (t)
        (unless (or (label? t) (uvar? t) (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (and (integer? t) (exact? t))
          (unless (int64? t)
            (error who "integer out of 64-bit range ~s" t)))
        (when (uvar? t)
          (unless (memq t uvar*)
            (error who "reference to unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        t)))
  (define Value
    (lambda (label* uvar*)
      (lambda (val)
        (match val
          [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
          [(begin ,[(Effect label* uvar*) -> ef*] ... ,[val]) (void)]
          [(sra ,[(Value label* uvar*) -> x] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))]
          [(,binop ,[(Value label* uvar*) -> x] ,[(Value label* uvar*) -> y])
           (guard (memq binop '(+ - * logand logor sra)))
           (void)]
          [,[(Triv label* uvar*) -> tr] (void)]))))
  (define Effect
    (lambda (label* uvar*)
      (lambda (ef)
        (match ef
          [(nop) (void)]
          [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
          [(begin ,[ef*] ... ,[ef]) (void)]
          [(set! ,var ,[(Value label* uvar*) -> val])
           (unless (memq var uvar*)
             (error who "assignment to unbound var ~s" var))]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Pred
    (lambda (label* uvar*)
      (lambda (pr)
        (match pr
          [(true) (void)]
          [(false) (void)]
          [(if ,[test] ,[conseq] ,[altern]) (void)]
          [(begin ,[(Effect label* uvar*) -> ef*] ... ,[pr]) (void)]
          [(,predop ,[(Value label* uvar*) -> x] ,[(Value label* uvar*) -> y])
           (unless (memq predop '(< > <= >= =))
             (error who "invalid predicate operator ~s" predop))]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Tail
    (lambda (tail label* uvar*)
      (match tail
        [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern]) (void)]
        [(begin ,[(Effect label* uvar*) -> ef*] ... ,[tail]) (void)]
        [(sra ,[(Value label* uvar*) -> x] ,y)
         (unless (uint6? y)
           (error who "invalid sra operand ~s" y))]
        [(,binop ,[(Value label* uvar*) -> x] ,[(Value label* uvar*) -> y])
         (guard (memq binop '(+ - * logand logor sra)))
         (void)]
        [(,[(Value label* uvar*) -> rator] 
           ,[(Value label* uvar*) -> rand*] ...)
         (void)]
        [,[(Triv label* uvar*) -> triv] (void)])))
  (define Body
    (lambda (label*)
      (lambda (bd fml*)
        (match bd
          [(locals (,local* ...) ,tail)
           (let ([uvar* `(,fml* ... ,local* ...)])
             (verify-x-list uvar* uvar? 'uvar)
             (Tail tail label* uvar*))]
          [,bd (error who "invalid Body ~s" bd)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
       (verify-x-list label* label? 'label)
       (map (lambda (fml*) (verify-x-list fml* uvar? 'formal)) fml**)
       (for-each (Body label*) bd* fml**)
       ((Body label*) bd '())]
      [,x (error who "invalid Program ~s" x)])
    x))


;;; Program	-->	(letrec ([label (lambda (uvar*) Body)]*) Body)
;;; Body	-->	(locals (uvar*) Tail)
;;; Tail	-->	Triv
;;;	|	(binop Value Value)
;;;	|	(Value Value*)
;;;	|	(if Pred Tail Tail)
;;;	|	(begin Effect* Tail)
;;; Pred	-->	(true)
;;;	|	(false)
;;;	|	(relop Value Value)
;;;	|	(if Pred Pred Pred)
;;;	|	(begin Effect* Pred)
;;; Effect	-->	(nop)
;;;	|	(set! uvar Value)
;;;	|	(if Pred Effect Effect)
;;;	|	(begin Effect* Effect)
;;; Value	-->	Triv
;;;	|	(binop Value Value)
;;;	|	(if Pred Value Value)
;;;	|	(begin Effect* Value)
;;; Triv	-->	uvar | int | label
(define-who remove-complex-opera*
  (define (Body bd)
    (define new-local* '())
    (define (new-t)
      (let ([t (unique-name 't)])
        (set! new-local* (cons t new-local*))
        t))
    ;;; returns true if the input val is not a Triv
    (define nontrivial?
      (lambda (x)
        (not (or (uvar? x) (label? x) (integer? x)))))
    ;;; handles the common code for binop, predop
    (define (binop-handler binop x y)
      (cond
        ((and (nontrivial? x) (nontrivial? y))
         (let ([t1 (new-t)] [t2 (new-t)])
           (make-begin `((set! ,t1 ,x) (set! ,t2 ,y) (,binop ,t1 ,t2)))))
        ((nontrivial? x)
         (let ([t (new-t)])
           (make-begin `((set! ,t ,x) (,binop ,t ,y)))))
        ((nontrivial? y)
         (let ([t (new-t)])
           (make-begin `((set! ,t ,y) (,binop ,x ,t)))))
        (else `(,binop ,x ,y))))
    (define (Value val)
      (match val
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[val]) `(begin ,ef* ... ,val)]
        [(,binop ,[x] ,[y]) (binop-handler binop x y)]
        [,tr tr]))
    (define (Effect ef)
      (match ef
          [(nop) `(nop)]
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[ef*] ...) `(begin ,ef* ...)]
          [(set! ,var ,[Value -> val]) `(set! ,var ,val)]
          [,ef (error who "invalid Effect ~s" ef)]))
    (define (Pred pr)
      (match pr
          [(true) `(true)]
          [(false) `(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
          [(,predop ,[Value -> x] ,[Value -> y]) (binop-handler predop x y)]
          [,pr (error who "invalid Pred ~s" pr)]))
    (define (handle-value x)
      (cond
        ((nontrivial? x)
         (let ([t (new-t)])
           (make-begin `((set! ,t ,x) ,t))))
        (else x)))
    (define (Tail tail)
      (match tail
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
        [(,binop ,[Value -> x] ,[Value -> y]) (binop-handler binop x y)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
         (append `(,(handle-value rator)) (map handle-value rand*))]
        [,triv triv]))
    (match bd
      [(locals (,local* ...) ,[Tail -> tail])
       `(locals (,local* ... ,new-local* ...) ,tail)]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...) 
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))

(define-who flatten-set!
  #;(define (Triv t) ---)
  #;(define (Value val) ---)
  (define (propagate-set! expr var)
    (match expr
      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
      [(begin ,[Effect -> ef*] ... ,[val]) `(begin ,ef* ... ,val)]
      [(,binop ,x ,y) `(set! ,var (,binop ,x ,y))]
      [,tr `(set! ,var ,tr)]))
  (define (Effect ef)
    (match ef
      [(nop) `(nop)]
      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
      [(begin ,[ef*] ...) (make-begin ef*)]
      [(set! ,var ,val) (propagate-set! val var)]
      [,ef (error who "invalid Effect ~s" ef)]))
  (define (Pred pr)
    (match pr
      [(true) `(true)]
      [(false) `(false)]
      [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
      [(,predop ,x ,y) `(,predop ,x ,y)]
      [,pr (error who "invalid Pred ~s" pr)]))
  (define (Tail tail)
    (match tail
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
        [(,binop ,x ,y) `(,binop ,x ,y)]
        [(,rator ,rand* ...) `(,rator ,rand* ...)]
        [,triv triv]))
  (define (Body bd)
    (match bd
      [(locals (,uvar* ...) ,[Tail -> tail]) `(locals (,uvar* ...) ,tail)]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...)
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))

;;; Program	-->	(letrec ([label (lambda (uvar*) Body)]*) Body)
;;; Body	-->	(locals (uvar*) Tail)
;;; Tail	-->	Triv
;;;	|	(binop Triv Triv)
;;;	|	(Triv Triv*)
;;;	|	(if Pred Tail Tail)
;;;	|	(begin Effect* Tail)
;;; Pred	-->	(true)
;;;	|	(false)
;;;	|	(relop Triv Triv)
;;;	|	(if Pred Pred Pred)
;;;	|	(begin Effect* Pred)
;;; Effect	-->	(nop)
;;;	|	(set! uvar Triv)
;;;	|	(set! uvar (binop Triv Triv))
;;;	|	(if Pred Effect Effect)
;;;	|	(begin Effect* Effect)
;;; Triv	-->	uvar | int | label
(define-who impose-calling-conventions
  #;(define (Triv t) ---)
  (define (Effect ef)
    (match ef
      [(nop) `(nop)]
      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
      [(begin ,[ef*] ...) `(begin ,ef* ...)]
      [(set! ,var ,val) `(set! ,var ,val)]
      [,ef (error who "invalid Effect ~s" ef)]))
  (define (Pred pr)
    (match pr
      [(true) `(true)]
      [(false) `(false)]
      [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
      [(begin ,[Effect -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
      [(,predop ,x ,y) `(,predop ,x ,y)]
      [,pr (error who "invalid Pred ~s" pr)]))
  (define (generate-set! dest src) `(set! ,dest ,src))
  (define (generate-frame-var-list count)
    (if (= count 0)
        `()
        (let ([newcount (- count 1)])
          (cons (index->frame-var newcount) (generate-frame-var-list newcount)))))
  (define (Tail tail rp)
    (match tail
      [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
      [(begin ,[Effect -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
      [(,proc ,args* ...)
       (make-begin
          (append
             (append
                (if (> (length args*) (length parameter-registers))
                    (let ([argstail (list-tail args* (length parameter-registers))])
                      (map generate-set! (generate-frame-var-list (length argstail)) argstail))
                    '())
             (let ([reg* (if (< (length args*) (length parameter-registers))
                             (list-head parameter-registers (length args*))
                             parameter-registers)])
               (map generate-set! reg* (list-head args* (length reg*)))))
          `((set! ,return-value-register ,rp))
          `((,proc ,frame-pointer-register ,return-value-register ,args*))))]
      [(,binop ,x ,y)
       `(begin
          (set! ,return-value-register (,binop ,x ,y))
          (,rp ,frame-pointer-register ,return-value-register))]
      [,triv
       `(begin
          (set! ,return-value-register ,triv)
          (,rp ,frame-pointer-register ,return-value-register))]))
  (define (Body bd fml*)
    (match bd
      [(locals (,locals* ...) ,tail)
       (let ([rp (unique-name 'rp)] [newlocals* (append locals* fml*)])
         `(locals (,newlocals* ,rp)
            ,(make-begin
              (append
                `((set! ,rp ,return-address-register))
                (append
                 (let ([reg* (if (< (length newlocals*) (length parameter-registers))
                             (list-head parameter-registers (length newlocals*))
                             parameter-registers)])
                   (map generate-set! (list-head newlocals* (length reg*)) reg*))
                 (if (> (length newlocals*) (length parameter-registers))
                     (let ([newlocalstail (list-tail newlocals* (length parameter-registers))])
                       (map generate-set! newlocalstail (generate-frame-var-list (length newlocalstail))))
                     '()))
                (Tail tail rp)))))]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)
       (let ([bd* (map Body bd* fml**)] [bd (Body bd '())])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))]
      [,x (error who "invalid Program ~s" x)])))
