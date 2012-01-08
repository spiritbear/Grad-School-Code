

;;---------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------

;(define-who uncover-register-conflict
;  (define Triv ---)
;  (define Effect* ---)
;  (define Effect ---)
;  (define Pred ---)
;  (define Tail ---)
;  (define Body
;    (lambda (x)
;      (match x
;        [(locals (,local* ...) 
;           (ulocals (,ulocal* ...)
;             (locate (,home* ...)
;               (frame-conflict ,fv-ct ,tail))))
;         ;; setup the conflict table ct for storing conflicts
;         (let ([ct ---])
;           (let ([live* (Tail tail ct)])
;             `(locals (,local* ...) 
;                (ulocals (,ulocal* ...)
;                  (locate (,home* ...)
;                    (frame-conflict ,fv-ct
;                      (register-conflict ,ct ,tail)))))))]
;        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
;        [,x (error who "invalid Body ~s" x)])))
;  (lambda (x)
;    (match x
;      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
;       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
;      [,x (error who "invalid Program ~s" x)])))
;

;
;(define-who assign-frame
;  (define find-homes ---)
;  (define Body
;    (lambda (body)
;      (match body
;        [(locals (,local* ...)
;           (ulocals (,ulocal* ...)
;             (spills (,spill* ...)
;               (locate (,home* ...)
;                 (frame-conflict ,ct ,tail)))))
;         (let ([home* (find-homes spill* ct home*)])
;           `(locals (,local* ...)
;              (ulocals (,ulocal* ...)
;                (locate (,home* ...)
;                  (frame-conflict ,ct ,tail)))))]
;        [(locate (,home* ...) ,body) `(locate (,home* ...) ,body)]
;        [,body (error who "invalid Body ~s" body)])))
;  (lambda (x)
;    (match x
;      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
;       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
;      [,x (error who "invalid Program ~s" x)])))
;
;(define-who finalize-frame-locations
;  (define Var ---)
;  (define Triv ---)
;  (define Pred ---)
;  (define Effect ---)
;  (define Tail ---)
;  (define Body
;    (lambda (bd)
;      (match bd
;        [(locals (,local* ...)
;           (ulocals (,ulocal* ...)
;             (locate ([,uvar* ,loc*] ...)
;               (frame-conflict ,ct ,[(Tail ---) -> tail]))))
;         `(locals (,local* ...)
;            (ulocals (,ulocal* ...)
;              (locate ([,uvar* ,loc*] ...)
;                (frame-conflict ,ct ,tail))))]
;        [(locate ([,uvar* ,loc*] ...) ,tail) 
;         `(locate ([,uvar* ,loc*] ...) ,tail)]
;        [,bd (error who "invalid Body ~s" bd)])))
;  (lambda (x)
;    (match x
;      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
;       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
;      [,x (error who "invalid Program ~s" x)])))
;

;(let ((new-unspillable-variable (unique-name 't)))
;	(values `(begin 
;			(set! ,new-unspillable-variable ,rhs1)
;			(set! ,new-unspillable-variable (,binop ,new-unspillable-variable ,rhs2))
;			(set! ,lhs ,new-unspillable-variable)) `(,new-local)))

;(define resolve
;	(trace-lambda resolve(lhs live* ct)
;		(cond
;			[(register? lhs) (record-conflict lhs live* ct)]
;			[(uvar? lhs) (update-conflict-table lhs live* ct)])))
;(define record-conflict
;	(trace-lambda rc(lhs live* ct)
;		(cond
;			[(null? live*) '()]
;			[(uvar? (car live*)) 
;					(let* ((var (car live*)) (conflict-entry (assq var ct)))
;						(set-cdr! conflict-entry (set-cons lhs (if (null? (cdr conflict-entry)) '() (cdr conflict-entry))))
;						(record-conflict lhs (cdr live*) ct))]
;			[else (record-conflict lhs (cdr live*) ct)])))
