;Shiv Indap
;sindap
;Assignment 1

(load "match.ss")

(optimize-level 2)

;Program	---->	(begin Statement+)
;Statement	---->	(set! Var1 int64)
;								|	(set! Var1 Var2)
;								|	(set! Var1 (Binop Var1 int32))
;								|	(set! Var1 (Binop Var1 Var2))
;Var	---->	rax | rcx | rdx | rbx | rbp | rsi | rdi
;								|	r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
;Binop	---->	+ | - | *

;Determines wether an integer is an int32
(define int32?
	(lambda (x)
		(and (integer? x) (>= x (- 0 (expt 2 31))) (<= x (- (expt 2 31) 1)))))  

;Determines wether an integer is an int64
(define int64?
	(lambda (x)
		(and (integer? x) (>= x (- 0 (expt 2 63))) (<= x (- (expt 2 63) 1)))))  


(define verify-scheme
	(lambda (x)
		;Prog determines if the program obeys the incomming syntax
		(define Prog
			(lambda (x)
				(match x
					[(begin ,stmt ,stmt* ...)
						(Statement stmt)
						(if (not (null? stmt*)) (Prog `(begin ,stmt* ...)))] ;Maybe a better way of doing this, I added an explicit Begin
					[,other (error 'parse "invalid program ~s" other)])))
		;statement is a function that determines wether x is a valid statement as specified by the syntax rules
		(define Statement
			(lambda (x)
				(match x
					[(set! ,var1 ,in64) (guard (int64? in64)) (Var var1)]
					[(set! ,var1 ,var2) (guard (symbol? var2)) (Var var1) (Var var2)]
					[(set! ,var1 (,op ,var1 ,in32)) (guard (int32? in32)) (Var var1) (Binop op)]
					[(set! ,var1 (,op ,var1 ,var2)) (guard (symbol? var2)) (Var var1) (Var var2) (Binop op)]
					[,other (error 'parse "invalid statement ~s" other)])))
		;Binop Determines if x is a valid Binary Operation specified by program syntax
		(define Binop
			(lambda (x)
				(match x
					[+ '+]
					[- '-]
					[* '*]
					[,other (error 'parse "Invalid Operator ~s" other)])))
		;Var Determines if x is a valid register 
		(define Var
			(lambda (x)
				(match x
					[r8 'r8]
					[r9 'r9]
					[r10 'r10]
					[r11 'r11]
					[r12 'r12]
					[r13 'r13]
					[r14 'r14]
					[r15 'r15]
					[rax 'rax]
					[rbx 'rbx]
					[rcx 'rcx]
					[rdx 'rdx]
					[rbp 'rbp]
					[rsi 'rsi]
					[rdi 'rdi]
					[,other (error 'parse "invalid Register ~s" other)])))		
					(Prog x) x))
					
(define generate-x86-64
	(lambda (x)
		(define Prog
			(lambda (x)
				(match x
					[(begin ,stmt ,stmt* ...)
						(Statement stmt)
						(if (not (null? stmt*)) (Prog `(begin ,stmt* ...)))]
					[,other (error 'parse "invalid program ~s" other)])))
				;statement is a function that determines wether x is a valid statement as specified by the syntax rules
		(define Statement
				(lambda (x)
					(match x
						[(set! ,var1 ,in64) (guard (int64? in64)) (printf "movq $~s ,%~s ~%" in64 var1)]
						[(set! ,var1 ,var2) (guard (symbol? var2)) (printf "movq %~s ,%~s ~%" var2 var1)]
						[(set! ,var1 (,op ,var1 ,in32)) (guard (int32? in32)) (printf "~s $~s ,%~s ~%" (Binop op) in32 var1)]
						[(set! ,var1 (,op ,var1 ,var2)) (guard (symbol? var2)) (printf "~s %~s ,%~s ~%" (Binop op) var2 var1)]
						[,other (error 'parse "invalid statement ~s" other)])))
(define Binop
	(lambda (x)
		(match x
			[+ 'addq]
			[- 'subq]
			[* 'imulq]
			[,other (error 'parse "Invalid Operator ~s" other)])))
;Var Determines if x is a valid register 
(define Var 
	(lambda (x)
		(match x
			[r8 'r8]
			[r9 'r9]
			[r10 'r10]
			[r11 'r11]
			[r12 'r12]
			[r13 'r13]
			[r14 'r14]
			[r15 'r15]
			[rax 'rax]
			[rbx 'rbx]
			[rcx 'rcx]
			[rdx 'rdx]
			[rbp 'rbp]
			[rsi 'rsi]
			[rdi 'rdi]
			[,other (error 'parse "invalid Register ~s" other)])))
			(printf ".globl _scheme_entry ~%") ;Required for assembly code generated
			(printf "_scheme_entry: ~%")			;Required for assembly code generated
			(Prog x)
			(printf "ret ~%")))								;Required for assembly code generated

;Driver Function as give in the Assignment Description			
(define driver
 (lambda (program)
   (with-output-to-file "t.s"
     (lambda ()
       (generate-x86-64 (verify-scheme program))))))