#lang racket

(require
 cpsc411/compiler-lib)

(provide
 check-paren-x64
 interp-paren-x64
 generate-x64
 wrap-x64-run-time
 wrap-x64-boilerplate)

(define-syntax-rule (TODO . stx)
  (error "Unfinished skeleton"))



;; int64? and int32? already implemented


(define (p64v1-register? reg)
  (and (member reg '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)) #t))



(define (p64v1-binop? bop)
  (and (member bop '(+ *)) #t))



(define (p64v1-sequence s)
  (match s 
	 [`(set! ,exp1 ,exp2) 
  (match* (exp1 exp2)
	 [ (reg int64)
	   #:when (and (p64v1-register? reg) (int64? int64)) 
	   `(set! ,reg ,int64)]
	 [(reg1 reg2)
	  #:when (and (p64v1-register? reg1)  (p64v1-register? reg2))
	  `(set! ,reg1 ,reg2)] 
	 [ (reg `(,binop ,reg ,int32)) 
	   #:when (and (p64v1-binop? binop) (p64v1-register? reg) (int32? int32)) 
	   `(set! ,reg (,binop ,reg ,int32))]
	 [ (reg1 `(,binop ,reg1 ,reg2))
	   #:when (and (p64v1-binop? binop) (p64v1-register? reg1) (p64v1-register? reg2))
	 `(set! ,reg1 (,binop ,reg1 ,reg2))]
 	 [ (reg1 `(,binop ,reg2 ,_))
	   (error "Register require the same name!")])]
  	[e (error (string-append "invalid instruction: " (~a e)))]))

(p64v1-sequence '(set! rsp 15))
(p64v1-sequence '(set! rsp rsp))
(p64v1-sequence '(set! rsp (+ rsp 26)))
; (p64v1-sequence '(set! rsp (* rax 2)))






(define (p64v1-process p)
  (match p
	 [`(begin ,s ...)  `(begin ,@(map p64v1-sequence s))]))









;; Optional; if you choose not to complete, implement a stub that returns the input



;; 



(define (check-paren-x64-init p)
  (define (initialized? reg inits)
    (and (member reg inits) #t))
      (match p
	   [`(begin ,s ...) 
	(let check-initialized
    	((initialized '())
     	 (s s))
	     (match s
		    ['() p]
		    [`((set! ,_ (,binop ,reg ,_)) ,rest ...)

		      (if (initialized? reg initialized)
			(check-initialized initialized rest)
			(error (string-append "The register has not been initialized: " (~a reg))))]

		    [`((set! ,reg ,_) ,rest ...)
		      (check-initialized `(,reg ,@initialized) rest)]
		    		    ))]))


(check-paren-x64-init
   '(begin
      (set! rax 170679)
      (set! rdi rax)
      (set! rdi (+ rdi rdi))
      (set! rsp rdi)
      (set! rsp (* rsp rsp))
      (set! rbx 8991)))


;; Optional; if you choose not to complete, implement a stub that returns the input
(define (check-paren-x64-syntax p)
  (p64v1-process p))

(check-paren-x64-syntax '(begin 
		  (set! rsp 15)
		  (set! rax 16)))

(define (check-paren-x64 p)
  (check-paren-x64-init (check-paren-x64-syntax p)))

;; Optional; if you choose not to complete, implement a stub that returns a valid exit code
(define (interp-paren-x64 p)
  (define (eval-instruction-sequence regfile s)
    (if (empty? s)
        ; If no more instructions, return exit code modulo 256 (since operating
        ; systems return exit code modulo 256).
        (modulo (dict-ref regfile 'rax) 256)
        (TODO ...)))
  (TODO ...))

(define (generate-x64 p)
  ; Paren-x64-v1 -> x64-instruction-sequence
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (TODO ...)]))

  (define (statement->x64 s)
    (TODO ... ))

  (define (binop->ins b)
    (TODO ... ))

  (program->x64 p))

(define (wrap-x64-run-time str)
  (TODO ...))

(define (wrap-x64-boilerplate str)
  (TODO ...))

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v1
   cpsc411/test-suite/public/v1)

  (run-tests
   (v1-public-test-suite
    (list
     check-paren-x64
     generate-x64
     wrap-x64-run-time
     wrap-x64-boilerplate)
    (list
     interp-paren-x64-v1
     interp-paren-x64-v1
     #f
     #f)
    check-paren-x64
    interp-paren-x64)))
