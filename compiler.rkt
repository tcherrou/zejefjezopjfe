#lang racket

(require
 cpsc411/compiler-lib)



(provide
 compile-paren-x64
 check-paren-x64
 interp-paren-x64
 generate-x64
 wrap-x64-run-time
 wrap-x64-boilerplate)

(define-syntax-rule (TODO . stx)
  (error "Unfinished skeleton"))



;; int64? and int32? already implemented


(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v1
   cpsc411/test-suite/public/v1))




;; Helpers

(define (p64v1-register? reg)
  (and (member reg '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)) #t))



(define (p64v1-binop? bop)
  (and (member bop '(+ *)) #t))


 
; p64v1-sequence: checks if a set! is correctly formed 

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
	   (error (string-append "Register in instruction: "(~a exp1 exp2)  "require the same name!"))])]
  	[e (error (string-append "invalid instruction: " (~a e)))]))



; p64v1-process : checks if paren-x64-v1 program starts with (begin ...) and the statements are correctly formed


(define (p64v1-process p)
  (match p
	 [`(begin ,s ...)  `(begin ,@(map p64v1-sequence s))]))







; check-paren-x64-init:  Check if all the registers used are initialized especially rax


(define (check-paren-x64-init p)
  (define (initialized? reg inits)
    (and (member reg inits) #t))
      (define initialized-regs (match p
	   [`(begin ,s ...) 
	(let check-initialized
    	((initialized '())
     	 (s s))
	     (match s
		    ['() initialized]
		    [`((set! ,_ (,binop ,reg ,_)) ,rest ...)

		      (if (initialized? reg initialized)
			(check-initialized initialized rest)
			(error (string-append "The register has not been initialized: " (~a reg))))]

		    [`((set! ,reg ,_) ,rest ...)
		      (check-initialized `(,reg ,@initialized) rest)]
		    		    ))]))
      (if (initialized? 'rax initialized-regs)
	p
	(error (string-append "Register rax not initialized: Please initialize rax in: " (~a p)))))




(module+ test 
	 (define-test-suite 
	   check-paren-x64-init-test
	   (check-not-exn (lambda ()(check-paren-x64-init '(begin (set! rax 0)))))
	   (check-exn exn:fail? (lambda () (check-paren-x64-init '(begin (set! rax (+ rbx 4))))))))


; check-paren-x64-syntax:  check if p is a correctly formed paren-x64-v1 program

(define (check-paren-x64-syntax p)
  (p64v1-process p))




(module+ test 
	 (define-test-suite check-paren-x64-syntax-test
	   (check-not-exn (lambda () (check-paren-x64-syntax '(begin (set! rcx rax) (set! rbx (+ rbx 5))))))
	   (check-exn exn:fail? (lambda () (check-paren-x64-syntax '(begin (set! rbx '(* rax 5))))))))







; check-paren-x64: checks if a paren-x64-v1 is well formed and initialized all register that were used

(define (check-paren-x64 p)
  (check-paren-x64-init (check-paren-x64-syntax p)))


; interp-paren-x64: interprets a paren-x64-v1 program

(define (interp-paren-x64 p)
  (define regs-hash (make-hash '((rsp . 0)(rbp . 0)(rax . 0)(rbx . 0)(rcx . 0)(rdx . 0)(rsi . 0)(rdi . 0)(r8 . 0)(r9 . 0)(r10 . 0)(r11 . 0)(r12 . 0)(r13 . 0)(r14 . 0) (r15 . 0))))
  (define binop-hash (make-hash `((+ . ,x64-add) (* . ,x64-mul))))
  (define (get-triv-value triv)
    (if (p64v1-register? triv)
      (dict-ref regs-hash triv)
      triv))
  (define (eval-instruction-sequence regfile s)
        ; If no more instructions, return exit code modulo 256 (since operating
        ; systems return exit code modulo 256).	
        (match s
	       ['() (modulo (dict-ref regfile 'rax) 256)]


	       [`((set! ,reg ,triv) ,rest ...)
		#:when(or (p64v1-register? triv) (int64? triv))
			(hash-set! regfile reg (get-triv-value triv))
			(eval-instruction-sequence regfile rest)]
	       [`((set! ,reg (,binop ,reg ,triv)) ,rest ...)
		 (let*
		   ((reg-val (get-triv-value reg))
		    (triv-val (get-triv-value triv))
		    (bop (dict-ref binop-hash binop))
		    (new-value (bop reg-val triv-val)))
		   (hash-set! regfile reg new-value))
		 (eval-instruction-sequence regfile rest)]))
  (match p 
	 [`(begin ,s ...) (eval-instruction-sequence regs-hash s)]))



; generate-x64 generates the x64 instructions corresponding with the paren-x64-v1 program p

(define (generate-x64 p)
  (define binop-hash (make-hash '((+ . "add") (* . "imul"))))
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (let make-inst-seq
	 ((s s)
	  (inst-seq ""))
	 (match s
	      ['() inst-seq]
	      [`((set! ,reg (,binop ,reg ,triv)) ,rest ...) (make-inst-seq rest (string-append inst-seq (binop->ins `(,binop ,reg ,triv))))] 
	      [`((set! ,reg ,triv) ,rest ...) (make-inst-seq rest (string-append inst-seq (statement->x64 `(,reg ,triv))))]
	      ))]))


  (define (statement->x64 s)
    (match s
	   [`(,reg ,triv) (string-append "mov " (~a reg) "," (~a triv) "\n")]))


  (define (binop->ins b)
    (match b
	   [`(,binop ,reg ,triv) (string-append (dict-ref binop-hash binop) " "  (~a reg) "," (~a triv)) "\n"]))

  (program->x64 p))
















; wrap-x64-run-time : add x64 instructions that moves a value in rax and performs a system call with the exit code as value of the program


(define (wrap-x64-run-time str)
  (define run-time-string "\n exit: \n mov rdi, rax \n mov rax,60 \n syscall")
  (string-append str run-time-string))


; wrap-x64-boiler-plate: add needed boilerplate to x64 program

(define (wrap-x64-boilerplate str)
  (define boilerplate-string "global start \n section .text \n start: \n")
  (string-append  boilerplate-string str))


; compile-paren-x64 : combines all above passes into one procedure

(define (compile-paren-x64 p)
  (wrap-x64-boilerplate (wrap-x64-run-time (generate-x64 (check-paren-x64 p)))))

(current-pass-list
   (list
    check-paren-x64
    generate-x64
    wrap-x64-run-time
    wrap-x64-boilerplate))
 
(module+ test 
	 (define-test-suite correctness-test
	   (check-equal? 
	      (execute
   '(begin
      (set! rax 170679)
      (set! rdi rax)
      (set! rdi (+ rdi rdi))
      (set! rsp rdi)
      (set! rsp (* rsp rsp))
      (set! rbx 8991))
    nasm-run/exit-code)
	      (interp-paren-x64
   '(begin
      (set! rax 170679)
      (set! rdi rax)
      (set! rdi (+ rdi rdi))
      (set! rsp rdi)
      (set! rsp (* rsp rsp))
      (set! rbx 8991))
)
)))











  (module+ test
    (run-tests
    (make-test-suite "Tests"
      (list 
      check-paren-x64-init-test
      check-paren-x64-syntax-test
      correctness-test

      )
    )))
