#lang racket
(require (prefix-in constr: "helpers.rkt"))
(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time)

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 assign-fvars
 replace-locations
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 check-paren-x64
 generate-x64

 interp-values-lang

 interp-paren-x64)


(module+ test
	 (require
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/public/v3
   ;; NB: Workaround typo in shipped version of cpsc411-lib
   (except-in cpsc411/langs/v3 values-lang-v3)
   cpsc411/langs/v2)
)







;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
(define-values (check-values-lang
                interp-values-lang
                ; uniquify
                ; sequentialize-let
                ; normalize-bind
                ; select-instructions
                ; assign-homes
                ; uncover-locals
                ; assign-fvars
                ; replace-locations
                ; ; flatten-begins
                ; patch-instructions
                ; implement-fvars
                check-paren-x64
             ;   generate-x64
                )
  (values
   values
   ; values
   ; values
   ; values
   ; values
   ; values
   ; values
   ; values
   ; values
   values
   ; values
   ; values
   ; values
   values
  ; values
   ))



(define (module? any)
  (match any
	 [`(module ,_ ,program) #t]
	 [_ #f]))

(define (cut-metadata exp)
  (match exp
	 [`(module ,_ ,program)
	   #:when(module? exp)
	 program]
  	[wrong (string-append (error "Expected module got") (~a wrong))]))
	 


(define (cut-module mod)
  (match mod
	 [`(module ,program) program]))


(define (begin? any)
  (match any
	 [`(begin ,_ ...) #t]
	 [_ #f]))


(define (cut-begin exp)
    (match exp
	 [`(begin ,s ...) 
	   #:when (begin? exp) s]
	 [wrong (string-append (error "expected begin statement") (~a wrong))]))



;; TODO assuming fvars are correct add error messages

(define/contract (fvar->index fvar)
		 (-> symbol? number?)
  (string->number (string-trim (symbol->string fvar) "fv" #:right? #f)))

(define (fvar? any)
  (and 
    (symbol? any)
  (string-prefix? (symbol->string any) "fv")
  (natural? (fvar->index any))))


(define (triv? any)
  (or (int64? any)
      (aloc? any)))


(define (let? any)
  (match any
	 [`(let (,pair ...) ,value) #t]
	 [_ #f]))



; uniquify: gives each name in the values-lang-v3 program a unique name while keeping in mind the same variable can be re-introduced.
; Lexical scoping rules must be applied in case of a shadowing name


(define (uniquify let-exp)

 ; To implement lexical scoping rules for uniquify we keep a dictionay of mapped names that represents the environment.
  (define (lookup-name name env)
    (dict-ref env name))

 (define (assign-unique-names names)
   (foldl (lambda(name acc)
	    `(,@(dict-set* acc name (fresh name))))
	  '() names))
 
   (define (uniquify-iter let-exp env)
  (foldr (lambda (instr acc)
	   (match instr
		  [`(,name ,val) #:when (name? name) 
				 (let ((aloc (lookup-name name env)))
				   `((,aloc ,@(uniquify-iter (list val)  '())) . ,acc))]
		  [`(let (,pair ...) ,val ...) 
		    (let 
		      ((unique-names (assign-unique-names (map first pair))))
		      `((let ,(uniquify-iter pair unique-names) ,@(uniquify-iter val `(,@unique-names . ,env))) . ,acc))]
		  [name #:when (name? name) `(,(lookup-name name env) . ,acc)]
		  [`(,binop ,triv_1 ,triv_2) `((,binop ,@(uniquify-iter  `(,triv_1) env) ,@(uniquify-iter `(,triv_2) env)) . ,acc)]
		  [triv #:when (triv? triv)`(,triv . ,acc)]))
	 '()
	 let-exp))
`(module ,@(uniquify-iter `(,(cut-module let-exp)) '())))




(module+ test
	 (define-test-suite uniquify-test
			    (check-equal? (uniquify '(module 
						       (let  
							 ([x 5]
							  [y (let ([x 98]) (+ x 2))])
							 (+ y x))))
					  '(module (let
						     ([x.2 5]
						      [y.3 (let ([x.4 98]) (+ x.4 2))])
						     (+ y.3 x.2)))

)))







; sequentialize-let : converts programs from values-unique-lang-v3 to imp-mf-lang-v3 by converting lets to begins and set!s

(define (sequentialize-let p)
	(define (sequentialize-let p)
	  (foldr 
	    (lambda(instr acc)
	      (match instr
		     [`(,aloc ,triv) #:when(triv? triv) `((set! ,aloc ,triv) . ,acc)]
		     [`(,aloc ,val)  #:when (not (let? val)) `((set! ,aloc ,val) . ,acc)]
		     [`(,aloc ,val) #:when(let? val) `((set! ,aloc ,@(sequentialize-let (list val))) . ,acc)]
		     [`(let (,pair ...) ,val) #:when (let? instr) `((begin ,@(sequentialize-let pair) ,@(sequentialize-let  (list val))) . ,acc)]
		     [_ `(,instr . ,acc)]))

	    '()   p))
`(module ,@(sequentialize-let (list (cut-module p)))))

(module+ test
	 (define-test-suite sequentialize-let-test
			    (check-equal? (sequentialize-let'(module
					    (let ([x.1 2])
					      (let ([x.2 2])
						(+ x.1 x.2)))))
					 '(module 
					    (begin
					      (set! x.1 2)
					      (begin 
						(set! x.2 2)
						(+ x.1 x.2)))))))







; normalize-bind: convert (set! aloc (begin ... val)) to (begin ... (set! aloc val))


(define (normalize-bind p)
	(define (normalize-bind p) 
	  (foldr (lambda (instr acc)
		   (match instr
			  [`(set! ,aloc (begin ,effects ... ,s ...))
			    #:when (not (null? s))
			    `((begin  ,@(normalize-bind effects) ,@(normalize-bind `((set! ,aloc ,@s)))) . ,acc)]
			  [_ `(,instr . ,acc)])) '() p))
	 `(module ,(normalize-bind (cut-module p))))




(module+ test
	 (define-test-suite normalize-bind-test
			    (check-equal? 
			       (normalize-bind
				 `(module (begin
					    (set! x.1 (begin
							(set! x.6 8)
							(begin
							  (set! x.7 (begin 5))
							  (+ x.1 x.5))))
					    (set! x.5 (begin 5))
					    (set! x.1 (+ 5 5))
					    (+  x.1 x.1))))
				  '(module
					  (begin
					    (begin (set! x.6 8) (begin (begin (set! x.7 5)) (set! x.1 (+ x.1 x.5))))
					    (begin (set! x.5 5))
					    (set! x.1 (+ 5 5))
					    (+ x.1 x.1)))
					)))









; select instructions:  converts instructions from imp-cmf-lang-v3 to equivalent instructions in asm-lang-v2 
; IMPORTANT NOTE: The example solution to convert an instruction such as (set! x.1 (+ x.2 x.1)) to (set! x.1 (+ x.1 x.2))
; This solutions chooses to do it in two steps (set! tmp.1 x.2) (set! x.1 (+ tmp.1 x.1))
; They also perform an extra step with a binop tail (+ x.1 32) is converted to (set! x.1 (+ x.1 32)) (halt x.1) instead of introducing a temporary variable


(define (select-instructions p)

; Convert tail expressions
(define (select-tail tail)
  (match tail
	 [triv
	   #:when(triv? triv)
	     (if (int64? triv)
	       (let ((tmp (fresh)))
		 `((set! ,tmp ,triv) (halt ,tmp)))
	       `(halt ,triv))]
	 [`(,binop ,triv_1 ,triv_2)
	   (cond [(aloc? triv_1)
	       `((set! ,triv_1 (,binop ,triv_1 ,triv_2)) (halt ,triv_1))]
		 [(and (not (aloc? triv_1)) (aloc? triv_2))
		  `((set! ,triv_2 (,binop ,triv_2 ,triv_1)) (halt ,triv_2))]
		 [(and (int64? triv_1) (int64? triv_2))
		  (let 
		    ((tmp (fresh)))
		    `((set! ,tmp ,triv_1) (set! ,tmp (,binop ,tmp ,triv_2))(halt ,tmp)))])]))

; Convert an effect

(define (select-effect effect)
  (match effect
	 [`(set! ,aloc (,binop ,aloc ,triv_2)) effect]
	 [`(set! ,aloc (,binop ,triv_1 ,aloc)) (let ((tmp (fresh)))
						     `((set! ,tmp ,triv_1) (set! ,tmp (,binop ,tmp ,aloc))(set! ,aloc ,tmp)))]
	 [`(set! ,aloc (,binop ,triv_1 ,triv_2)) `((set! ,aloc ,triv_1) (set! ,aloc (,binop ,aloc ,triv_2)))]))

; Convert multiple effects


(define (select-effects effects)
  (foldr (lambda (effect acc)
	   (match effect
	 [`(set! ,aloc (,binop ,triv_1 ,triv_2))
	   #:when(not (eq? aloc triv_1))
	   `(,@(select-effect effect) . ,acc)]
	 [`(begin ,s ...) `(begin ,@(select-effects s))]
	 [_ `(,effect . ,acc)]))
	 '()
	 effects))



  (match p
	 [`(module (begin ,tail)) `(module () (begin ,@(select-tail tail)))]
	 [`(module (begin ,effect ... ,tail)) `(module () (begin ,@(select-effects effect) ,@(select-tail tail)))]))



 
(module+ test
	 (define-test-suite select-instructions-test
			    (check-equal? 
				 (select-instructions '(module  
							 (begin
							   (set! x.2 (+ 5 5))
							   (set! x.1 (+ x.2 x.1))
							   (+ x.1 32))))
				 '(module ()
					  (begin
					    (set! x.2 5)
					    (set! x.2 (+ x.2 5))
					    (set! tmp.1 x.2)
					    (set! tmp.1 (+ tmp.1 x.1))
					    (set! x.1 tmp.1)
					    (set! x.1 (+ x.1 32))
					    (halt x.1)))
)))












; uncover-locals: traverses the asm-lang-v2 program to find abstract locations
; The abstract locations are added to a set since one may appear multiple times
; The set is then converted to a list and added to the locals metadata field in a asm-lang-v2/locals program
; IMPORTANT NOTE: my representation slightly differs from the example 
;	1) assignment field called assignments
;	2) instead of a dictionary of fields  e.g ((x.1 fv1)) a dictionary of cons cells e.g ((x.1 . fv1))

(define (uncover-locals p)
	(define (uncover-locals-iter p)
	  (foldr (lambda (instr acc)
		   (match instr 
			  [`(begin ,s ...) (set-union (uncover-locals-iter s) acc)]

			  [`(set! ,aloc ,triv)
			    #:when(aloc? aloc)
			    (if (aloc? triv) 
			      (set-add (set-add acc triv) aloc)
			      (set-add acc aloc))]
			  [`(set! ,aloc_1 `(,_ ,aloc_1 ,triv))
			    #:when(aloc? aloc_1)
			    (if (aloc? triv)
			      (set-add (set-add acc triv) aloc_1)
			      (set-add acc aloc_1))]
			  [`(halt ,aloc)
			    #:when (aloc? aloc) (set-add acc aloc)]))
		 (set) p))
	 (define program (cut-metadata p))
	 (define locals (uncover-locals-iter  (list program)))
	 `(module ((locals ,(set->list locals))) ,program))

(module+ test
	 (define-test-suite uncover-locals-test
			    (check-equal? 
			      (uncover-locals
   '(module ()
     (begin
       (set! x.1 0)
       (halt x.1))))
			      '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1))))))



				


(define (get-locals p)
  (match p
	 [`(module ((locals ,alocs) ,_ ...) ,_) alocs]
	 [_ (error "No local field")]))

(define (get-assignments p)
  (match p
	 [`(module (,_ (assignments ,assignments)),_) assignments]
	 [_ (error "No assignments field")]))



; assign-fvars: create a metadata field which indicates which abstract location is to be replaced by which fvar.

(define (assign-fvars p)
  (define locals (get-locals p))
  (define assignments (foldr (lambda (aloc acc) `((,aloc . ,(make-fvar (length acc))) . ,acc)) '()  locals))
  `(module ((locals ,locals) (assignments ,assignments)),(cut-metadata p)))



(module+ test
	 (define-test-suite assign-fvars-test)
	 (check-equal? (assign-fvars
   '(module
      ((locals (x.1 y.1 w.1)))
      (begin
        (set! x.1 0)
        (set! y.1 x.1)
        (set! w.1 (+ w.1 y.1))
        (halt w.1))))
		       '(module

   ((locals (x.1 y.1 w.1)) (assignments ((x.1 . fv2) (y.1 . fv1) (w.1 . fv0))))

   (begin (set! x.1 0) (set! y.1 x.1) (set! w.1 (+ w.1 y.1)) (halt w.1)))))











;; replace-locations: replace the abstract locations in the program with their assigned fvar

(define (replace-locations p)
  (define assignments (get-assignments p))
  (define program (cut-metadata p))
  (define (replace-locations-iter p)
  (foldr (lambda (instr acc)
	 (match instr 
		[`(begin ,s ...) `((begin ,@(replace-locations-iter s)) . ,acc)]
		 [`(set! ,aloc_1 (,binop ,aloc_1 ,triv))
		  (let
		    ((assigned-loc (dict-ref assignments aloc_1)))
		    (if (aloc? triv)
		      `((set! ,assigned-loc (,binop ,assigned-loc ,(dict-ref assignments triv))) . ,acc)     ; If both arguments are fvars then both need to change
		      `((set! ,assigned-loc (,binop ,assigned-loc ,triv)) . ,acc)))]


		[`(set! ,aloc ,triv) 
		  (let 
		    ((assigned-loc (dict-ref assignments aloc)))
		  (if (aloc? triv)
		    `((set!  ,assigned-loc ,(dict-ref assignments triv)). ,acc)  ; Same concept here
		    `((set! ,assigned-loc ,triv) . ,acc)))]

		[`(halt ,aloc) `((halt ,(dict-ref assignments aloc)) . ,acc)])) '() p))
  (first (replace-locations-iter `(,program))))






(module+ test
	 (define-test-suite replace-locations-test
			    (check-equal?
			      (replace-locations
				'(module ((locals (x.1 y.1 w.1))
					  (assignments ((x.1 . rax) (y.1 . rbx) (w.1 . r9))))
					 (begin
					   (set! x.1 0)
					   (set! y.1 x.1)
					   (set! w.1 (+ w.1 y.1))
					   (halt w.1))))
			      '(begin
				 (set! rax 0) 
				 (set! rbx rax)
				 (set! r9 (+ r9 rbx))
				 (halt r9)))))




; assign-homes: assigns a physical location to each abstract location.

(define (assign-homes p)
  (replace-locations (assign-fvars (uncover-locals p))))


(module+ test
	 (define-test-suite assign-homes-test
			    (check-equal?
			       (assign-homes 
				   '(module ()
				     (begin
				       (set! x.1 0)
				       (set! y.1 x.1)
				       (set! y.1 (+ y.1 x.1))
				       (halt y.1))))
			       '(begin
				  (set! fv0 0)
				  (set! fv1 fv0)
				  (set! fv1 (+ fv1 fv0))
				  (halt fv1)))))



; flatten-begins:  takes a nested-asm-lang-v2 program and removes nested begin statements by pulling out the instructions in them and putting them in one big begin
; nested-asm-lang-v2 -> para-asm-lang-v2

(define (flatten-begins p)
(define (flatten-begins-foldr p)
   (foldr (lambda(instr acc) (match instr
				   [`(begin ,s ...) (append (flatten-begins-foldr instr) acc)]
				   [_ `(,instr . ,acc)])) '() (cut-begin p)))
(constr:make-begin (flatten-begins-foldr p)))



(module+ test
	 (define-test-suite flatten-begins-test
	 (check-equal?
	   (flatten-begins
	     '(begin
		(set! rax 5)
		(begin
		  (set! rbx 6)
		  (begin 
		    (set! rcx (+ rcx rbx))))
		(halt rcx)))
	   '(begin
	      (set! rax 5)
	      (set! rbx 6)
	      (set! rcx (+ rcx rbx))
	      (halt rcx)))))

















 ; patch-instructions:  Traverses the para-asm-lang-v2 program and converts instructions not supported in paren-x64-fvars-v2 to their equivalent  

(define (patch-instructions p)
  (constr:make-begin 
    (foldr (lambda (instr acc) (match instr
				    [`(halt ,triv) (cons `(set! rax ,triv) acc)]
				    [`(set! ,fv_1 ,fv_2)
				       #:when (and (fvar? fv_1) (fvar? fv_2))
				       `(((set! r10 ,fv_2) (set! ,fv_1 r10)) . ,acc)]
				    [_ `(,instr . ,acc)])) '() (cut-begin p))))

(module+ test
	 (define-test-suite patch-instructions-test
	   (check-equal? (patch-instructions
   '(begin
      (set! rbx 0)
      (set! rcx 0)
      (set! r9 42)
      (set! rbx rcx)
      (set! rbx (+ rbx r9))
      (halt rbx)))
 '(begin
   (set! rbx 0)
   (set! rcx 0)
   (set! r9 42)
   (set! rbx rcx)
   (set! rbx (+ rbx r9))
   (set! rax rbx)))))




;; implement-fvars: Replace fvars in program with a displacement mode operand.

(define (implement-fvars p)

  ; Converts an fvar to a displacement mode operand.
  (define (fvar->displacement-operand fvar)
    (let
      ((displacement-index (fvar->index fvar)))
      `(rbp - ,(* displacement-index 8))))

  ; Matches an instructions and replaces the fvars appearing in it.
  (define (match-and-replace-fvar s)
    (match s
	   [`(set! ,exp1 ,exp2)
	    (match* (exp1 exp2)
		    [(fvar other)
		     #:when (and (fvar? fvar) (or (register? other) (int32? other)))
		     `(set! ,(fvar->displacement-operand fvar) ,other)]
		    [(reg fvar)
		     #:when (and (register? reg )(fvar? fvar))
		     `(set! ,reg ,(fvar->displacement-operand fvar))]
		    [(reg `(,binop ,reg ,fvar))
		     #:when(fvar? fvar)
		     `(set! ,reg (,binop ,reg ,(fvar->displacement-operand fvar)))]
		     [(_ _) s])]))

  ; Map the match-and-replace function over all instructions

  (match p
	 [`(begin ,s ...) `(begin ,@(map match-and-replace-fvar s))]))


 (module+ test 
	  (define-test-suite implement-fvars-test
			    (check-equal? (implement-fvars '(begin (set! fv1 rax) (set! fv0 rbx) (set! rax (+ rax fv0))))
					   '(begin (set! (rbp - 8) rax) (set! (rbp - 0) rbx) (set! rax (+ rax (rbp - 0)))))))






; generate-x64: Extension from milestone 1. Can translate displacement mode operands in x64

(define (generate-x64 p)
  (define binop-hash (make-hash '((+ . "add") (* . "imul"))))
  ; Paren-x64-v1 -> x64-instruction-sequence

;; Added general function that converts addresses, integers and registers to string in x64

  (define (loc->x64 loc)
    (match loc 
	   [reg #:when (register? reg) (~a reg)]
	   [`(rbp - ,num) #:when (= (modulo num 8) 0) (string-append "QWORD [ " "rbp - " (number->string num) " ]")] 
	   [num #:when(number? num) (~a num)]))

  (define (statement->x64 s)
    (match s
	   [`(,exp1 ,exp2) (string-append "mov " (~a (loc->x64 exp1)) ", " (~a (loc->x64 exp2)) "\n")]))

(define (binop->ins b)
    (match b
	   [`(,binop ,reg ,triv) (string-append (dict-ref binop-hash binop) " "  (~a (loc->x64 reg)) ", " (~a (loc->x64 triv)) "\n")]))


  (define (program->x64 p)
    (match p
	   [`(begin ,s ...)
	     (foldr (lambda (instr acc)
	     (match instr
		    [`(set! ,reg (,binop ,reg ,triv)) (string-append acc (binop->ins`(,binop ,reg ,triv)))]
		    [`(set! ,reg ,triv) (string-append (statement->x64 `(,reg ,triv)) acc)]))
    ""
    s)]))
    (program->x64 p))
 

(module+ test
    (define-test-suite generate-x64-test 
		     (check-equal? 
		       (generate-x64 
		       '(begin
			  (set! (rbp - 0) 0)
			  (set! (rbp - 8) 42)
			  (set! rax (rbp - 0))
			  (set! rax (+ rax (rbp - 8)))))
				"mov QWORD [ rbp - 0 ], 0\nmov QWORD [ rbp - 8 ], 42\nmov rax, QWORD [ rbp - 0 ]\nadd rax, QWORD [ rbp - 8 ]\n")))

(define (interp-paren-x64 p)
  ; Environment (List-of (paren-x64-v2 Statements)) -> Integer
  (define (eval-instruction-sequence env sls)
    (if (empty? sls)
        (dict-ref env 'rax)
        (TODO "Implement the fold over a sequence of Paren-x64-v2 /s/.")))

  ; Environment Statement -> Environment
  (define (eval-statement env s)
    (TODO "Implement the transition function evaluating a Paren-x64-v2 /s/."))

  ; (Paren-x64-v2 binop) -> procedure?
  (define (eval-binop b)
    (TODO "Implement the interpreter for Paren-x64-v2 /binop/."))

  ; Environment (Paren-x64-v2 triv) -> Integer
  (define (eval-triv regfile t)
    (TODO "Implement the interpreter for Paren-x64-v2 /triv/."))

  (TODO "Implement the interpreter for Paren-x64-v2 /p/."))

; (define (generate-x64 p)
;   (define (program->x64 p)
;     (match p
;       [`(begin ,s ...)
;        (TODO "generate-x64")]))

;   (define (statement->x64 s)
;     (TODO "generate-x64"))

;   (define (loc->x64 loc)
;     (TODO "generate-x64"))

;   (define (binop->ins b)
;     (TODO "generate-x64"))

;   (program->x64 p))



(current-pass-list
 (list
  check-values-lang
  uniquify
  sequentialize-let
  normalize-bind
  select-instructions
  assign-homes
  flatten-begins
  patch-instructions
  implement-fvars
  generate-x64
  wrap-x64-run-time
  wrap-x64-boilerplate))






  (module+ test
    (run-tests
    (make-test-suite "Milestone 2 tests"
		     (list
		       generate-x64-test
		       implement-fvars-test
		       patch-instructions-test
		       flatten-begins-test
		       uncover-locals-test
		       assign-fvars-test
		       replace-locations-test
		       assign-homes-test
		       select-instructions-test
		       normalize-bind-test
		       sequentialize-let-test
		       uniquify-test




   ))))
