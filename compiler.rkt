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
                generate-x64
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
   values
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
	 



(define (begin? any)
  (match any
	 [`(begin ,_ ...) #t]
	 [_ #f]))


(define (cut-begin exp)
    (match exp
	 [`(begin ,s ...) 
	   #:when (begin? exp) s]
	 [wrong (string-append (error "expected begin statement") (~a wrong))]))

(define stub-program '(begin
      (set! fv0 0)
      (set! fv1 42)
      (set! fv0 fv1)
      (halt fv0)))

;; TODO assuming fvars are correct add error messages

(define/contract (fvar->index fvar)
		 (-> symbol? number?)
  (string->number (string-trim (symbol->string fvar) "fv" #:right? #f)))


;; TODO maybe implement contract for this
(define (fvar? any)
  (and 
    (symbol? any)
  (string-prefix? (symbol->string any) "fv")
  (natural? (fvar->index any))))


(define (implement-fvars p)

  (define (fvar->displacement-operand fvar)
    (let
      ((displacement-index (fvar->index fvar)))
      `(rbp - ,(* displacement-index 8))))



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
		     `(set! reg (,binop ,reg ,(fvar->displacement-operand fvar)))]
		     [(_ _) s])]))
  (match p
	 [`(begin ,s ...) `(begin ,@(map match-and-replace-fvar s))]))



; (define (patch-instructions p)
;  	(match p
; 	 [`(begin ,s ...)
; 	   `(begin ,@(map (lambda (s) (match s
; 		  [`(halt ,triv)
; 		    `(set! rax ,triv)]
; 		  [`(set ,fv_1 ,fv_2)
; 		    #:when (and (fvar? fv_1) (fvar? fv_2))
; 		    `()]
; 		  [_ s])) s))]))



(define (patch-instructions p)
  (constr:make-begin 
    (foldr (lambda (instr acc) (match instr
				    [`(halt ,triv) (cons `(set! rax ,triv) acc)]
				    [`(set! ,fv_1 ,fv_2)
				       #:when (and (fvar? fv_1) (fvar? fv_2))
				       (append `((set! r10 ,fv_2) (set! ,fv_1 r10)) acc)]
				    [_ (cons instr acc)])) '() (cut-begin p))))


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
				


(define (get-locals p)
  (match p
	 [`(module ((locals ,alocs) ,_ ...) ,_) alocs]
	 [_ (error "No local field")]))

(define (get-assignments p)
  (match p
	 [`(module (,_ (assignments ,assignments)),_) assignments]
	 [_ (error "No module field")]))

(define (assign-fvars p)
  (define locals (get-locals p))
  (define assignments (foldr (lambda (aloc acc) `((,aloc . ,(make-fvar (length acc))) . ,acc)) '()  locals))
  `(module ((locals ,locals) (assignments ,assignments)),(cut-metadata p)))

(define (replace-locations p)
  (define assignments (get-assignments p))
  (define program (cut-metadata p))
  (define (replace-locations-iter p)
  (foldr (lambda (instr acc)
	 (match instr 
		[`(begin ,s ...) `((begin ,@(replace-locations-iter s)) . ,acc)]
		[`(set! ,aloc ,triv) 
		  (let 
		    ((assigned-loc (dict-ref assignments aloc)))
		  (if (aloc? triv)
		    `((set!  ,assigned-loc ,(dict-ref assignments triv)). ,acc)
		    `((set! ,assigned-loc ,triv) . ,acc)))]

		[`(set! ,aloc_1 (,binop aloc_1 ,triv))
		  (let
		    ((assigned-loc (dict-ref assignments aloc_1)))
		    (if (aloc? triv)
		      `((set! ,assigned-loc (,binop ,assigned-loc ,(dict-ref assignments triv))) . ,acc)
		      `((set! ,assigned-loc (,binop ,assigned-loc ,triv)) . ,acc)))]
		[`(halt ,aloc) `((halt ,(dict-ref assignments aloc)) . ,acc)])) '() p))
  (first (replace-locations-iter `(,program))))



(define (assign-homes p)
  (replace-locations (assign-fvars (uncover-locals p))))

  

(define (flatten-begins p)
(define (flatten-begins-foldr p)
   (foldr (lambda(instr acc) (match instr
				   [`(begin ,s ...) (append (flatten-begins-foldr instr) acc)]
				   [_ `(,instr . ,acc)])) '() (cut-begin p)))
(constr:make-begin (flatten-begins-foldr p)))





(define (triv? any)
  (or (int64? any)
      (aloc? any)))
 

;; TODO: Fill in.
;; You might want to reuse check-paren-x64 and generate-x64 from milestone-1


; (+ 2 2) -> temp.1 = 2, temp.1 = temp.1 + 2, halt temp.1
; x.1 = 2 + 2, x.1 = 2, x.1 = x.1 + 2
; x.1 = y.2 + z.3, x.1 = y.2, x.1 = x.1 + z.3



; (+ 2 2) -> `(set! tmp.1 2) (set! tmp.1 (+ tmp.1 2)) (halt tmp.1)
; (+ x.1 2) -> (set! x.1 (+ x.1 2)) (halt x.1)
; x.1 -> (halt x.1)
; 5 -> (set! tmp.1 5) (halt tmp.1)
; (set! aloc (+ 5 5)) -> (set! aloc 5) (set! aloc (+ aloc 5))






; (define (select-instructions p)

;   ; (Imp-cmf-lang-v3 value) -> (List-of (Asm-lang-v2 effect)) and (Asm-lang-v2 aloc)
;   ; Assigns the value v to a fresh temporary, returning two values: the list of
;   ; statements the implement the assignment in Loc-lang, and the aloc that the
;   ; value is stored in.
;   (define (assign-tail-tmp v)
;     (match v 
; 	   [triv 
; 	     #:when(triv? triv)
; 	     (if (int64? triv)
; 	       (let ((tmp (fresh)))
; 		 `((set! ,tmp ,triv) (halt ,tmp)))
; 	       `(halt ,triv))]))

; 	   ; [`(,binop ,triv_1 ,triv_2)
; 	   ;   #:when (and (triv? triv_1) (triv? triv_2))
; 	   ;   (let 
; 	   ;     ((tmp (fresh)))
; 	   ;   `((set! ,tmp ,triv_1) (set! ,tmp (,binop ,tmp ,triv_2) (halt ,tmp)))]))



;   (define (select-tail e)
;     (TODO "Implement select-tail"))

;   (define (select-value e)
;     (TODO "Implement select-value"))






;   (define (select-effect e)
;     (match e
; 	   [`(begin ,s ...) `(begin ,(select-effect s))]
; 	   [`(set! ,aloc (,binop ,triv_1 ,triv_2))
; 	     #:when(and (int64? triv_1) (int64? triv_2))
; 	     `((set! ,aloc ,triv_1) (set! ,aloc (,binop ,aloc ,triv_2)))] 
; ))












  ; (match p
  ;   [`(module ,tail)
  ;    `(module () ,(select-tail tail))]
  ;   [`(module () ,effect ...) 
  ;     `(module () ,(select-effect effect))]))


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




(define (cut-module mod)
  (match mod
	 [`(module ,program) program]))






; (select-effect `(begin (set! x.1 (+ x.2 x.1))))


(define (select-effect effect)
  (match effect
	 [`(set! ,aloc (,binop ,aloc ,triv_2)) effect]
	 [`(set! ,aloc (,binop ,triv_1 ,aloc)) (let ((tmp (fresh)))
						     `((set! ,tmp triv_1) (set! ,tmp (,binop ,tmp ,aloc))(set! ,aloc ,tmp)))]
	 [`(set! ,aloc (,binop ,triv_1 ,triv_2)) `((set! ,aloc ,triv_1) (set! ,aloc (,binop ,aloc ,triv_2)))]))




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

; (select-effects '(begin (set! x.1 (+ x.1 x.2))))
; (select-effects '(begin (set! x.1 (+ x.2 x.1))))
; (select-effects '(begin (set! x.1 (+ 5 5))))

(define (select-instructions p)
  (match p
	 [`(module (begin ,tail)) `(module () (begin ,@(select-tail tail)))]
	 [`(module (begin ,effect ... ,tail)) `(module () (begin ,(select-effects effect) ,@(select-tail tail)))]))



(define (normalize-bind p)
	(define (normalize-bind p) 
	  (foldr (lambda (instr acc)
		   (match instr
			  [`(set! ,aloc (begin ,effects ... ,s ...)) #:when (not (null? s)) `((begin  ,@(normalize-bind effects) ,@(normalize-bind `((set! ,aloc ,@s)))) . ,acc)]
			  [_ `(,instr . ,acc)])) '() p))
	 `(module ,(normalize-bind p)))



(define (let? any)
  (match any
	 [`(let (,pair ...) ,value) #t]
	 [_ #f]))

(define (sequentialize-let p)
	(define (sequentialize-let p)
	  (foldr 
	    (lambda(instr acc)
	      (displayln instr)
	      (match instr
		     [`(,aloc ,triv) #:when(triv? triv) `((set! ,aloc ,triv) . ,acc)]
		     [`(,aloc ,val)  #:when (not (let? val)) `((set! ,aloc ,val) . ,acc)]
		     [`(,aloc ,val) #:when(let? val) `((set! ,aloc ,@(sequentialize-let (list val))) . ,acc)]
		     [`(let (,pair ...) ,val) #:when (let? instr) `((begin ,@(sequentialize-let pair) ,@(sequentialize-let  (list val))) . ,acc)]
		     [_ `(,instr . ,acc)]))

	    '()   p))
`(module ,@(sequentialize-let (list (cut-module p)))))





; (sequentialize-let `(module
; 		      (let
; 		      ([x.2 (let ([x.9 (+ 9 7)]) (+ 5 9))]
; 		       [x.6 98]) (let ([x.6 5]) 4))))
















; (normalize-bind `(begin
; 		     (set! x.1 (begin
; 				   (set! x.6 8)
; 				   (begin
; 				     (set! x.7 (begin 5))
; 				     (+ x.1 x.5))))
; 		     (set! x.5 (begin 5))
; 		     (set! x.1 (+ 5 5))
; 		     (+  x.1 x.1)))

; (normalize-bind 
; 		     `(list (set! x.1 (begin (set! x.5 (begin 5)) (begin (begin (begin 5)))))))






; (select-instructions `(module (begin 5)))
;  (set! x.1 (+ x.1 x.2))
;  (set! x.1 (+ x.2 x.1)) -> (set! tmp.1 x.2) (set! tmp.1 (+ tmp.1 x.1)) (set! x.1 tmp.1)
; `(set! x.1 (+ 5 5)) -> (set! x.1 5) (set! (+ x.1 5))


;select-instructions get program p in imp-cmf-lang-v3
; (define (select-instructions p)
;   (foldr (lambda (instr acc)
; 	   (match instr
; 	     [`(begin ,s ... ,tail) (append `(,(select-instructions s) ,(select-tail tail)) acc)]
; 	     [`(set! ,aloc `(,binop ,triv_1 ,triv_2))
; 	       `((set! ,aloc ,triv_1) (set! ,aloc (,binop ,aloc ,triv_2)))]
; 	     [_ instr])) '() (cut-module p)))




; (select-tail 5)
; (select-tail 'x.1)
; (select-tail '(+ 5 5))
; (select-tail '(+ x.1 x.2))
; (select-tail '(* x.1 5))
; (select-tail '(+ 5 x.1))








(define (uniquify let-exp)


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
  (require
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/public/v3
   ;; NB: Workaround typo in shipped version of cpsc411-lib
   (except-in cpsc411/langs/v3 values-lang-v3)
   cpsc411/langs/v2)

  (run-tests
   (v3-public-test-sutie
    (current-pass-list)
    (list
     interp-values-lang-v3
     interp-values-lang-v3
     interp-values-unique-lang-v3
     interp-imp-mf-lang-v3
     interp-imp-cmf-lang-v3
     interp-asm-lang-v2
     interp-nested-asm-lang-v2
     interp-para-asm-lang-v2
     interp-paren-x64-fvars-v2
     interp-paren-x64-v2
     #f #f))))
