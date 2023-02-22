#lang racket
(require rackunit)
(provide FACT_S compile execute)

; String -> void
; Raises an error with str as the error message.
(define (TODO str) (error str))

; String -> Integer
; or
; String -> Procedure? -> Integer
;
; Runs the string as a shell command, and either returns the exit code or fails
; with an error message including command and the non-successful exit code.
; Options takes a predicate as the second argument, which takes an exit code
; and returns true if the exit code indicates success.
(define (system/exit-code! str [success? zero?])
  (let ([code (system/exit-code str)])
    (unless (success? code)
      (error (format "Command '~a' failed with exit code '~a'" str code)))
    code))

; An x64 Program
;; Copy the x64 program implementing fact.s into the here string, between the #<<EOS EOF.
(define FACT_S 
  "global start


section .text

start:
	mov r8,5

fact_start:
	mov r9,1

fact_loop:
	cmp r8,0
	je fact_done
	imul r9, r8
	dec r8
	jmp fact_loop
fact_done:
 	mov rax, 60
 	mov rdi, r9
	syscall


section .data 
dummy: db 0"
)

; x64 Program -> File name
; Takes an x64 Program, compiles it to an executable, and returns the name of
; the generated executable.
(define (compile str)
  (define p (path->string (make-temporary-file "~a.s")))

  (define o (string-replace p ".s" ".o"))

  (define exe (string-replace p ".s" ".exe"))

  (with-output-to-file p (thunk (display str)) #:exists 'replace)
  (system/exit-code! (format "nasm -f elf64 ~a -o ~a" p o))
  (system/exit-code! (format "ld -e start -o ~a ~a" exe o))
  exe)

; x64 Program -> Natural
; Takes an x64 Program, compiles it to an executable, and runs it, returning
; the error code.
(define (execute str)
  (system/exit-code! (compile str) number?))

(module+ test
  (check-regexp-match
    #rx"2\\.(13|14|15)"
    (with-output-to-string (thunk (system "nasm --version"))))

  (check-regexp-match
    #rx"\\.exe"
    (compile FACT_S))

  (check-equal? (execute FACT_S) 10))
