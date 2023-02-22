global start


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
dummy: db 0
