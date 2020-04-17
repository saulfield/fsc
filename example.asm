	section .text
main:
	; function prologue
	push rbp
	mov rbp, rsp

	; function body
	mov rax, [y]      	; global variable
	push rax     		; save left operand
	mov rax, 2
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	add rax, rcx
	push rax      		; initialize new variable: x
	mov rax, [rbp + -8]	; local variable or arg
	
	; function epilogue
	mov rsp, rbp
	pop rbp
	ret
	
	; entry point
	global _start
_start:
	call main
	mov rdi, rax 		; call exit with return code from main
	mov rax, 60  		; sys_exit
	syscall
	
	; global variables
	section .data
x: db 3
y: db 5
