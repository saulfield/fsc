	global _start
	section .text
main:
	; function prologue
	push rbp
	mov rbp, rsp

	; function body
	mov rax, [x]   	; global variable
	push rax     		; save left operand
	mov rax, [y]   	; global variable
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	add rax, rcx
	
	; function epilogue
	mov rsp, rbp
	pop rbp
	ret
_start:
	call main
	mov rdi, rax 		; call exit with return code from main
	mov rax, 60  		; sys_exit
	syscall
	
	; global variables
	section .data
x: db 3
y: db 0
