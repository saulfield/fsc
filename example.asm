	global _start
	section .data
x: db 3
	section .data
y: db 0
	section .text
main:
	; function prologue
	push rbp
	mov rbp, rsp

	; function body
	push 20      		; local variable declaration
	mov rax, [rbp + -8]	; local variable or arg
	push rax     		; save left operand
	mov rax, [rbp + 16]	; local variable or arg
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	add rax, rcx
	
	; function epilogue
	mov rsp, rbp
	pop rbp
	ret
_start:
	push 10
	call main
	mov rdi, rax 		; call exit with return code from main
	mov rax, 60  		; sys_exit
	syscall
