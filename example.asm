	section .text
main:
	; function prologue
	push rbp
	mov rbp, rsp

	; function body
	mov rax, [y]      	; get global variable
	push rax     		; save left operand
	mov rax, 2
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	add rax, rcx
	push rax      		; initialize new variable: z
	mov rax, [rbp + -8]	; get local variable or arg
	push rax     		; save left operand
	mov rax, 1
	mov rcx, rax 		; move right operand into rcx
	pop rax      		; restore left operand
	add rax, rcx
	mov [rbp + -8], rax	; assign local variable or arg
	mov rax, [rbp + -8]	; get local variable or arg
	
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
